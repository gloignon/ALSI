library(data.table)

# Dependency-triple surprisal models.
#
# Both models score the same unit — one dependency arc encoded as the triple
# (head UPOS, dependency relation, dependent UPOS) — but differ in what they
# treat as predictable:
#
#   deprel_surprisal()  p(dep_pos | head_pos, dep_rel)
#                       "what category fills this grammatical slot?"
#   attach_surprisal()  p(dep_rel | head_pos, dep_pos)
#                       "which relation links these two categories?"
#
# They are built from the same counts (build_deprel_ngrams()) and share the
# backoff-table and scoring machinery below.

# Attach each token's head UPOS within (doc_id, sentence_id) and drop arcs that
# cannot be scored: root tokens (no incoming arc) and any arc whose head or
# dependent carries an excluded UPOS. Returns a data.table with head_pos and
# dep_pos columns added.
.attach_head_pos <- function(dt_corpus, exclude_pos) {
  dt <- setDT(copy(dt_corpus))[, .(doc_id, sentence_id, token_id,
                                   head_token_id, upos, dep_rel)]

  heads <- dt[, .(doc_id, sentence_id, head_token_id = token_id,
                  head_pos = upos)]

  dt <- dt[head_token_id != "0"]
  dt[heads, head_pos := i.head_pos, on = .(doc_id, sentence_id,
                                           head_token_id)]
  dt[, dep_pos := upos]

  if (length(exclude_pos) > 0L) {
    dt <- dt[!head_pos %chin% exclude_pos & !dep_pos %chin% exclude_pos]
  }
  return(dt[!is.na(head_pos) & !is.na(dep_pos)])
}

# Build trigram/bigram/unigram backoff tables from triple counts, conditioning
# `target_col` on the two `ctx_cols`. The first context column is dropped at
# the bigram level (the gentler backoff keeps the more informative second
# column). Used by both prepare_deprel_model() and prepare_attach_model().
.prepare_backoff_model <- function(counts, ctx_cols, target_col) {
  c1 <- ctx_cols[1]
  c2 <- ctx_cols[2]
  dt <- copy(counts)

  dt[, context_N := sum(N), by = c(c1, c2)]
  dt[, prob := N / context_N]
  dt[, context_entropy := -sum(prob * log2(prob)), by = c(c1, c2)]
  tri <- dt[, c(c1, c2, target_col, "prob", "context_entropy"), with = FALSE]
  setkeyv(tri, c(c1, c2, target_col))

  # Bigram — marginalise over c1, keep c2 as the context
  bi <- dt[, .(N = sum(N)), by = c(c2, target_col)]
  bi[, context_N := sum(N), by = c2]
  bi[, prob := N / context_N]
  bi[, context_entropy := -sum(prob * log2(prob)), by = c2]
  bi <- bi[, c(c2, target_col, "prob", "context_entropy"), with = FALSE]
  setkeyv(bi, c(c2, target_col))

  # Unigram — marginal target frequency
  uni <- dt[, .(N = sum(N)), by = target_col]
  uni[, prob := N / sum(N)]
  uni <- uni[, c(target_col, "prob"), with = FALSE]
  setkeyv(uni, target_col)

  return(list(trigram = tri, bigram = bi, unigram = uni))
}

# Score a parsed corpus against a backoff model. `ctx_cols`/`target_col` must
# match those used in .prepare_backoff_model(); `prefix` names the output
# columns (e.g. "deprel" -> deprel_surprisal). Returns the token/sentence/doc
# list documented on the public wrappers.
.score_dependency <- function(dt_corpus, model, ctx_cols, target_col, prefix,
                              exclude_pos, backoff_scale) {
  dt <- .attach_head_pos(dt_corpus, exclude_pos)

  join_cols <- c(ctx_cols, target_col)
  dt <- model$trigram[dt, on = join_cols]

  if (!is.null(backoff_scale)) {
    bi_keys <- c(ctx_cols[2], target_col)
    missing_tri <- is.na(dt$prob)
    if (any(missing_tri)) {
      bi_lookup <- model$bigram[dt[missing_tri], on = bi_keys,
                                .(prob_bi = prob, ce_bi = context_entropy)]
      dt[missing_tri, prob            := backoff_scale * bi_lookup$prob_bi]
      dt[missing_tri, context_entropy := bi_lookup$ce_bi]

      missing_bi <- missing_tri & is.na(dt$prob)
      if (any(missing_bi)) {
        uni_lookup <- model$unigram[dt[missing_bi], on = target_col,
                                    .(prob_uni = prob)]
        dt[missing_bi, prob            := (backoff_scale^2) *
                                          uni_lookup$prob_uni]
        dt[missing_bi, context_entropy := NA_real_]
      }
    }
  }

  s_col <- paste0(prefix, "_surprisal")
  e_col <- paste0(prefix, "_entropy")
  r_col <- paste0(prefix, "_entropy_reduction")

  dt[, (s_col) := -log2(prob)]
  # Reduction in reading order within the sentence; arcs have no inherent
  # linear order, so this telescopes over scored tokens as they appear.
  dt[, (r_col) := shift(context_entropy, 1L) - context_entropy,
     by = .(doc_id, sentence_id)]
  setnames(dt, "context_entropy", e_col)

  token_dt <- dt[, c("doc_id", "sentence_id", "token_id", "upos", "dep_rel",
                     s_col, e_col, r_col), with = FALSE]

  measures   <- c(s_col, e_col, r_col)
  stat_names <- as.vector(rbind(paste0("mean_", measures),
                                paste0("sd_", measures)))
  agg_one <- function(sd_block) {
    vals <- lapply(measures, function(m) {
      c(mean(sd_block[[m]], na.rm = TRUE), sd(sd_block[[m]], na.rm = TRUE))
    })
    setNames(as.list(unlist(vals)), stat_names)
  }

  sent_dt <- token_dt[, agg_one(.SD), by = .(doc_id, sentence_id)]
  doc_dt  <- token_dt[, agg_one(.SD), by = doc_id]

  return(list(
    token_surprisal = token_dt,
    sent_surprisal  = sent_dt,
    doc_surprisal   = doc_dt
  ))
}


#' Prepare a dependency-triple probability table (predicting dependent POS)
#'
#' Converts the output of \code{build_deprel_ngrams()} into keyed lookup
#' tables for \eqn{p(\text{dep\_pos} \mid \text{head\_pos}, \text{dep\_rel})}.
#' The model scores how predictable a dependent's POS is given its head's POS
#' and the relation linking them.
#'
#' @param deprel_ngrams data.table with columns \code{head_pos},
#'   \code{dep_rel}, \code{dep_pos}, \code{N} — as returned by
#'   \code{build_deprel_ngrams()}.
#' @returns A list of three keyed data.tables: \code{trigram}
#'   (\code{head_pos}, \code{dep_rel}, \code{dep_pos}, \code{prob},
#'   \code{context_entropy}), \code{bigram} (marginalised over \code{head_pos}),
#'   and \code{unigram} (marginal \code{dep_pos} frequency).
prepare_deprel_model <- function(deprel_ngrams) {
  return(.prepare_backoff_model(deprel_ngrams,
                                ctx_cols = c("head_pos", "dep_rel"),
                                target_col = "dep_pos"))
}

#' Prepare a dependency-triple probability table (predicting the relation)
#'
#' Converts the output of \code{build_deprel_ngrams()} into keyed lookup
#' tables for \eqn{p(\text{dep\_rel} \mid \text{head\_pos}, \text{dep\_pos})}.
#' The model scores how predictable the relation linking a head and a
#' dependent is, given only their two POS categories — i.e. how ambiguous the
#' attachment label is. A determiner under a noun is almost always \code{det};
#' a noun under a verb may be \code{nsubj}, \code{obj}, \code{obl}, ...
#'
#' @param deprel_ngrams data.table with columns \code{head_pos},
#'   \code{dep_rel}, \code{dep_pos}, \code{N} — as returned by
#'   \code{build_deprel_ngrams()} (the same counts used by
#'   \code{prepare_deprel_model()}).
#' @returns A list of three keyed data.tables: \code{trigram}
#'   (\code{head_pos}, \code{dep_pos}, \code{dep_rel}, \code{prob},
#'   \code{context_entropy}), \code{bigram} (marginalised over \code{head_pos}),
#'   and \code{unigram} (marginal \code{dep_rel} frequency).
prepare_attach_model <- function(deprel_ngrams) {
  return(.prepare_backoff_model(deprel_ngrams,
                                ctx_cols = c("head_pos", "dep_pos"),
                                target_col = "dep_rel"))
}


#' Score dependency-triple surprisal and entropy (dependent-POS conditioning)
#'
#' Given a parsed corpus (one row per token) and a model from
#' \code{prepare_deprel_model()}, computes token-, sentence-, and
#' document-level surprisal, entropy, and entropy reduction over dependency
#' arcs. Each non-root token contributes one arc: its UPOS is the target, its
#' head's UPOS together with \code{dep_rel} form the context.
#'
#' Unseen triples receive \code{NA} surprisal unless \code{backoff_scale} is
#' set, in which case Stupid Backoff falls back to the \code{dep_rel}-only
#' bigram and then the marginal unigram.
#'
#' @param dt_corpus data.table with columns \code{doc_id}, \code{sentence_id},
#'   \code{token_id}, \code{head_token_id}, \code{upos}, \code{dep_rel}.
#' @param deprel_model List from \code{prepare_deprel_model()}.
#' @param exclude_pos Character vector of UPOS tags to exclude. An arc is
#'   dropped when its head or dependent carries one of these tags.
#' @param backoff_scale Numeric Stupid Backoff discount (Brants et al. 2007),
#'   or \code{NULL} to disable (unseen triples receive \code{NA}).
#' @returns A named list with \code{token_surprisal}, \code{sent_surprisal},
#'   and \code{doc_surprisal} data.tables. Token columns: \code{doc_id},
#'   \code{sentence_id}, \code{token_id}, \code{upos}, \code{dep_rel},
#'   \code{deprel_surprisal}, \code{deprel_entropy},
#'   \code{deprel_entropy_reduction}. Sentence/document tables carry the
#'   mean and SD of each.
#' @references
#'   Sidorov, G., Velasquez, F., Stamatatos, E., Gelbukh, A., &
#'   Chanona-Hernández, L. (2014). Syntactic n-grams as machine learning
#'   features for natural language processing. *Expert Systems with
#'   Applications*, 41(3), 853--860. \doi{10.1016/j.eswa.2013.08.015}
#'
#'   Brants, T., Popat, A. C., Xu, P., Och, F. J., & Dean, J. (2007). Large
#'   language models in machine translation. *Proceedings of EMNLP-CoNLL*,
#'   858--867.
deprel_surprisal <- function(dt_corpus, deprel_model,
                             exclude_pos = character(0),
                             backoff_scale = NULL) {
  return(.score_dependency(dt_corpus, deprel_model,
                           ctx_cols = c("head_pos", "dep_rel"),
                           target_col = "dep_pos", prefix = "deprel",
                           exclude_pos = exclude_pos,
                           backoff_scale = backoff_scale))
}


#' Score dependency-attachment surprisal and entropy (relation conditioning)
#'
#' Given a parsed corpus and a model from \code{prepare_attach_model()},
#' computes token-, sentence-, and document-level surprisal, entropy, and
#' entropy reduction for the \emph{relation} on each dependency arc. Each
#' non-root token contributes one arc: its \code{dep_rel} is the target, its
#' head's UPOS together with its own UPOS form the context. High surprisal =
#' the relation linking these two categories is unexpected; high entropy = the
#' two categories admit many possible relations (an ambiguous attachment).
#'
#' @param dt_corpus data.table with columns \code{doc_id}, \code{sentence_id},
#'   \code{token_id}, \code{head_token_id}, \code{upos}, \code{dep_rel}.
#' @param attach_model List from \code{prepare_attach_model()}.
#' @param exclude_pos Character vector of UPOS tags to exclude. An arc is
#'   dropped when its head or dependent carries one of these tags.
#' @param backoff_scale Numeric Stupid Backoff discount (Brants et al. 2007),
#'   or \code{NULL} to disable (unseen triples receive \code{NA}).
#' @returns A named list with \code{token_surprisal}, \code{sent_surprisal},
#'   and \code{doc_surprisal} data.tables. Token columns: \code{doc_id},
#'   \code{sentence_id}, \code{token_id}, \code{upos}, \code{dep_rel},
#'   \code{attach_surprisal}, \code{attach_entropy},
#'   \code{attach_entropy_reduction}. Sentence/document tables carry the mean
#'   and SD of each.
#' @references
#'   Sidorov, G., Velasquez, F., Stamatatos, E., Gelbukh, A., &
#'   Chanona-Hernández, L. (2014). Syntactic n-grams as machine learning
#'   features for natural language processing. *Expert Systems with
#'   Applications*, 41(3), 853--860. \doi{10.1016/j.eswa.2013.08.015}
attach_surprisal <- function(dt_corpus, attach_model,
                             exclude_pos = character(0),
                             backoff_scale = NULL) {
  return(.score_dependency(dt_corpus, attach_model,
                           ctx_cols = c("head_pos", "dep_pos"),
                           target_col = "dep_rel", prefix = "attach",
                           exclude_pos = exclude_pos,
                           backoff_scale = backoff_scale))
}
