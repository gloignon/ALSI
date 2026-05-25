library(data.table)

#' Prepare a POS trigram probability table from raw counts
#'
#' Converts the output of \code{build_pos_ngrams()} into a keyed lookup table
#' of conditional probabilities \code{p(pos3 | pos1, pos2)} and pre-computed
#' context entropy.
#'
#' @param pos_ngrams data.table with columns \code{pos1}, \code{pos2},
#'   \code{pos3}, \code{N} — as returned by \code{build_pos_ngrams()}.
#' @returns A list with three keyed data.tables:
#'   \describe{
#'     \item{\code{trigram}}{Columns \code{pos1}, \code{pos2}, \code{pos3},
#'       \code{prob}, \code{context_entropy}.}
#'     \item{\code{bigram}}{Columns \code{pos2}, \code{pos3}, \code{prob},
#'       \code{context_entropy} — marginalised over \code{pos1}.}
#'     \item{\code{unigram}}{Columns \code{pos3}, \code{prob} — marginal tag
#'       frequency.}
#'   }
prepare_pos_model <- function(pos_ngrams) {
  dt <- copy(pos_ngrams)

  # Trigram table
  dt[, context_N := sum(N), by = .(pos1, pos2)]
  dt[, prob := N / context_N]
  dt[, context_entropy := -sum(prob * log2(prob)), by = .(pos1, pos2)]
  tri <- dt[, .(pos1, pos2, pos3, prob, context_entropy)]
  setkey(tri, pos1, pos2, pos3)

  # Bigram table — marginalise over pos1
  bi <- dt[, .(N = sum(N)), by = .(pos2, pos3)]
  bi[, context_N := sum(N), by = pos2]
  bi[, prob := N / context_N]
  bi[, context_entropy := -sum(prob * log2(prob)), by = pos2]
  bi <- bi[, .(pos2, pos3, prob, context_entropy)]
  setkey(bi, pos2, pos3)

  # Unigram table — marginal tag frequency
  uni <- dt[, .(N = sum(N)), by = pos3]
  uni[, prob := N / sum(N)]
  uni <- uni[, .(pos3, prob)]
  setkey(uni, pos3)

  return(list(trigram = tri, bigram = bi, unigram = uni))
}


#' Score POS surprisal and entropy on a parsed corpus
#'
#' Given a corpus data.table (one row per token, as returned by
#' \code{udpipe::udpipe_annotate} coerced via \code{setDT()}) and a POS
#' trigram model from \code{prepare_pos_model()}, computes token-, sentence-,
#' and document-level surprisal, entropy, and entropy reduction.
#'
#' The trigram model and the corpus must share the same tagging scheme:
#' a corpus parsed with the ALSI verb-modified UDPipe model should be scored
#' with a model built from \code{fr_gsd-ud-train_alsi-verbpos.conllu}, and a
#' corpus parsed with the official model with the official ngram table.
#'
#' Unseen trigrams receive \code{NA} surprisal (no smoothing — backoff will be
#' added separately).
#'
#' @param dt_corpus data.table with at minimum columns \code{doc_id},
#'   \code{sentence_id}, \code{token_id}, \code{upos}.
#' @param pos_model List as returned by \code{prepare_pos_model()}, containing
#'   \code{trigram}, \code{bigram}, and \code{unigram} tables.
#' @param exclude_pos Character vector of UPOS tags to exclude from scoring.
#'   Excluded tokens are dropped before trigram contexts are formed, so they
#'   do not interrupt sequences. Pass the same tags used in
#'   \code{build_pos_ngrams()} for consistency.
#' @param use_sentence_boundaries Logical. If \code{TRUE}, prepend two
#'   \code{<BOS>} tokens and append one \code{<EOS>} token to each sentence
#'   before forming trigrams. This scores the first and last content tokens
#'   (which would otherwise lack a full left context). Must match the value
#'   used when building the model with \code{build_pos_ngrams()}.
#'   BOS/EOS rows are not included in \code{token_surprisal} output.
#' @param backoff_scale Numeric. Stupid Backoff discount applied at each
#'   fallback level (Brants et al. 2007). When a trigram is unseen the bigram
#'   probability is multiplied by \code{backoff_scale}; if the bigram is also
#'   unseen the unigram probability is multiplied by
#'   \code{backoff_scale^2}. Set to \code{NULL} to disable backoff (unseen
#'   trigrams receive \code{NA} surprisal, original behaviour).
#' @returns A named list with three data.tables:
#'   \describe{
#'     \item{\code{token_surprisal}}{One row per scored token: \code{doc_id},
#'       \code{sentence_id}, \code{token_id}, \code{upos},
#'       \code{pos_surprisal}, \code{pos_entropy},
#'       \code{pos_entropy_reduction}.}
#'     \item{\code{sent_surprisal}}{One row per sentence: \code{doc_id},
#'       \code{sentence_id}, plus mean/sd of the three token-level measures.}
#'     \item{\code{doc_surprisal}}{One row per document: \code{doc_id}, plus
#'       mean/sd of the three token-level measures.}
#'   }
pos_surprisal <- function(dt_corpus, pos_model, exclude_pos = character(0),
                          use_sentence_boundaries = FALSE,
                          backoff_scale = NULL) {
  dt <- setDT(copy(dt_corpus))[, .(doc_id, sentence_id, token_id, upos)]

  if (length(exclude_pos) > 0L) {
    dt <- dt[!upos %chin% exclude_pos]
  }

  if (use_sentence_boundaries) {
    # Prepend two <BOS> and append one <EOS> per sentence.
    # token_id NA marks boundary rows so they can be stripped from output.
    bos_eos <- dt[, {
      list(
        token_id = NA_character_,
        upos     = c("<BOS>", "<BOS>", .SD$upos, "<EOS>")
      )
    }, by = .(doc_id, sentence_id)]
    dt <- bos_eos
  }

  dt[, pos1 := shift(upos, 2L), by = sentence_id]
  dt[, pos2 := shift(upos, 1L), by = sentence_id]
  dt[, pos3 := upos]

  # Drop tokens without a full trigram context.
  # With boundaries every real token has context; without, drop first two.
  dt <- dt[!is.na(pos1) & !is.na(pos2)]

  # Strip boundary tokens from scored rows (they have no token_id)
  dt <- dt[upos != "<BOS>" & upos != "<EOS>"]

  # Join trigram table — unseen trigrams get NA prob/context_entropy
  dt <- pos_model$trigram[dt, on = .(pos1, pos2, pos3)]

  if (!is.null(backoff_scale)) {
    # Stupid Backoff: for unseen trigrams fall back to scaled bigram prob;
    # for unseen bigrams fall back to scaled^2 unigram prob.
    # context_entropy is taken from whichever level provided the probability.
    bi  <- pos_model$bigram
    uni <- pos_model$unigram

    missing_tri <- is.na(dt$prob)
    if (any(missing_tri)) {
      bi_lookup <- bi[dt[missing_tri], on = .(pos2, pos3),
                      .(prob_bi = prob, ce_bi = context_entropy)]
      dt[missing_tri, prob             := backoff_scale * bi_lookup$prob_bi]
      dt[missing_tri, context_entropy  := bi_lookup$ce_bi]

      # Second level: trigram AND bigram unseen
      missing_bi <- missing_tri & is.na(dt$prob)
      if (any(missing_bi)) {
        uni_lookup <- uni[dt[missing_bi], on = .(pos3), .(prob_uni = prob)]
        dt[missing_bi, prob            := (backoff_scale^2) * uni_lookup$prob_uni]
        dt[missing_bi, context_entropy := NA_real_]  # no meaningful entropy at unigram level
      }
    }
  }

  dt[, pos_surprisal := -log2(prob)]
  dt[, pos_entropy_reduction := shift(context_entropy, 1L) - context_entropy,
     by = sentence_id]
  setnames(dt, "context_entropy", "pos_entropy")

  token_dt <- dt[, .(doc_id, sentence_id, token_id, upos,
                     pos_surprisal, pos_entropy, pos_entropy_reduction)]

  sent_dt <- token_dt[, .(
    mean_pos_surprisal         = mean(pos_surprisal,         na.rm = TRUE),
    sd_pos_surprisal           = sd(pos_surprisal,           na.rm = TRUE),
    mean_pos_entropy           = mean(pos_entropy,           na.rm = TRUE),
    sd_pos_entropy             = sd(pos_entropy,             na.rm = TRUE),
    mean_pos_entropy_reduction = mean(pos_entropy_reduction, na.rm = TRUE),
    sd_pos_entropy_reduction   = sd(pos_entropy_reduction,   na.rm = TRUE)
  ), by = .(doc_id, sentence_id)]

  doc_dt <- token_dt[, .(
    mean_pos_surprisal         = mean(pos_surprisal,         na.rm = TRUE),
    sd_pos_surprisal           = sd(pos_surprisal,           na.rm = TRUE),
    mean_pos_entropy           = mean(pos_entropy,           na.rm = TRUE),
    sd_pos_entropy             = sd(pos_entropy,             na.rm = TRUE),
    mean_pos_entropy_reduction = mean(pos_entropy_reduction, na.rm = TRUE),
    sd_pos_entropy_reduction   = sd(pos_entropy_reduction,   na.rm = TRUE)
  ), by = doc_id]

  return(list(
    token_surprisal = token_dt,
    sent_surprisal  = sent_dt,
    doc_surprisal   = doc_dt
  ))
}
