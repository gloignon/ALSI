library(data.table)

# Annotation-derived morphological complexity features.
# These features are computed entirely from UDPipe parser/tagger output
# (feats column and dependency relations) — no external lexical database needed.
#
# For word-level psycholinguistic norm features (MorphoLex-FR morpheme count,
# family size, imageability, etc.) see fnt_fr_norms.R.
#
# References:
#   Inflectional complexity: Sagot & Walther (2011,
#     Proceedings of the 9th International Conference on Finite-State Methods
#     and Natural Language Processing).
#   Passive difficulty: Forster & Olbrei (1974,
#     Quarterly Journal of Experimental Psychology);
#     Štajner & Mitkov (2012, LREC).
#   Subjunctive and other verb forms are tracked via verb_tense_features()
#   in fnt_counters.R, which provides count and proportion summaries.


# ── Internal helpers ──────────────────────────────────────────────────────────

# Count UD feature=value pairs in a feats string vector.
# "Mood=Ind|VerbForm=Fin|Number=Sing" → 3; NA or "" → 0.
.count_feats <- function(feats) {
  n <- lengths(strsplit(feats, "|", fixed = TRUE))
  n[is.na(feats) | feats == ""] <- 0L
  return(n)
}

# Extract the value of one UD feature key from a feats string vector.
# Returns NA_character_ when the key is absent or feats is NA.
.extract_feat <- function(feats, key) {
  pattern <- paste0("(?:^|\\|)", key, "=([^|]+)")
  ifelse(
    grepl(pattern, feats, perl = TRUE),
    sub(paste0(".*(?:^|\\|)", key, "=([^|]+).*"), "\\1", feats, perl = TRUE),
    NA_character_
  )
}

# Shannon entropy (bits) over a character vector of category labels.
# Returns NA if x has length 0 after removing NAs.
.entropy_bits <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  p <- table(x) / length(x)
  p <- p[p > 0]
  return(-sum(p * log2(p)))
}


# ── inflectional_profile ──────────────────────────────────────────────────────

#' Compute inflectional profile features from UDPipe \code{feats} strings.
#'
#' Parses the pipe-separated \code{feats} column produced by UDPipe to derive
#' document-level count/proportion summaries over morphosyntactic annotations.
#'
#' @param dt_corpus Parsed data.table. Required columns: \code{doc_id},
#'   \code{upos}, \code{feats}, \code{compte}. If \code{sentence_id},
#'   \code{token_id}, \code{head_token_id}, and \code{dep_rel} are also
#'   present, \code{morph_prop_compound_tense} and \code{morph_prop_passive}
#'   are computed from the dependency tree; otherwise they return \code{NA}.
#'
#' @returns A \code{data.frame} with one row per \code{doc_id}:
#' \describe{
#'   \item{morph_feats_per_word}{Mean number of UD feature=value pairs per
#'     feats-bearing countable token.}
#'   \item{morph_verb_feat_density}{Mean UD features per VERB/AUX token.}
#'   \item{morph_part_rate}{Proportion of VERB/AUX tokens tagged
#'     \code{VerbForm=Part} (any participle, including adjectival and
#'     nominalized uses). Broad participial density; see
#'     \code{morph_prop_compound_tense} for a narrower compound-tense measure.}
#'   \item{morph_prop_compound_tense}{Proportion of VERB/AUX tokens that are
#'     true periphrastic tense heads: \code{VerbForm=Part} AND have at least
#'     one être/avoir \code{aux}/\code{aux:pass} child in the dependency tree.
#'     Returns \code{NA} without dep columns.}
#'   \item{morph_prop_passive}{Proportion of VERB/AUX tokens with
#'     \code{dep_rel = "aux:pass"} — the être auxiliary of a passive
#'     construction. French GSD encodes passive via dep_rel, not
#'     \code{Voice=Pass}. Returns \code{NA} without dep columns.}
#' }
inflectional_profile <- function(dt_corpus) {
  dt <- setDT(copy(dt_corpus))

  required <- c("doc_id", "upos", "feats", "compte")
  if (!all(required %in% names(dt)))
    stop("dt_corpus missing columns: ",
         paste(setdiff(required, names(dt)), collapse = ", "))

  dep_cols     <- c("sentence_id", "token_id", "head_token_id", "dep_rel")
  has_dep_cols <- all(dep_cols %in% names(dt))

  dt[, .n_feats := .count_feats(feats)]

  # Mean features per countable token (any UPOS with feats present).
  feat_density <- dt[compte == TRUE & .n_feats > 0L,
                     .(morph_feats_per_word = mean(.n_feats)),
                     by = doc_id]

  # Verb/AUX tokens: extract relevant feature values.
  dt_v <- dt[compte == TRUE & upos %in% c("VERB", "AUX")]
  dt_v[, .verbform := .extract_feat(feats, "VerbForm")]
  dt_v[, .mood     := .extract_feat(feats, "Mood")]
  dt_v[, .is_fin   := !is.na(.verbform) & .verbform == "Fin"]

  # Compound tense and passive both require dependency columns.
  if (has_dep_cols) {
    sent_cols <- intersect(c("doc_id", "paragraph_id", "sentence_id"), names(dt))

    # Compound tense: VERB token with VerbForm=Part that has an être/avoir
    # aux child. Restricted to upos==VERB in the aggregation.
    dt_aux_heads <- dep_child_flags(
      dt,
      dep_rels     = c("aux", "aux:tense", "aux:pass"),
      lemma_filter = c("être", "etre", "avoir")
    )
    dt_aux_heads[, .is_aux_head := TRUE]
    setkeyv(dt_v, c(sent_cols, "token_id"))
    dt_v[dt_aux_heads, .is_aux_head := TRUE]
    dt_v[is.na(.is_aux_head), .is_aux_head := FALSE]

    # Passive: French GSD uses dep_rel="aux:pass" on the être auxiliary.
    dt_v[, .is_aux_pass := dep_rel == "aux:pass"]
  } else {
    dt_v[, .is_aux_head := NA]
    dt_v[, .is_aux_pass := NA]
  }

  verb_stats <- dt_v[, .(
    morph_verb_feat_density   = mean(.n_feats,                             na.rm = TRUE),
    morph_part_rate           = mean(!is.na(.verbform) & .verbform == "Part"),
    morph_prop_compound_tense = if (any(!is.na(.is_aux_head)))
                                  mean(upos == "VERB" & !is.na(.verbform) &
                                         .verbform == "Part" & .is_aux_head == TRUE)
                                else NA_real_,
    morph_prop_passive        = if (any(!is.na(.is_aux_pass)))
                                  mean(.is_aux_pass == TRUE)
                                else NA_real_
  ), by = doc_id]

  result <- merge(feat_density, verb_stats, by = "doc_id", all = TRUE)
  return(as.data.frame(result))
}
