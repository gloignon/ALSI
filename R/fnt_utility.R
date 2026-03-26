# Utility functions shared across demos and pipelines.
library(data.table)

# ---------------------------------------------------------------------------
# Multi-word sequence matching against CoNLL-style parsed data
# ---------------------------------------------------------------------------
#
# match_multiword_sequences()
#
# Given a token-per-row data.table (CoNLL-style) and a lexicon of multi-word
# expressions, find all occurrences efficiently using rolling n-gram joins.
#
# Arguments:
#   dt_tokens   - data.table with at least: doc_id, sentence_id, token_id, token
#                 Must be ordered by (doc_id, sentence_id, token_id).
#   dt_lexicon  - data.table with at least: forme_lower (space-separated MWE),
#                 n_tokens (number of tokens in the expression).
#                 May contain additional columns (relation, cat, etc.) that will
#                 be carried through to the output.
#   token_col   - column in dt_tokens to match against (default "token").
#                 Matched case-insensitively (tolower applied).
#   lexicon_col - column in dt_lexicon containing the expression (default "forme_lower").
#   lexicon_n_col - column in dt_lexicon with token count (default "n_tokens").
#
# Returns:
#   A data.table with one row per match, containing:
#     doc_id, sentence_id, token_id_start, token_id_end, matched_forme,
#     plus all extra columns from dt_lexicon.
#
# Algorithm:
#   For each n-gram length k (from max down to 1), paste k consecutive tokens
#   into a single key string, then hash-join against the lexicon entries of
#   length k. This is O(N × max_k) where N = number of tokens.

match_multiword_sequences <- function(dt_tokens, dt_lexicon,
                                       token_col = "token",
                                       lexicon_col = "forme_lower",
                                       lexicon_n_col = "n_tokens",
                                       pos_filter = FALSE,
                                       upos_col = "upos",
                                       lexicon_cat_col = "cat",
                                       cat_to_upos = list(
                                         cco = "CCONJ",
                                         csu = "SCONJ",
                                         prep = "ADP",
                                         `prep-V-ant` = "ADP",
                                         adv  = "ADV"
                                       )) {

  stopifnot(
    is.data.table(dt_tokens),
    is.data.table(dt_lexicon),
    all(c("doc_id", "sentence_id", "token_id") %in% names(dt_tokens)),
    token_col %in% names(dt_tokens),
    lexicon_col %in% names(dt_lexicon),
    lexicon_n_col %in% names(dt_lexicon)
  )

  use_pos <- isTRUE(pos_filter) && upos_col %in% names(dt_tokens) &&
    lexicon_cat_col %in% names(dt_lexicon)

  # Work on a minimal copy to avoid modifying the original
  copy_cols <- c("doc_id", "sentence_id", "token_id")
  if (use_pos) copy_cols <- c(copy_cols, upos_col)
  dt <- dt_tokens[, ..copy_cols]
  dt[, token_lower := tolower(dt_tokens[[token_col]])]
  if (use_pos) setnames(dt, upos_col, ".upos", skip_absent = TRUE)
  # token_id is often character ("1","2",...,"10") — sort numerically within sentence.
  # Multi-word token ranges like "30-31" (UDPipe contractions) get NA; drop them
  # as they are expansion rows, not real token positions.
  dt[, .tid_num := suppressWarnings(as.numeric(token_id))]
  dt <- dt[!is.na(.tid_num)]
  setorderv(dt, c("doc_id", "sentence_id", ".tid_num"))
  dt[, .tid_num := NULL]

  # Add a row index for fast shifting
  dt[, .row := .I]

  # Deduplicate lexicon entries (same forme may appear with different relations)
  lex_extra_cols <- setdiff(names(dt_lexicon), c(lexicon_col, lexicon_n_col))
  dt_lex <- copy(dt_lexicon)
  setnames(dt_lex, c(lexicon_col, lexicon_n_col), c(".lex_forme", ".lex_n"))

  max_k <- max(dt_lex$.lex_n)
  results <- vector("list", max_k)

  for (k in seq_len(max_k)) {
    # Subset lexicon to entries of length k
    lex_k <- dt_lex[.lex_n == k]
    if (nrow(lex_k) == 0L) next

    # Unique formes for this k (for the join)
    lex_formes_k <- unique(lex_k[, .(.lex_forme)])

    if (k == 1L) {
      # Simple case: direct join on single token
      if (use_pos) {
        dt_k <- dt[, .(doc_id, sentence_id, token_id_start = token_id,
                        token_id_end = token_id, .ngram = token_lower, .upos_start = .upos)]
      } else {
        dt_k <- dt[, .(doc_id, sentence_id, token_id_start = token_id,
                        token_id_end = token_id, .ngram = token_lower)]
      }
    } else {
      # Build n-gram by pasting k consecutive tokens within same doc+sentence
      # Use shift to get tokens at positions +1, +2, ..., +k-1
      shifted_cols <- paste0(".tok", seq_len(k) - 1L)
      dt[, (shifted_cols[1]) := token_lower]
      for (j in 2:k) {
        dt[, (shifted_cols[j]) := shift(token_lower, n = j - 1L, type = "lead")]
      }
      # Also shift doc_id and sentence_id to ensure all k tokens are in the same sentence
      dt[, .doc_end := shift(doc_id, n = k - 1L, type = "lead")]
      dt[, .sent_end := shift(sentence_id, n = k - 1L, type = "lead")]
      dt[, .tid_end := shift(token_id, n = k - 1L, type = "lead")]

      # Only keep rows where all k tokens are in the same doc+sentence
      valid <- dt[doc_id == .doc_end & sentence_id == .sent_end]
      valid[, .ngram := do.call(paste, c(.SD, sep = " ")), .SDcols = shifted_cols]

      if (use_pos) {
        dt_k <- valid[, .(doc_id, sentence_id, token_id_start = token_id,
                           token_id_end = .tid_end, .ngram, .upos_start = .upos)]
      } else {
        dt_k <- valid[, .(doc_id, sentence_id, token_id_start = token_id,
                           token_id_end = .tid_end, .ngram)]
      }

      # Clean up temp columns
      dt[, c(shifted_cols, ".doc_end", ".sent_end", ".tid_end") := NULL]
    }

    # Join against lexicon
    matched <- dt_k[lex_formes_k, on = .(.ngram = .lex_forme), nomatch = 0L]

    if (nrow(matched) > 0L) {
      matched[, matched_forme := .ngram]
      matched[, .ngram := NULL]
      results[[k]] <- matched
    }
  }

  dt_matches <- rbindlist(results, fill = TRUE)

  if (nrow(dt_matches) == 0L) {
    # Return empty table with correct schema
    return(data.table(
      doc_id = character(), sentence_id = integer(),
      token_id_start = character(), token_id_end = character(),
      matched_forme = character()
    ))
  }

  # Join back the full lexicon metadata (relation, cat, etc.)
  dt_matches <- merge(
    dt_matches,
    dt_lex,
    by.x = "matched_forme", by.y = ".lex_forme",
    all.x = TRUE, allow.cartesian = TRUE
  )
  dt_matches[, .lex_n := NULL]

  # POS filtering: keep only matches where first token's UPOS is compatible with lexicon cat
  if (use_pos && ".upos_start" %in% names(dt_matches) && lexicon_cat_col %in% names(dt_matches)) {
    # Build expected UPOS per row from cat_to_upos mapping
    cat_vec <- dt_matches[[lexicon_cat_col]]
    expected_upos <- cat_to_upos[cat_vec]
    # Entries with unknown cat get NA — keep them (don't filter)
    has_mapping <- !sapply(expected_upos, is.null)
    expected_upos[!has_mapping] <- NA_character_
    expected_upos <- unlist(expected_upos)
    dt_matches[, .expected_upos := expected_upos]
    dt_matches <- dt_matches[is.na(.expected_upos) | .upos_start == .expected_upos]
    dt_matches[, c(".expected_upos", ".upos_start") := NULL]
  } else if (".upos_start" %in% names(dt_matches)) {
    dt_matches[, .upos_start := NULL]
  }

  setcolorder(dt_matches, c("doc_id", "sentence_id", "token_id_start", "token_id_end", "matched_forme"))
  setorderv(dt_matches, c("doc_id", "sentence_id", "token_id_start"))

  return(dt_matches)
}

# Split a block of text into sentences on sentence-ending punctuation
# followed by whitespace and an uppercase letter (including French accented).
# Returns a character vector of trimmed, non-empty sentences.
split_sentences <- function(text) {
  s <- unlist(strsplit(text, "(?<=[.!?])\\s+(?=[A-Z\u00c0\u00c2\u00c9\u00c8\u00ca\u00cb\u00ce\u00cf\u00d4\u00d9\u00db\u00dc\u00c7])", perl = TRUE))
  trimws(s[nchar(trimws(s)) > 0])
}

# Read a text file and return a tibble with one row per sentence.
# Handles both one-sentence-per-line and single-line-blob formats.
read_sentences <- function(path, doc_id) {
  raw <- readLines(path, encoding = "UTF-8", warn = FALSE)
  sents <- raw %>%
    paste(collapse = " ") %>%
    split_sentences()
  if (length(sents) == 0) return(NULL)
  tibble::tibble(doc_id = doc_id,
                 sentence_id = seq_along(sents),
                 sentence = sents)
}
