# Functions for multi-word expression (MWE) matching and feature extraction.
library(data.table)

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
    lexicon_col %in% names(dt_lexicon)
  )

  if (!lexicon_n_col %in% names(dt_lexicon)) {
    dt_lexicon <- copy(dt_lexicon)
    dt_lexicon[, (lexicon_n_col) := lengths(strsplit(get(lexicon_col), " "))]
  }

  use_pos <- isTRUE(pos_filter) && upos_col %in% names(dt_tokens) &&
    lexicon_cat_col %in% names(dt_lexicon)

  copy_cols <- c("doc_id", "sentence_id", "token_id")
  if (use_pos) copy_cols <- c(copy_cols, upos_col)
  dt <- dt_tokens[, ..copy_cols]
  dt[, token_lower := tolower(dt_tokens[[token_col]])]
  if (use_pos) setnames(dt, upos_col, ".upos", skip_absent = TRUE)

  # token_id ranges like "30-31" (UDPipe contractions) are expansion rows — drop them
  dt[, .tid_num := suppressWarnings(as.numeric(token_id))]
  dt <- dt[!is.na(.tid_num)]
  setorderv(dt, c("doc_id", "sentence_id", ".tid_num"))
  dt[, .tid_num := NULL]

  dt_lex <- copy(dt_lexicon)
  setnames(dt_lex, c(lexicon_col, lexicon_n_col), c(".lex_forme", ".lex_n"))

  max_k   <- max(dt_lex$.lex_n)
  results <- vector("list", max_k)

  for (k in seq_len(max_k)) {
    lex_k <- dt_lex[.lex_n == k]
    if (nrow(lex_k) == 0L) next

    lex_formes_k <- unique(lex_k[, .(.lex_forme)])

    if (k == 1L) {
      dt_k <- if (use_pos) {
        dt[, .(doc_id, sentence_id, token_id_start = token_id,
               token_id_end = token_id, .ngram = token_lower, .upos_start = .upos)]
      } else {
        dt[, .(doc_id, sentence_id, token_id_start = token_id,
               token_id_end = token_id, .ngram = token_lower)]
      }
    } else {
      shifted_cols <- paste0(".tok", seq_len(k) - 1L)
      dt[, (shifted_cols[1]) := token_lower]
      for (j in 2:k)
        dt[, (shifted_cols[j]) := shift(token_lower, n = j - 1L, type = "lead")]
      dt[, .doc_end  := shift(doc_id,      n = k - 1L, type = "lead")]
      dt[, .sent_end := shift(sentence_id, n = k - 1L, type = "lead")]
      dt[, .tid_end  := shift(token_id,    n = k - 1L, type = "lead")]

      valid <- dt[doc_id == .doc_end & sentence_id == .sent_end]
      valid[, .ngram := do.call(paste, c(.SD, sep = " ")), .SDcols = shifted_cols]

      dt_k <- if (use_pos) {
        valid[, .(doc_id, sentence_id, token_id_start = token_id,
                  token_id_end = .tid_end, .ngram, .upos_start = .upos)]
      } else {
        valid[, .(doc_id, sentence_id, token_id_start = token_id,
                  token_id_end = .tid_end, .ngram)]
      }

      dt[, c(shifted_cols, ".doc_end", ".sent_end", ".tid_end") := NULL]
    }

    matched <- dt_k[lex_formes_k, on = .(.ngram = .lex_forme), nomatch = 0L]
    if (nrow(matched) > 0L) {
      matched[, matched_forme := .ngram][, .ngram := NULL]
      results[[k]] <- matched
    }
  }

  dt_matches <- rbindlist(results, fill = TRUE)

  if (nrow(dt_matches) == 0L) {
    return(data.table(
      doc_id = character(), sentence_id = integer(),
      token_id_start = character(), token_id_end = character(),
      matched_forme = character()
    ))
  }

  dt_matches <- merge(dt_matches, dt_lex,
                      by.x = "matched_forme", by.y = ".lex_forme",
                      all.x = TRUE, allow.cartesian = TRUE)
  dt_matches[, .lex_n := NULL]

  if (use_pos) {
    cat_vec       <- dt_matches[[lexicon_cat_col]]
    expected_upos <- cat_to_upos[cat_vec]
    has_mapping   <- !sapply(expected_upos, is.null)
    expected_upos[!has_mapping] <- NA_character_
    dt_matches[, .expected_upos := unlist(expected_upos)]
    dt_matches <- dt_matches[is.na(.expected_upos) | .upos_start == .expected_upos]
    dt_matches[, c(".expected_upos", ".upos_start") := NULL]
  }

  setcolorder(dt_matches, c("doc_id", "sentence_id", "token_id_start", "token_id_end", "matched_forme"))
  setorderv(dt_matches, c("doc_id", "sentence_id", "token_id_start"))
  return(dt_matches)
}

connective_density_features <- function(dt_corpus, matches) {
  match_key <- function(dt) paste(dt$sentence_id, dt$token_id_start)

  dt_word_counts <- dt_corpus[compte == TRUE, .(word_count = .N), by = doc_id]

  dt_conn_total <- matches[, .(n_connectives = uniqueN(match_key(.SD))), by = doc_id]

  dt_conn_group <- dcast(
    matches[, .(n = uniqueN(match_key(.SD))), by = .(doc_id, relation_group)],
    doc_id ~ relation_group, value.var = "n", fill = 0L
  )
  dt_conn_cat <- dcast(
    matches[, .(n = uniqueN(match_key(.SD))), by = .(doc_id, cat)],
    doc_id ~ cat, value.var = "n", fill = 0L
  )
  setnames(dt_conn_cat, setdiff(names(dt_conn_cat), "doc_id"),
           paste0("cat_", setdiff(names(dt_conn_cat), "doc_id")))

  dt_features <- Reduce(function(a, b) merge(a, b, by = "doc_id", all.x = TRUE),
                        list(dt_word_counts, dt_conn_total, dt_conn_group, dt_conn_cat))

  conn_cols <- setdiff(names(dt_features), c("doc_id", "word_count"))
  for (col in conn_cols) set(dt_features, which(is.na(dt_features[[col]])), col, 0L)

  density_cols <- paste0(conn_cols, "_per100w")
  dt_features[, (density_cols) := lapply(.SD, function(x) x / word_count * 100),
              .SDcols = conn_cols]
  return(dt_features)
}
