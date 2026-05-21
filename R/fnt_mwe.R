# Functions for multi-word expression (MWE) matching and feature extraction.
library(data.table)

#' Match Multi-Word Expressions in a Parsed Token Table
#'
#' Finds lexicon entries in a token-level corpus by sliding n-gram windows
#' within each document and sentence. Matching is case-insensitive: corpus
#' tokens are lowercased before comparison, and lexicon forms are expected to
#' be stored in lowercase.
#'
#' The input token table is expected to follow the post-processed UDPipe shape
#' used elsewhere in ALSI, with \code{doc_id}, \code{sentence_id}, and
#' \code{token_id}. Multi-word token range rows such as \code{"30-31"} are
#' ignored so matching operates on real token rows only.
#'
#' If \code{pos_filter = TRUE} and both POS/category columns are available,
#' matches are filtered by the UPOS tag of the first matched token. Categories
#' are mapped to expected UPOS tags with \code{cat_to_upos}; categories absent
#' from the mapping are left unfiltered.
#'
#' @param dt_tokens A parsed \code{data.table} with columns \code{doc_id},
#'   \code{sentence_id}, \code{token_id}, and the token column named by
#'   \code{token_col}. If POS filtering is used, it should also contain
#'   \code{upos_col}.
#' @param dt_lexicon A \code{data.table} of MWE entries. It must contain the
#'   lowercase surface form column named by \code{lexicon_col}. If
#'   \code{lexicon_n_col} is absent, token counts are computed from spaces in
#'   \code{lexicon_col}.
#' @param token_col Name of the token column in \code{dt_tokens}. Default
#'   \code{"token"}.
#' @param lexicon_col Name of the lowercase MWE surface-form column in
#'   \code{dt_lexicon}. Default \code{"forme_lower"}.
#' @param lexicon_n_col Name of the lexicon column giving the number of tokens
#'   in each MWE. Default \code{"n_tokens"}.
#' @param pos_filter Logical; if TRUE, filter matches by the UPOS tag of the
#'   first token when the required POS/category columns are available.
#' @param upos_col Name of the UPOS column in \code{dt_tokens}. Default
#'   \code{"upos"}.
#' @param lexicon_cat_col Name of the grammatical category column in
#'   \code{dt_lexicon}. Default \code{"cat"}.
#' @param cat_to_upos Named list mapping lexicon categories to expected UPOS
#'   tags for POS filtering.
#' @returns A \code{data.table} with one row per match. The leading columns are
#'   \code{doc_id}, \code{sentence_id}, \code{token_id_start},
#'   \code{token_id_end}, and \code{matched_forme}, followed by any additional
#'   lexicon metadata columns. Returns zero rows with the same leading columns
#'   when no matches are found.
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

  token_key_cols <- c("doc_id", "sentence_id", "token_id")

  stopifnot(
    is.data.table(dt_tokens),
    is.data.table(dt_lexicon),
    all(token_key_cols %in% names(dt_tokens)),
    token_col %in% names(dt_tokens),
    lexicon_col %in% names(dt_lexicon)
  )

  if (!lexicon_n_col %in% names(dt_lexicon)) {
    dt_lexicon <- copy(dt_lexicon)
    dt_lexicon[, (lexicon_n_col) := lengths(strsplit(get(lexicon_col), " "))]
  }

  use_pos <- isTRUE(pos_filter) && upos_col %in% names(dt_tokens) &&
    lexicon_cat_col %in% names(dt_lexicon)

  copy_cols <- token_key_cols
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

  empty_matches <- data.table(
    doc_id = character(), sentence_id = integer(),
    token_id_start = character(), token_id_end = character(),
    matched_forme = character()
  )

  if (nrow(dt) == 0L || nrow(dt_lex) == 0L) {
    return(empty_matches)
  }

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
      windows <- dt[, {
        shifted_tokens <- shift(token_lower, n = seq.int(0L, k - 1L), type = "lead")
        names(shifted_tokens) <- shifted_cols
        c(
          list(
            token_id_start = token_id,
            token_id_end = shift(token_id, n = k - 1L, type = "lead")
          ),
          if (use_pos) list(.upos_start = .upos),
          shifted_tokens
        )
      }, by = .(doc_id, sentence_id)]

      valid <- windows[!is.na(token_id_end)]
      valid[, .ngram := do.call(paste, c(.SD, sep = " ")), .SDcols = shifted_cols]

      dt_k <- if (use_pos) {
        valid[, .(doc_id, sentence_id, token_id_start,
                  token_id_end, .ngram, .upos_start)]
      } else {
        valid[, .(doc_id, sentence_id, token_id_start,
                  token_id_end, .ngram)]
      }
    }

    matched <- dt_k[lex_formes_k, on = .(.ngram = .lex_forme), nomatch = 0L]
    if (nrow(matched) > 0L) {
      matched[, matched_forme := .ngram][, .ngram := NULL]
      results[[k]] <- matched
    }
  }

  dt_matches <- rbindlist(results, fill = TRUE)

  if (nrow(dt_matches) == 0L) {
    return(empty_matches)
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

#' Compute Document-Level Connective Density Features
#'
#' Aggregates MWE matches, typically produced by
#' \code{match_multiword_sequences()}, into one row per document. The function
#' counts distinct matched start positions, adds optional breakdowns by
#' \code{relation_group} and \code{cat} when those columns are present, and
#' computes per-100-word density variants for every count column.
#'
#' Documents with no matches are retained with zero connective counts. Word
#' counts are computed from rows where \code{compte == TRUE}, matching the
#' countable-token convention used in the parsed corpus.
#'
#' @param dt_corpus A parsed \code{data.table} with columns \code{doc_id} and
#'   \code{compte}.
#' @param matches A \code{data.table} of MWE matches with columns
#'   \code{doc_id}, \code{sentence_id}, and \code{token_id_start}. If present,
#'   \code{relation_group} and \code{cat} are used for dynamic feature
#'   breakdowns.
#' @returns A \code{data.table} with one row per document and columns:
#'   \describe{
#'     \item{\code{doc_id}}{Document identifier.}
#'     \item{\code{word_count}}{Countable token count from \code{dt_corpus}.}
#'     \item{\code{n_connectives}}{Number of distinct connective matches.}
#'     \item{\code{<relation_group>}}{Optional count columns for each observed
#'       relation group.}
#'     \item{\code{cat_<cat>}}{Optional count columns for each observed lexicon
#'       category.}
#'     \item{\code{*_per100w}}{Per-100-word density columns for every count
#'       feature.}
#'   }
connective_density_features <- function(dt_corpus, matches) {
  stopifnot(
    is.data.table(dt_corpus),
    is.data.table(matches),
    all(c("doc_id", "compte") %in% names(dt_corpus)),
    all(c("doc_id", "sentence_id", "token_id_start") %in% names(matches))
  )

  dt_word_counts <- dt_corpus[compte == TRUE, .(word_count = .N), by = doc_id]

  if (nrow(matches) == 0L) {
    dt_features <- copy(dt_word_counts)
    dt_features[, n_connectives := 0L]
    dt_features[, n_connectives_per100w := 0]
    return(dt_features)
  }

  dt_match_keys <- unique(matches[, .(doc_id, sentence_id, token_id_start)])
  dt_conn_total <- dt_match_keys[, .(n_connectives = .N), by = doc_id]

  feature_tables <- list(dt_word_counts, dt_conn_total)

  if ("relation_group" %in% names(matches)) {
    dt_group_keys <- unique(matches[, .(doc_id, sentence_id, token_id_start, relation_group)])
    dt_conn_group <- dcast(
      dt_group_keys[, .(n = .N), by = .(doc_id, relation_group)],
      doc_id ~ relation_group, value.var = "n", fill = 0L
    )
    feature_tables <- c(feature_tables, list(dt_conn_group))
  }

  if ("cat" %in% names(matches)) {
    dt_cat_keys <- unique(matches[, .(doc_id, sentence_id, token_id_start, cat)])
    dt_conn_cat <- dcast(
      dt_cat_keys[, .(n = .N), by = .(doc_id, cat)],
      doc_id ~ cat, value.var = "n", fill = 0L
    )
    setnames(dt_conn_cat, setdiff(names(dt_conn_cat), "doc_id"),
             paste0("cat_", setdiff(names(dt_conn_cat), "doc_id")))
    feature_tables <- c(feature_tables, list(dt_conn_cat))
  }

  dt_features <- Reduce(function(a, b) merge(a, b, by = "doc_id", all.x = TRUE),
                        feature_tables)

  conn_cols <- setdiff(names(dt_features), c("doc_id", "word_count"))
  for (col in conn_cols) set(dt_features, which(is.na(dt_features[[col]])), col, 0L)

  density_cols <- paste0(conn_cols, "_per100w")
  dt_features[, (density_cols) := lapply(.SD, function(x) x / word_count * 100),
              .SDcols = conn_cols]
  return(dt_features)
}
