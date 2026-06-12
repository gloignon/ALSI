library(data.table)

# Functions for joining French psycholinguistic norm databases to a parsed
# corpus and aggregating word-level scores to document-level features.
#
# Each function follows the same pattern:
#   1. Join the norm DB to corpus tokens (inflected form, fallback to lemma).
#   2. Restrict to content words (compte == TRUE, UPOS in a target set).
#   3. Aggregate per doc_id, returning one row per document.
#
# Current databases:
#   add_morpholex_features()      — MorphoLex-FR (morpheme count, family size)
#   add_oov_edit_features()       — minimum Levenshtein distance to lexicon for
#                                   OOV content words
#
# Planned additions:
#   add_imageability_features()   — imageability / concreteness norms
#   add_valence_features()        — affective valence / arousal / dominance

# Default UPOS tags treated as content words.
.norms_content_upos <- c("NOUN", "VERB", "ADJ", "ADV")


#' Join MorphoLex-FR to a parsed corpus and compute word-structure features.
#'
#' Matching is attempted in two passes: (1) inflected \code{token} →
#' MorphoLex-FR \code{word}; (2) \code{lemma} → \code{word} for tokens that
#' were not matched in pass 1. Only content words (\code{compte == TRUE} and
#' \code{upos} in \code{content_upos}) are considered.
#'
#' @param dt_corpus Parsed data.table (UDPipe output after post-processing).
#'   Required columns: \code{doc_id}, \code{token}, \code{lemma},
#'   \code{upos}, \code{compte}.
#' @param dt_morpholex MorphoLex-FR data.table as produced by
#'   \code{R/artefact_builders/build_morpholex_fr.R}. Required columns:
#'   \code{word}, \code{n_morphemes}, \code{root_fam_size},
#'   \code{pref_fam_size}, \code{suff_fam_size}. Defaults to a lazy
#'   \code{.alsi_load_db("morpholex")}; run \code{alsi_setup_databases("morpholex")}
#'   first, or pass an already-loaded data.table.
#' @param content_upos Character vector of UPOS tags counted as content words.
#'   Default: \code{c("NOUN","VERB","ADJ","ADV")}.
#'
#' @returns A \code{data.frame} with one row per \code{doc_id}:
#' \describe{
#'   \item{morph_match_rate}{Proportion of content words matched in MorphoLex-FR.}
#'   \item{morph_mean_n_morphemes}{Mean morpheme count (matched words only).
#'     More morphemes require more decomposition steps during reading
#'     (Taft & Forster 1975; Taft 1994).}
#'   \item{morph_prop_complex}{Proportion of matched content words with
#'     \eqn{n\_morphemes > 1}.}
#'   \item{morph_mean_root_famsize}{Mean \eqn{\log(1 + \text{root family size})}.
#'     Larger families predict faster lexical access (Schreuder & Baayen 1997;
#'     Bertram, Baayen & Schreuder 2000).}
#'   \item{morph_prop_prefixed}{Proportion of matched content words carrying a
#'     prefix (Taft & Forster 1975: prefix stripping is a mandatory early step).}
#'   \item{morph_prop_suffixed}{Proportion of matched content words carrying a
#'     suffix.}
#'   \item{morph_mean_suff_famsize}{Mean \eqn{\log(1 + \text{suffix family size})}
#'     for suffixed words.}
#' }
#'
#' @references
#'   Database: Mailhot, Wilson, Macoir, Deacon & Sánchez-Gutiérrez (2020,
#'     \emph{Behavior Research Methods}).
#'   Morpheme count / decomposition cost: Taft & Forster (1975);
#'     Taft (1994, \emph{Language and Cognitive Processes}).
#'   Root / affix family size: Schreuder & Baayen (1997,
#'     \emph{Journal of Memory and Language});
#'     Bertram, Baayen & Schreuder (2000, \emph{Journal of Memory and Language}).
add_morpholex_features <- function(dt_corpus,
                                   dt_morpholex = .alsi_load_db("morpholex"),
                                   content_upos = .norms_content_upos) {
  dt     <- setDT(copy(dt_corpus))
  dt_lex <- setDT(copy(dt_morpholex))

  required_corpus <- c("doc_id", "token", "lemma", "upos", "compte")
  required_lex    <- c("word", "n_morphemes", "root_fam_size",
                       "pref_fam_size", "suff_fam_size")
  if (!all(required_corpus %in% names(dt)))
    stop("dt_corpus missing columns: ",
         paste(setdiff(required_corpus, names(dt)), collapse = ", "))
  if (!all(required_lex %in% names(dt_lex)))
    stop("dt_morpholex missing columns: ",
         paste(setdiff(required_lex, names(dt_lex)), collapse = ", "))

  dt_cw <- dt[compte == TRUE & upos %in% content_upos]
  dt_cw[, .token_lc := tolower(token)]
  dt_cw[, .lemma_lc := tolower(lemma)]
  dt_lex[, .word_lc  := tolower(word)]

  morph_cols <- c("n_morphemes", "root_fam_size", "pref_fam_size", "suff_fam_size")
  setkey(dt_lex, .word_lc)

  # Pass 1: join by inflected token form.
  dt_tok <- merge(dt_cw,
                  dt_lex[, c(".word_lc", morph_cols), with = FALSE],
                  by.x = ".token_lc", by.y = ".word_lc",
                  all.x = TRUE)
  is_matched       <- !is.na(dt_tok$n_morphemes)
  dt_tok_matched   <- dt_tok[is_matched]
  dt_tok_unmatched <- dt_tok[!is_matched,
                              .SD, .SDcols = setdiff(names(dt_tok), morph_cols)]

  # Pass 2: join unmatched by lemma.
  dt_lem <- merge(dt_tok_unmatched,
                  dt_lex[, c(".word_lc", morph_cols), with = FALSE],
                  by.x = ".lemma_lc", by.y = ".word_lc",
                  all.x = TRUE)

  dt_joined <- rbindlist(list(dt_tok_matched, dt_lem), fill = TRUE)
  dt_joined[, .is_matched := !is.na(n_morphemes)]

  result <- dt_joined[, .(
    morph_match_rate        = mean(.is_matched),
    morph_mean_n_morphemes  = mean(n_morphemes,          na.rm = TRUE),
    morph_prop_complex      = mean(n_morphemes > 1L,     na.rm = TRUE),
    morph_mean_root_famsize = mean(log1p(root_fam_size), na.rm = TRUE),
    morph_prop_prefixed     = if (sum(.is_matched) > 0L)
                                mean(!is.na(pref_fam_size[.is_matched]))
                              else NA_real_,
    morph_prop_suffixed     = if (sum(.is_matched) > 0L)
                                mean(!is.na(suff_fam_size[.is_matched]))
                              else NA_real_,
    morph_mean_suff_famsize = {
      v <- log1p(suff_fam_size[!is.na(suff_fam_size)])
      if (length(v) > 0L) mean(v) else NA_real_
    }
  ), by = doc_id]

  return(as.data.frame(result))
}


# -- OOV edit distance --------------------------------------------------------

# Load a lexicon from a data.table / data.frame / file path (.Rds, .csv, .tsv).
.load_lexicon <- function(lexicon, word_col) {
  if (is.character(lexicon)) {
    ext     <- tolower(tools::file_ext(lexicon))
    lexicon <- switch(ext,
      rds = readRDS(lexicon),
      csv = data.table::fread(lexicon),
      tsv = data.table::fread(lexicon, sep = "\t"),
      stop("Unsupported lexicon file type: '", ext,
           "'. Use .Rds, .csv, or .tsv.")
    )
  }
  setDT(lexicon)
  if (!word_col %in% names(lexicon))
    stop("Column '", word_col, "' not found in lexicon. ",
         "Available columns: ", paste(names(lexicon), collapse = ", "))
  return(lexicon)
}


#' OOV minimum Levenshtein distance to a reference lexicon.
#'
#' For each content-word token not found in \code{lexicon} (by inflected form
#' then by lemma), computes the minimum Levenshtein edit distance to any entry
#' in the lexicon.  In-vocabulary words contribute to the OOV proportion but
#' are excluded from distance aggregation.
#'
#' Candidate search is restricted to lexicon entries whose length differs from
#' the query by at most \code{max_len_gap} characters, which reduces computation
#' without affecting accuracy for typical French vocabulary.
#'
#' @param dt_corpus Parsed data.table from \code{post_process_lexicon}.
#'   Required columns: \code{doc_id}, \code{token}, \code{lemma},
#'   \code{upos}, \code{compte}.
#' @param lexicon Reference lexicon: a data.table / data.frame, or a file path
#'   to an \code{.Rds}, \code{.csv}, or \code{.tsv} file. Defaults to a lazy
#'   \code{.alsi_load_db("flp")} (the French Lexicon Project word list); run
#'   \code{alsi_setup_databases("flp")} first, or pass your own lexicon.
#' @param word_col Name of the column in \code{lexicon} containing word forms.
#'   Default: \code{"word"} (matches FLP \code{dt_flp_words.Rds}).
#' @param content_upos Character vector of UPOS tags treated as content words.
#'   Default: \code{c("NOUN","VERB","ADJ","ADV")}.
#' @param max_len_gap Maximum character-length difference between an OOV token
#'   and a lexicon candidate before the candidate is excluded from the search.
#'   Default: \code{3L}.
#'
#' @returns A \code{data.frame} with one row per \code{doc_id}:
#' \describe{
#'   \item{oov_edit_prop}{Proportion of content-word tokens not found in the
#'     lexicon (by token or lemma).}
#'   \item{oov_edit_mean_dist}{Mean minimum edit distance across OOV tokens.
#'     \code{NA} when \code{oov_edit_prop == 0}.}
#'   \item{oov_edit_max_dist}{Maximum minimum edit distance among OOV tokens
#'     (worst-case outlier). \code{NA} when \code{oov_edit_prop == 0}.}
#'   \item{oov_edit_prop_isolated}{Proportion of OOV tokens with minimum edit
#'     distance \eqn{> 2} (no close orthographic neighbor in the lexicon).
#'     \code{NA} when \code{oov_edit_prop == 0}.}
#' }
#'
#' @references
#'   Yarkoni, T., Balota, D. A., & Yap, M. J. (2008). Moving beyond
#'   Coltheart's N: A new measure of orthographic similarity.
#'   \emph{Psychonomic Bulletin & Review}, 15(5), 971--979.
#'   \doi{10.3758/PBR.15.5.971}
#'
#'   Andrews, S. (1997). The effect of orthographic similarity on lexical
#'   retrieval: Resolving neighborhood conflicts.
#'   \emph{Psychonomic Bulletin & Review}, 4(4), 439--461.
#'   \doi{10.3758/BF03214334}
#' @export
add_oov_edit_features <- function(dt_corpus,
                                   lexicon       = .alsi_load_db("flp"),
                                   word_col      = "word",
                                   content_upos  = .norms_content_upos,
                                   max_len_gap   = 3L) {
  dt  <- setDT(copy(dt_corpus))
  lex <- .load_lexicon(lexicon, word_col)

  required <- c("doc_id", "token", "lemma", "upos", "compte")
  if (!all(required %in% names(dt)))
    stop("dt_corpus missing columns: ",
         paste(setdiff(required, names(dt)), collapse = ", "))

  dt_cw <- dt[compte == TRUE & upos %in% content_upos]
  dt_cw[, .token_lc := tolower(token)]
  dt_cw[, .lemma_lc := tolower(lemma)]

  lex_words <- unique(tolower(lex[[word_col]]))
  lex_words <- lex_words[!is.na(lex_words) & nzchar(lex_words)]
  lex_len   <- nchar(lex_words)

  dt_cw[, .is_oov := !(.token_lc %in% lex_words) &
                     !(.lemma_lc %in% lex_words)]

  oov_forms <- unique(dt_cw[.is_oov == TRUE, .token_lc])

  if (length(oov_forms) == 0L) {
    doc_ids <- unique(dt_cw$doc_id)
    result  <- data.table(
      doc_id                 = doc_ids,
      oov_edit_prop          = 0,
      oov_edit_mean_dist     = NA_real_,
      oov_edit_max_dist      = NA_real_,
      oov_edit_prop_isolated = NA_real_
    )
    return(as.data.frame(result))
  }

  # Min edit distance per unique OOV type (operates on types, not tokens).
  min_dist <- vapply(oov_forms, function(w) {
    w_len      <- nchar(w)
    candidates <- lex_words[abs(lex_len - w_len) <= max_len_gap]
    if (length(candidates) == 0L) return(as.numeric(w_len))
    as.numeric(min(adist(w, candidates)))
  }, numeric(1L))

  dt_dist <- data.table(.token_lc = oov_forms, .min_dist = min_dist)
  dt_cw   <- merge(dt_cw, dt_dist, by = ".token_lc", all.x = TRUE)

  result <- dt_cw[, {
    n_oov      <- sum(.is_oov)
    oov_dists  <- .min_dist[.is_oov == TRUE]
    .(
      oov_edit_prop          = n_oov / .N,
      oov_edit_mean_dist     = if (n_oov > 0L) mean(oov_dists)      else NA_real_,
      oov_edit_max_dist      = if (n_oov > 0L) max(oov_dists)       else NA_real_,
      oov_edit_prop_isolated = if (n_oov > 0L) mean(oov_dists > 2L) else NA_real_
    )
  }, by = doc_id]

  return(as.data.frame(result))
}
