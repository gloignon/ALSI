# Word burstiness features
#
# References:
#   Altmann, Pierrehumbert & Motter (2009), PLoS ONE 4(11): e7678
#   Church & Gale (1995), Natural Language Engineering 1(2): 163–190
#   Goh & Barabási (2008), Europhys Lett 81: 48002
#
library(data.table, quietly = TRUE)

.replace_nan <- function(dt) {
  for (col in names(dt))
    if (is.numeric(dt[[col]])) dt[is.nan(get(col)), (col) := NA_real_]
  invisible(dt)
}

.prep_dt <- function(dt_corpus, doc_col, token_col, upos_col = NULL, lowercase = TRUE) {
  cols <- c(doc_col, token_col, upos_col)
  dt   <- copy(dt_corpus[, .SD, .SDcols = cols])
  setnames(dt, c(doc_col, token_col), c("doc_id", "token"))
  if (!is.null(upos_col)) setnames(dt, upos_col, "upos")
  if (lowercase) dt[, token := tolower(token)]
  dt
}

#' Compute Per-Word Burstiness (Weibull β) from a Corpus Token Stream
#'
#' For each word that appears at least \code{min_occurrences} times, fits a
#' Weibull (stretched-exponential) distribution to the sequence of
#' inter-occurrence distances (gap in tokens between successive uses of the
#' same word).  Returns the shape parameter β: β = 1 is a Poisson (random)
#' process; β < 1 indicates burstiness (content words); β close to 1
#' indicates near-uniform distribution (function words).
#'
#' The Weibull MLE for β is obtained via \code{uniroot} on the profile score
#' equation (scale concentrated out), which is strictly monotone and avoids
#' the divergence problems of unconstrained Newton–Raphson.
#'
#' Reference: Altmann, Pierrehumbert & Motter (2009), PLoS ONE 4(11): e7678.
#'
#' @param dt_corpus A \code{data.table} with at least a \code{token} column
#'   giving the full corpus token stream in order.
#' @param token_col Name of the token column. Default \code{"token"}.
#' @param min_occurrences Minimum occurrences required to attempt fitting.
#'   Words below this threshold are returned with \code{NA}. Default 20.
#' @param lowercase Logical; lowercase tokens before computing. Default TRUE.
#' @returns A \code{data.table} with columns \code{word}, \code{n_occ},
#'   \code{mean_gap}, \code{beta}, \code{beta_se}, sorted by \code{beta}
#'   ascending (most bursty first).
compute_burstiness_beta <- function(dt_corpus,
                                    token_col       = "token",
                                    min_occurrences = 20L,
                                    lowercase       = TRUE) {
  stopifnot(is.data.table(dt_corpus), token_col %in% names(dt_corpus))

  tokens <- dt_corpus[[token_col]]
  if (lowercase) tokens <- tolower(tokens)

  keep   <- !is.na(tokens) & nzchar(tokens)
  tokens <- tokens[keep]

  N        <- length(tokens)
  pos_list <- split(seq_len(N), tokens)

  .fit_weibull_beta <- function(pos) {
    if (length(pos) < 2L) return(c(beta = NA_real_, se = NA_real_))
    gaps <- diff(pos)
    gaps <- gaps[gaps > 0]
    n    <- length(gaps)
    if (n < 2L) return(c(beta = NA_real_, se = NA_real_))

    # Degenerate case: all gaps identical → β → ∞ (point mass, not Weibull)
    if (var(gaps) == 0) return(c(beta = NA_real_, se = NA_real_))

    # Profile score for Weibull shape β (scale concentrated out):
    #   S(β) = n/β + Σlog(t_i) - n * [Σ(t_i^β log t_i) / Σ(t_i^β)]
    # S(β) is strictly decreasing in β, so uniroot is reliable.
    lg     <- log(gaps)
    sum_lg <- sum(lg)
    score  <- function(b) {
      tb <- gaps^b
      n / b + sum_lg - n * sum(tb * lg) / sum(tb)
    }

    beta <- tryCatch(
      uniroot(score, interval = c(1e-4, 20), tol = 1e-8)$root,
      error = function(e) NA_real_
    )
    if (is.na(beta)) return(c(beta = NA_real_, se = NA_real_))

    tb <- gaps^beta
    s0 <- sum(tb); s1 <- sum(tb * lg); s2 <- sum(tb * lg^2)  # Σt^β, Σt^β·log t, Σt^β·(log t)²
    info <- n / beta^2 - n * (s2 / s0 - (s1 / s0)^2)
    se   <- if (is.finite(info) && info > 0) 1 / sqrt(info) else NA_real_
    c(beta = beta, se = se)
  }

  out <- rbindlist(Map(function(w, pos) {
    n_occ    <- length(pos)
    mean_gap <- if (n_occ >= 2L) mean(diff(pos)) else NA_real_
    if (n_occ < min_occurrences)
      return(data.table(word = w, n_occ = n_occ, mean_gap = mean_gap,
                        beta = NA_real_, beta_se = NA_real_))
    fit <- .fit_weibull_beta(pos)
    data.table(word = w, n_occ = n_occ, mean_gap = mean_gap,
               beta = fit[["beta"]], beta_se = fit[["se"]])
  }, names(pos_list), pos_list))
  setorder(out, beta)
  invisible(out)
}


#' Compute Per-Word Adaptation Scores from Document-Level Counts
#'
#' Estimates \eqn{\Pr(x \geq 2 \mid x \geq 1)} under a Negative Binomial
#' model (Gamma mixture of Poissons) for each word, following Church & Gale
#' (1995).  The adaptation ratio \code{p_x2_given_x1 / p_x1} indexes
#' document-level burstiness: values >> 1 indicate topic words that strongly
#' recur within a document once seen; values near 1 indicate near-Poisson
#' (function word) behaviour.
#'
#' NB parameters are estimated via method of moments from the corpus-level
#' mean and variance of per-document counts.
#'
#' Reference: Church & Gale (1995), Natural Language Engineering 1(2): 163–190.
#'
#' @param dt_corpus A \code{data.table} with columns \code{doc_id} and
#'   \code{token}.
#' @param doc_col Name of the document-id column. Default \code{"doc_id"}.
#' @param token_col Name of the token column. Default \code{"token"}.
#' @param min_docs Minimum documents required. Words in fewer documents are
#'   returned with \code{NA}. Default 10.
#' @param lowercase Logical; lowercase tokens. Default TRUE.
#' @returns A \code{data.table} with columns \code{word}, \code{n_docs},
#'   \code{mean_count}, \code{var_count}, \code{nb_r}, \code{nb_p},
#'   \code{p_x1}, \code{p_x2_given_x1}, \code{adaptation}, sorted by
#'   \code{adaptation} descending.
compute_adaptation_score <- function(dt_corpus,
                                     doc_col   = "doc_id",
                                     token_col = "token",
                                     min_docs  = 10L,
                                     lowercase = TRUE) {
  stopifnot(is.data.table(dt_corpus),
            doc_col   %in% names(dt_corpus),
            token_col %in% names(dt_corpus))

  dt <- .prep_dt(dt_corpus, doc_col, token_col, lowercase = lowercase)

  n_docs_total <- uniqueN(dt$doc_id)

  counts     <- dt[, .N, by = .(token, doc_id)]
  setnames(counts, "N", "cnt")

  word_stats <- counts[, .(
    n_docs   = .N,
    sum_cnt  = sum(cnt),
    sum_cnt2 = sum(cnt^2)
  ), by = token]

  word_stats[, `:=`(
    mean_count = sum_cnt  / n_docs_total,
    var_count  = sum_cnt2 / n_docs_total - (sum_cnt / n_docs_total)^2
  )]

  word_stats[, `:=`(
    nb_r = fifelse(var_count > mean_count,
                   mean_count^2 / (var_count - mean_count), NA_real_),
    nb_p = fifelse(var_count > mean_count,
                   mean_count  / var_count,                 NA_real_)
  )]

  word_stats[n_docs >= min_docs & !is.na(nb_r),
    p_x1 := 1 - pnbinom(0L, size = nb_r, prob = 1 - nb_p)]
  word_stats[n_docs >= min_docs & !is.na(nb_r),
    p_x2_given_x1 := (1 - pnbinom(1L, size = nb_r, prob = 1 - nb_p)) / p_x1]

  word_stats[is.na(nb_r) | n_docs < min_docs, `:=`(
    p_x1          = NA_real_,
    p_x2_given_x1 = NA_real_
  )]

  word_stats[, adaptation := p_x2_given_x1 / p_x1]

  out <- word_stats[, .(word = token, n_docs, mean_count, var_count,
                        nb_r, nb_p, p_x1, p_x2_given_x1, adaptation)]
  setorder(out, -adaptation, na.last = TRUE)
  invisible(out)
}


#' Document-Level Burstiness Features
#'
#' Aggregates word-level Weibull β and Negative Binomial adaptation scores to
#' one row per document.
#'
#' By default word-level norms are derived from \code{dt_corpus} itself.
#' Supply \code{ref_corpus} to use an external reference — each document then
#' scores as a pure lookup with no information shared across target documents.
#' This is the recommended approach for group comparisons: establish norms on
#' a neutral reference (e.g. Wikipedia), score each document independently.
#'
#' @param dt_corpus \code{data.table} with \code{doc_id}, \code{token}, and
#'   optionally \code{upos}. The documents to score.
#' @param ref_corpus Optional \code{data.table} used as the reference for
#'   computing word-level norms. Must contain the same \code{doc_col} /
#'   \code{token_col} columns. Default \code{NULL} (use \code{dt_corpus}).
#' @param doc_col Name of the document-id column. Default \code{"doc_id"}.
#' @param token_col Name of the token column. Default \code{"token"}.
#' @param upos_col Name of the UPOS column. Default \code{"upos"}.
#' @param content_upos UPOS tags treated as content words.
#'   Default \code{c("NOUN","VERB","ADJ","ADV")}.
#' @param min_occurrences Passed to \code{compute_burstiness_beta()}.
#'   Default 20.
#' @param min_docs Passed to \code{compute_adaptation_score()}. Default 10.
#' @param lowercase Logical; lowercase tokens. Default TRUE.
#' @param return_word_tables If TRUE, attaches word-level tables as attributes
#'   \code{"beta_tbl"} and \code{"adapt_tbl"} on the result. Default FALSE.
#' @returns A \code{data.table} with one row per document and columns:
#'   \code{mean_beta}, \code{median_beta}, \code{prop_bursty},
#'   \code{mean_adaptation}, \code{median_adaptation}, and (if upos available)
#'   \code{mean_beta_content}, \code{prop_bursty_content},
#'   \code{mean_adaptation_content}.
burstiness_doc_features <- function(dt_corpus,
                                    ref_corpus          = NULL,
                                    doc_col             = "doc_id",
                                    token_col           = "token",
                                    upos_col            = "upos",
                                    content_upos        = c("NOUN","VERB","ADJ","ADV"),
                                    min_occurrences     = 20L,
                                    min_docs            = 10L,
                                    burstiness_threshold = 0.6,
                                    lowercase           = TRUE,
                                    return_word_tables  = FALSE) {
  stopifnot(is.data.table(dt_corpus),
            doc_col   %in% names(dt_corpus),
            token_col %in% names(dt_corpus))

  have_upos <- !is.null(upos_col) && upos_col %in% names(dt_corpus)
  ref       <- if (!is.null(ref_corpus)) ref_corpus else dt_corpus

  # ── 1. Word-level norms from reference ──────────────────────────────────────
  beta_tbl  <- compute_burstiness_beta(ref,
                                       token_col       = token_col,
                                       min_occurrences = min_occurrences,
                                       lowercase       = lowercase)
  adapt_tbl <- compute_adaptation_score(ref,
                                        doc_col   = doc_col,
                                        token_col = token_col,
                                        min_docs  = min_docs,
                                        lowercase = lowercase)

  word_props <- merge(beta_tbl[,  .(word, beta)],
                      adapt_tbl[, .(word, adaptation)],
                      by = "word", all = TRUE)

  # ── 2. Join onto target corpus ───────────────────────────────────────────────
  dt <- .prep_dt(dt_corpus, doc_col, token_col,
                 upos_col = if (have_upos) upos_col else NULL,
                 lowercase = lowercase)
  dt <- merge(dt, word_props, by.x = "token", by.y = "word", all.x = TRUE)

  # ── 3. Aggregate per document ────────────────────────────────────────────────
  result <- dt[, .(
    mean_beta         = mean(beta,       na.rm = TRUE),
    median_beta       = median(beta,     na.rm = TRUE),
    prop_bursty       = mean(beta < burstiness_threshold, na.rm = TRUE),
    mean_adaptation   = mean(adaptation, na.rm = TRUE),
    median_adaptation = median(adaptation, na.rm = TRUE)
  ), by = doc_id]

  if (have_upos) {
    cw <- dt[upos %in% content_upos, .(
      mean_beta_content       = mean(beta,       na.rm = TRUE),
      prop_bursty_content     = mean(beta < burstiness_threshold, na.rm = TRUE),
      mean_adaptation_content = mean(adaptation, na.rm = TRUE)
    ), by = doc_id]
    result <- merge(result, cw, by = "doc_id", all.x = TRUE)
  }

  .replace_nan(result)
  setnames(result, "doc_id", doc_col)
  setorderv(result, doc_col)

  if (return_word_tables) {
    attr(result, "beta_tbl")  <- beta_tbl
    attr(result, "adapt_tbl") <- adapt_tbl
  }
  invisible(result)
}


#' Within-Document Burstiness Features (document-blind)
#'
#' Computes the Goh–Barabási burstiness parameter \emph{B} for each word
#' within each document independently — no information from other documents
#' is used.
#'
#' \deqn{B = \frac{\sigma_\tau - \mu_\tau}{\sigma_\tau + \mu_\tau}}
#'
#' B = 0 for Poisson, B > 0 bursty, B < 0 periodic/regular.  At least
#' \code{min_gaps} gaps (min_gaps + 1 occurrences) per word are required.
#'
#' Note: short documents (~500 tokens) typically yield B < 0 for repeating
#' words because repeated vocabulary in a focused essay is spread more evenly
#' than Poisson.  This measure is more informative on long texts or corpora.
#'
#' Reference: Goh & Barabási (2008), Europhys Lett 81: 48002.
#'
#' @param dt_corpus \code{data.table} with \code{doc_id}, \code{token}, and
#'   optionally \code{upos}.
#' @param doc_col Name of the document-id column. Default \code{"doc_id"}.
#' @param token_col Name of the token column. Default \code{"token"}.
#' @param upos_col Name of the UPOS column. Default \code{"upos"}.
#' @param content_upos UPOS tags treated as content words.
#' @param min_gaps Minimum gaps per word per document (= min occurrences − 1).
#'   Default 2.
#' @param lowercase Logical; lowercase tokens. Default TRUE.
#' @returns A \code{data.table} with columns \code{mean_B}, \code{sd_B},
#'   \code{n_words_B}, and (if upos available) \code{mean_B_content},
#'   \code{sd_B_content}, \code{n_words_B_content}.
burstiness_within_doc <- function(dt_corpus,
                                  doc_col      = "doc_id",
                                  token_col    = "token",
                                  upos_col     = "upos",
                                  content_upos = c("NOUN","VERB","ADJ","ADV"),
                                  min_gaps     = 2L,
                                  lowercase    = TRUE) {
  stopifnot(is.data.table(dt_corpus),
            doc_col   %in% names(dt_corpus),
            token_col %in% names(dt_corpus))

  have_upos <- !is.null(upos_col) && upos_col %in% names(dt_corpus)

  dt <- .prep_dt(dt_corpus, doc_col, token_col,
                 upos_col = if (have_upos) upos_col else NULL,
                 lowercase = lowercase)
  dt <- dt[!is.na(token) & nzchar(token)]
  dt[, pos := seq_len(.N), by = doc_id]

  .B <- function(gaps) {
    if (length(gaps) < min_gaps) return(NA_real_)
    mu <- mean(gaps); s <- sd(gaps)  # μ_τ, σ_τ from Goh & Barabási eq. 1
    if ((s + mu) == 0) return(NA_real_)
    (s - mu) / (s + mu)
  }

  word_doc_B <- dt[, {
    gaps <- diff(pos)
    .(B          = .B(gaps),
      is_content = if (have_upos) upos[1L] %in% content_upos else NA)
  }, by = .(doc_id, token)]

  result <- word_doc_B[!is.na(B), .(
    mean_B    = mean(B),
    sd_B      = sd(B),
    n_words_B = .N
  ), by = doc_id]

  if (have_upos) {
    cw <- word_doc_B[!is.na(B) & is_content == TRUE, .(
      mean_B_content    = mean(B),
      sd_B_content      = sd(B),
      n_words_B_content = .N
    ), by = doc_id]
    result <- merge(result, cw, by = "doc_id", all.x = TRUE)
  }

  .replace_nan(result)
  setnames(result, "doc_id", doc_col)
  setorderv(result, doc_col)
  invisible(result)
}
