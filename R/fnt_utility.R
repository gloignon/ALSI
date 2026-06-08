# Utility functions shared across demos and pipelines.
library(data.table)
library(tidyverse)
library(effsize)

#' Report Effect Sizes for Per-100-Word Features
#'
#' Prints a sorted summary table for columns ending in \code{"_per100w"},
#' comparing two classes with Pearson correlation and Cohen's \emph{d}. When
#' \code{word_count} is present, also reports each feature's correlation with
#' document length.
#'
#' @param dt_features A data.frame or \code{data.table} with a \code{class}
#'   column, where class values \code{1} and \code{2} define the two groups to
#'   compare, plus one or more numeric columns ending in \code{"_per100w"}.
#'   If present, \code{word_count} is used for the length-correlation column.
#' @param label Character label printed as the section title.
#' @returns The printed tibble is returned invisibly by \code{print()}.
report_effects <- function(dt_features, label) {
  has_length <- "word_count" %in% names(dt_features)
  cat("\n===", label, "===\n\n")
  dt_features |>
    select(ends_with("_per100w"), class, any_of("word_count")) |>
    pivot_longer(ends_with("_per100w"), names_to = "feature", values_to = "value") |>
    summarise(
      r           = cor(value, class, use = "complete.obs"),
      cohen_d     = cohen.d(value[class == 1], value[class == 2])$estimate,
      corr_length = if (has_length) cor(value, word_count, use = "complete.obs") else NA_real_,
      .by = feature
    ) |>
    arrange(desc(abs(cohen_d))) |>
    mutate(across(where(is.numeric), ~ round(.x, 2))) |>
    print()
}

#' Count words in a string
#'
#' Splits on whitespace and punctuation, so contractions and elisions count
#' as two words (\emph{l'avion} → 2) and comma-separated items count
#' individually (\emph{Bonjour, Alex} → 2).
#'
#' @param text A character vector.
#' @returns An integer vector of word counts, one per element of \code{text}.
count_words <- function(text) {
  matches <- gregexpr("[[:alpha:]À-ɏ]+", text, perl = TRUE)
  return(lengths(regmatches(text, matches)))
}


#' Split text into sentences
#'
#' Splits on sentence-ending punctuation followed by whitespace and an
#' uppercase letter (including French accented capitals).
#'
#' @param text A character string.
#' @returns A character vector of trimmed, non-empty sentences.
split_sentences <- function(text) {
  s <- unlist(strsplit(text, "(?<=[.!?])\\s+(?=[A-Z\u00c0\u00c2\u00c9\u00c8\u00ca\u00cb\u00ce\u00cf\u00d4\u00d9\u00db\u00dc\u00c7])", perl = TRUE))
  trimws(s[nchar(trimws(s)) > 0])
}

#' Faceted Boxplot with Optional Cohen's d Annotation
#'
#' Takes a wide data frame (one row per document, one column per feature),
#' pivots it to long format, and draws a faceted boxplot comparing two groups.
#' Optionally overlays each facet with the Cohen's \emph{d} effect size.
#'
#' @param df A data frame with a grouping column and one or more numeric feature
#'   columns to compare.
#' @param group_col Name of the grouping column (unquoted or as a string).
#'   Must have exactly two levels.
#' @param feature_cols A \code{<tidy-select>} expression for the feature columns
#'   to plot (e.g. \code{ends_with("_per100w")} or \code{c(feat_a, feat_b)}).
#'   If \code{NULL} (default), all numeric columns except \code{group_col} and
#'   \code{pair_col} are used.
#' @param feature_labels Optional named character vector mapping feature column
#'   names to human-readable facet titles (e.g.
#'   \code{c(mean_beta = "Mean β (all)")}). Columns absent from the vector keep
#'   their raw name. Default \code{NULL} uses raw column names as facet titles.
#' @param title Plot title passed to \code{labs()}.
#' @param x_lab,y_lab Axis labels (default \code{NULL} suppresses the label).
#' @param ncol Number of columns in \code{facet_wrap()} (default \code{3}).
#' @param notch Logical; whether to draw notched boxplots (default \code{FALSE}).
#' @param show_d Logical; whether to annotate each facet with Cohen's \emph{d}
#'   (default \code{TRUE}).
#' @param d_size Text size for the Cohen's \emph{d} label (default \code{3.5}).
#' @param paired Logical; if \code{TRUE}, compute paired Cohen's \emph{d}
#'   (appropriate for matched pairs such as Wikipedia–Vikidia article pairs).
#'   Default \code{FALSE}.
#' @param pair_col Optional name of the column identifying matched pairs
#'   (unquoted or as a string). When \code{paired = TRUE} and \code{pair_col}
#'   is \code{NULL}, rows are matched by their position within each group (first
#'   row of group 1 pairs with first row of group 2, etc.).
#' @returns A \code{ggplot} object.
plot_faceted_boxplot <- function(df,
                                 group_col,
                                 feature_cols   = NULL,
                                 feature_labels = NULL,
                                 title    = NULL,
                                 x_lab    = NULL,
                                 y_lab    = "Value",
                                 ncol     = 3,
                                 notch    = FALSE,
                                 show_d   = TRUE,
                                 d_size   = 2.8,
                                 paired   = FALSE,
                                 pair_col = NULL) {
  group_col <- rlang::ensym(group_col)

  exclude_cols <- c(as.character(group_col), pair_col)
  if (is.null(feature_cols)) {
    feat_names <- df |>
      select(-any_of(exclude_cols)) |>
      select(where(is.numeric)) |>
      names()
  } else {
    feat_names <- df |>
      select({{ feature_cols }}) |>
      names()
  }

  if (paired && is.null(pair_col)) {
    stop("`pair_col` must be supplied when `paired = TRUE`.")
  }

  df_long <- df |>
    select(!!group_col, any_of(feat_names), any_of(as.character(pair_col))) |>
    pivot_longer(any_of(feat_names), names_to = "feature", values_to = "value") |>
    mutate(group = as.factor(!!group_col))

  facet_labeller <- if (is.null(feature_labels)) {
    ggplot2::label_value
  } else {
    ggplot2::as_labeller(feature_labels)
  }

  p <- ggplot(df_long, aes(x = group, y = value, fill = group)) +
    geom_boxplot(notch = notch, outlier.size = 0.8) +
    facet_wrap(~ feature, scales = "free_y", ncol = ncol,
               labeller = facet_labeller) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_minimal() +
    theme(legend.position = "none")

  if (show_d) {
    groups <- levels(df_long$group)

    if (paired) {
      pair_sym <- rlang::sym(pair_col)
      d_labels <- df_long |>
        select(feature, group, !!pair_sym, value) |>
        pivot_wider(names_from = group, values_from = value) |>
        summarise(
          d = {
            diffs <- .data[[groups[1]]] - .data[[groups[2]]]
            mean(diffs, na.rm = TRUE) / sd(diffs, na.rm = TRUE)
          },
          .by = feature
        ) |>
        mutate(label = paste0("d = ", round(d, 2)))
    } else {
      d_labels <- df_long |>
        summarise(
          d = effsize::cohen.d(
            value[group == groups[1]],
            value[group == groups[2]]
          )$estimate,
          .by = feature
        ) |>
        mutate(label = paste0("d = ", round(d, 2)))
    }

    p <- p +
      geom_text(
        data      = d_labels,
        aes(label = label),
        x         = 1.5, y = Inf,
        hjust     = 0.5, vjust = 1.4,
        size      = d_size,
        inherit.aes = FALSE
      )
  }

  return(p)
}

#' Read a text file and return one row per sentence
#'
#' Reads a text file, collapses lines, splits into sentences, and returns
#' a tibble. Handles both one-sentence-per-line and single-blob formats.
#'
#' @param path Path to a text file (UTF-8).
#' @param doc_id Document identifier to assign.
#' @returns A \code{tibble} with \code{doc_id}, \code{sentence_id}, and
#'   \code{sentence}, or NULL if no sentences are found.
#' Compare two groups across a set of features
#'
#' For each feature, computes Cohen's d and a Wilcoxon p-value between two
#' groups. Supports paired designs via \code{pair_col}.
#'
#' @param df A data frame with one row per document.
#' @param grp_col Name of the column that identifies the group.
#' @param grp_a,grp_b Values of \code{grp_col} to compare.
#' @param feat_cols Character vector of feature column names.
#' @param corpus_label Label for the \code{corpus} column in the output.
#' @param paired Logical; if TRUE uses paired Cohen's d and Wilcoxon test.
#' @param pair_col Column name used to align pairs (required when \code{paired = TRUE}).
#' @param strip_prefix Optional string prefix to remove from feature names in output.
#' @returns A tibble with columns \code{corpus}, \code{feature}, \code{d}, \code{p}.
compare_groups <- function(df, grp_col, grp_a, grp_b, feat_cols,
                           corpus_label, paired = FALSE, pair_col = NULL,
                           strip_prefix = NULL) {
  purrr::map_dfr(feat_cols, function(f) {
    if (paired) {
      wide <- df |>
        select(all_of(c(pair_col, grp_col, f))) |>
        pivot_wider(names_from = all_of(grp_col), values_from = all_of(f))
      x_a <- wide[[grp_a]]
      x_b <- wide[[grp_b]]
      ok  <- is.finite(x_a) & is.finite(x_b)
      x_a <- x_a[ok]
      x_b <- x_b[ok]
    } else {
      x_a <- df |> filter(.data[[grp_col]] == grp_a) |> pull(all_of(f))
      x_b <- df |> filter(.data[[grp_col]] == grp_b) |> pull(all_of(f))
      x_a <- x_a[is.finite(x_a)]
      x_b <- x_b[is.finite(x_b)]
    }

    feat_label <- if (!is.null(strip_prefix)) str_remove(f, strip_prefix) else f

    if (length(x_a) < 2) {
      return(tibble(corpus = corpus_label, feature = feat_label,
                    d = NA_real_, p = NA_real_))
    }

    tibble(
      corpus  = corpus_label,
      feature = feat_label,
      d = effsize::cohen.d(x_a, x_b, paired = paired)$estimate,
      p = wilcox.test(x_a, x_b, paired = paired)$p.value
    )
  })
}

read_sentences <- function(path, doc_id) {
  raw <- readLines(path, encoding = "UTF-8", warn = FALSE)
  sents <- raw |>
    paste(collapse = " ") |>
    split_sentences()
  if (length(sents) == 0) return(NULL)
  tibble::tibble(doc_id = doc_id,
                 sentence_id = seq_along(sents),
                 sentence = sents)
}


#' Cohen's kappa for two binary raters
#'
#' General-purpose intercoder reliability for any long-format coding table with
#' a grouping variable (e.g. code category). Commonly used to validate
#' LLM-assigned codes against a human gold standard; see also
#' \code{\link{code_corpus}} in \file{fnt_qualitative_coding.R}.
#'
#' @param dt_rater1 \code{data.table} with columns \code{item_id},
#'   \code{group} (e.g. code name), and \code{applies} (logical or 0/1).
#' @param dt_rater2 \code{data.table} with the same columns, representing the
#'   second rater (e.g. human gold standard).
#' @param item_col  Name of the item identifier column (default \code{"passage_id"}).
#' @param group_col Name of the grouping column (default \code{"code"}).
#' @param value_col Name of the binary rating column (default \code{"applies"}).
#' @returns A \code{data.table} with columns \code{group}, \code{kappa},
#'   \code{p_observed}, and \code{n}, sorted by descending kappa.
compute_kappa <- function(dt_rater1, dt_rater2,
                          item_col  = "passage_id",
                          group_col = "code",
                          value_col = "applies") {
  r1 <- data.table::copy(data.table::as.data.table(dt_rater1))
  r2 <- data.table::copy(data.table::as.data.table(dt_rater2))

  data.table::setnames(r1, c(item_col, group_col, value_col), c("item", "group", "r1"))
  data.table::setnames(r2, c(item_col, group_col, value_col), c("item", "group", "r2"))

  merged <- data.table::merge.data.table(
    r1[!is.na(r1), .(item, group, r1 = as.integer(r1))],
    r2[!is.na(r2), .(item, group, r2 = as.integer(r2))],
    by = c("item", "group")
  )

  result <- merged[
    , {
        n   <- .N
        p_o <- sum(r1 == r2) / n
        p_e <- (sum(r1) / n) * (sum(r2) / n) +
               (sum(!r1) / n) * (sum(!r2) / n)
        k   <- if (p_e == 1) NA_real_ else (p_o - p_e) / (1 - p_e)
        .(kappa = k, p_observed = p_o, n = n)
      },
    by = group
  ]

  data.table::setnames(result, "group", group_col)
  return(result[order(-kappa)])
}


#' Stratified k-fold cross-validation
#'
#' Assigns documents to folds (stratified by \code{strata_col} so each fold
#' contains a proportional mix of groups), then calls \code{fit_fn} and
#' \code{predict_fn} on each train/test split and returns the pooled
#' out-of-fold predictions.
#'
#' @param df A data frame with one row per document.
#' @param strata_col Name of the column to stratify on (character).
#' @param k Number of folds (default 5).
#' @param fit_fn A function \code{function(train_df)} that returns a fitted
#'   model object.
#' @param predict_fn A function \code{function(model, test_df)} that returns
#'   a vector of predictions, one per row of \code{test_df}.
#' @param seed Integer random seed for reproducibility (default 42).
#' @returns A tibble with one row per document containing \code{.row_id}
#'   (original row index), \code{.fold}, and \code{.pred} (the out-of-fold
#'   prediction).
cv_predict <- function(df, strata_col, k = 5L,
                       fit_fn, predict_fn, seed = 42L) {
  set.seed(seed)
  df <- df |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::group_by(.data[[strata_col]]) |>
    dplyr::mutate(.fold = sample(rep(seq_len(k), length.out = dplyr::n()))) |>
    dplyr::ungroup()

  purrr::map_dfr(seq_len(k), function(fold_k) {
    train <- dplyr::filter(df, .fold != fold_k)
    test  <- dplyr::filter(df, .fold == fold_k)
    model <- fit_fn(train)
    preds <- predict_fn(model, test)
    tibble::tibble(.row_id = test$.row_id, .fold = fold_k, .pred = preds)
  })
}


#' Quadratic Weighted Kappa
#'
#' Computes QWK between two integer vectors of ordinal ratings. Disagreements
#' are penalised quadratically: being off by 2 levels costs four times as much
#' as being off by 1. Used in automated essay scoring to evaluate how well a
#' predicted ordering matches human-assigned levels.
#'
#' @param true Integer vector of true labels in \code{[1, n_levels]}.
#' @param pred Integer vector of predicted labels in \code{[1, n_levels]}.
#' @param n_levels Number of ordinal levels (default 5).
#' @returns A single numeric value in \code{(-Inf, 1]}.
qwk <- function(true, pred, n_levels = 5L) {
  wt    <- outer(seq_len(n_levels), seq_len(n_levels),
                 function(i, j) (i - j)^2 / (n_levels - 1)^2)
  tab_o <- table(factor(true, seq_len(n_levels)), factor(pred, seq_len(n_levels)))
  tab_e <- outer(rowSums(tab_o), colSums(tab_o)) / sum(tab_o)
  return(1 - sum(wt * tab_o) / sum(wt * tab_e))
}


#' Find parent tokens that have a qualifying child dependency.
#'
#' Scans \code{dep_rel} in a parsed corpus for children matching \code{dep_rels}
#' (and optionally \code{lemma_filter} / \code{child_upos}), then returns the
#' unique sentence-scoped identifiers of their head tokens. Callers join the
#' result back to flag or filter specific tokens.
#'
#' The sentence key is built from all of \code{doc_id}, \code{paragraph_id},
#' \code{sentence_id} that are present in \code{dt_corpus}.
#'
#' @param dt_corpus Parsed data.table (UDPipe output after post-processing).
#'   Required columns: \code{doc_id}, \code{sentence_id}, \code{token_id},
#'   \code{head_token_id}, \code{dep_rel}, \code{compte}.
#' @param dep_rels Character vector of \code{dep_rel} values to match on
#'   children (e.g. \code{c("aux", "aux:pass")}).
#' @param lemma_filter Optional character vector. When supplied and a
#'   \code{lemma} column is present, only children whose lowercased lemma is
#'   in \code{lemma_filter} are considered.
#' @param child_upos Optional character vector of UPOS values; only children
#'   with matching UPOS are considered.
#' @param only_counted Logical; if \code{TRUE} (default) only children where
#'   \code{compte == TRUE} are considered.
#'
#' @returns A data.table keyed on the sentence-key columns plus \code{token_id},
#'   containing one row per unique parent token that has at least one qualifying
#'   child. Merge this back onto your token table to flag or filter parents.
#'
#' @examples
#' \dontrun{
#' # Flag VERB tokens that govern an être/avoir auxiliary child.
#' aux_heads <- dep_child_flags(
#'   dt_corpus,
#'   dep_rels     = c("aux", "aux:tense", "aux:pass"),
#'   lemma_filter = c("être", "etre", "avoir")
#' )
#' setkeyv(dt_verbs, c("doc_id", "sentence_id", "token_id"))
#' dt_verbs[aux_heads, is_compound_head := TRUE]
#' }
dep_child_flags <- function(dt_corpus,
                             dep_rels,
                             lemma_filter = NULL,
                             child_upos   = NULL,
                             only_counted = TRUE) {
  dt <- as.data.table(dt_corpus)

  required <- c("doc_id", "sentence_id", "token_id", "head_token_id",
                "dep_rel", "compte")
  if (!all(required %in% names(dt)))
    stop("dt_corpus missing columns: ",
         paste(setdiff(required, names(dt)), collapse = ", "))

  dt_ch <- if (only_counted) dt[compte == TRUE] else dt
  dt_ch <- dt_ch[dep_rel %in% dep_rels]

  if (!is.null(lemma_filter) && "lemma" %in% names(dt_ch))
    dt_ch <- dt_ch[tolower(lemma) %in% tolower(lemma_filter)]

  if (!is.null(child_upos))
    dt_ch <- dt_ch[upos %in% child_upos]

  sent_cols <- intersect(c("doc_id", "paragraph_id", "sentence_id"), names(dt))
  result    <- unique(dt_ch[, c(sent_cols, "head_token_id"), with = FALSE])
  setnames(result, "head_token_id", "token_id")
  setkeyv(result, c(sent_cols, "token_id"))
  return(result)
}
