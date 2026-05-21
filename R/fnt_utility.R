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
  tokens <- gregexpr("[[:alpha:]À-ɏ]+", text, perl = TRUE)
  vapply(tokens, function(m) sum(m > 0L), integer(1L))
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
#' @param title Plot title passed to \code{labs()}.
#' @param x_lab,y_lab Axis labels (default \code{NULL} suppresses the label).
#' @param ncol Number of columns in \code{facet_wrap()} (default \code{NULL}
#'   lets ggplot choose).
#' @param notch Logical; whether to draw notched boxplots (default \code{FALSE}).
#' @param show_d Logical; whether to annotate each facet with Cohen's \emph{d}
#'   (default \code{TRUE}).
#' @param d_size Text size for the Cohen's \emph{d} label (default \code{3.5}).
#' @returns A \code{ggplot} object.
plot_faceted_boxplot <- function(df,
                                 group_col,
                                 feature_cols,
                                 title    = NULL,
                                 x_lab    = NULL,
                                 y_lab    = "Value",
                                 ncol     = NULL,
                                 notch    = FALSE,
                                 show_d   = TRUE,
                                 d_size   = 2.8) {
  group_col <- rlang::ensym(group_col)

  df_long <- df |>
    select(!!group_col, {{ feature_cols }}) |>
    pivot_longer({{ feature_cols }}, names_to = "feature", values_to = "value") |>
    mutate(group = as.factor(!!group_col))

  p <- ggplot(df_long, aes(x = group, y = value, fill = group)) +
    geom_boxplot(notch = notch, outlier.size = 0.8) +
    facet_wrap(~ feature, scales = "free_y", ncol = ncol) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_minimal() +
    theme(legend.position = "none")

  if (show_d) {
    groups <- levels(df_long$group)
    d_labels <- df_long |>
      summarise(
        d = effsize::cohen.d(
          value[group == groups[1]],
          value[group == groups[2]]
        )$estimate,
        .by = feature
      ) |>
      mutate(label = paste0("d = ", round(d, 2)))

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
