# Utility functions shared across demos and pipelines.
library(data.table)
library(tidyverse)
library(effsize)

# Print a sorted effect-size table for a set of per-100w density features,
# comparing two classes. dt_features must have a `class` column (values 1 and 2),
# a `word_count` column, and columns ending in `_per100w`.
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
