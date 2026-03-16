# Utility functions shared across demos and pipelines.

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
