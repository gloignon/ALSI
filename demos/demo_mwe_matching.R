# Demo: multi-word expression (MWE) matching on parsed data
#
# In this demo you will:
# 1) match simple hand-built expressions against a toy corpus;
# 2) verify that matches are isolated per document in a multi-document corpus.

# 0) Setup ----

library(data.table)
library(tidyverse)
source("R/fnt_corpus.R",  encoding = "UTF-8")
source("R/fnt_mwe.R",     encoding = "UTF-8")

udmodel_path <- "models/french_gsd-remix_3.udpipe"


# 1) Toy example: hand-built lexicon ----

# Parse a toy sentence directly — no manual tokenisation needed
toy_sentence <- "Il a réussi à se préparer afin de partir, mais au lieu de se diriger vers la gare, il a pris un taxi."

dt_toy <- parse_text(
  data.table(doc_id = "d1", text = toy_sentence),
  ud_model = udmodel_path, n_cores = 1, show_progress = FALSE, parser = "none"
)

# A tiny lexicon with 1-, 2-, and 3-word MWE entries
dt_lex_toy <- data.table(
  forme_lower = c("mais", "afin de", "au lieu de", "cependant", "il a"),
  n_tokens    = c(1L, 2L, 3L, 1L, 2L)  # optional, will be computed if missing using spaces as separator
)

# Find which MWEs are in my lexicon, and where they occur.
matches_toy <- match_multiword_sequences(dt_toy, dt_lex_toy)
print(matches_toy)


# 2) Multi-document toy example ----

# Two docs to verify per-document isolation (reuses dt_lex_toy)
dt_two_docs <- parse_text(
  data.table(
    doc_id = c("d1", "d2"),
    text   = c(
      "Il a réussi à partir, mais il a changé d'avis.",
      "Cependant, afin de mieux se préparer, il a tout relu."
    )
  ),
  ud_model = udmodel_path, n_cores = 1, show_progress = FALSE, parser = "none"
)

matches_two <- match_multiword_sequences(dt_two_docs, dt_lex_toy)
print(matches_two)
