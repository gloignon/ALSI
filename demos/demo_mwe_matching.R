# Demo: multi-word expression (MWE) matching on parsed data
#
# In this demo you will:
# 1) match simple hand-built expressions against a toy corpus;
# 2) match the LEXCONN connective database against the demo corpus;
# 3) compute connective density features per document;
# 4) compare Vikidia vs Wikipedia with effect sizes.
#
# Prerequisite:
# - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).
# - Run R/artefact_builders/fetch_lexconn.R first (creates lexical_dbs/dt_lexconn.Rds).

# 0) Setup ----

library(data.table)
library(tidyverse)
library(effsize)
source("R/fnt_corpus.R",  encoding = "UTF-8")
source("R/fnt_mwe.R",     encoding = "UTF-8")
source("R/fnt_utility.R", encoding = "UTF-8")

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


# 3) LEXCONN on the demo corpus: Vikidia vs Wikipedia ----
# Prerequisite: run demos/demo_parse_tag.R to create out/demo_parsed_tagged.Rds.

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")
dt_lexconn <- readRDS("lexical_dbs/dt_lexconn.Rds")

matches_vw <- match_multiword_sequences(dt_parsed_corpus, dt_lexconn)

# Most frequent connectives and distribution by relation type
matches_vw |> count(matched_forme, sort = TRUE) |> slice_head(n = 20) |> print()
matches_vw |> count(relation_group, sort = TRUE) |> print()

# Compare connective density between Vikidia and Wikipedia, using the matched connectives.
  # this will get us the doc_id and class of each document
dt_classes_vw <- dt_parsed_corpus |> distinct(doc_id, class = if_else(grepl("^viki", doc_id), 1L, 2L))

# now we can merge the class info with the features, and report effect sizes per feature.
connective_density_features(dt_parsed_corpus, matches_vw) |>
  merge(dt_classes_vw, by = "doc_id") |>
  report_effects("Vikidia vs Wikipedia")


# 4) LEXCONN on ALECTOR corpus: source vs target ----

dt_alector <- readRDS("out/alector_parsed.Rds")
message("ALECTOR corpus: ", nrow(dt_alector), " tokens, ", n_distinct(dt_alector$doc_id), " documents")

matches_alector <- match_multiword_sequences(dt_alector, dt_lexconn)
message("Matches: ", nrow(matches_alector))

dt_classes_al <- unique(data.table(
  doc_id = dt_alector$doc_id,
  class = ifelse(grepl("_target_", dt_alector$doc_id), 1L, 2L)
))

dt_features_al <- connective_density_features(dt_alector, matches_alector) |>
  merge(dt_classes_al, by = "doc_id")

report_effects(dt_features_al, "ALECTOR: target/simplified (1) vs source/original (2)")

# Boxplots (ALECTOR)
plot_faceted_boxplot(
  dt_features_al, class, ends_with("_per100w"),
  title = "Connective Density: ALECTOR target (1) vs source (2)",
  x_lab = "Class", y_lab = "Per 100 words"
)
