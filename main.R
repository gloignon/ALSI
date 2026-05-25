# ALSI / ILSA
#
# Analyseur Lexicosyntaxique intégré
# Integrated lexicosyntactic analyzer
#
# Author : Guillaume Loignon <loignon.guillaume@uqam.ca>
# Last update: 2026-05-25
#
# ---------------------------------------------------------------------------
# This script is the main demo pipeline for ALSI. It runs every feature
# family on the bundled Vikidia/Wikipedia demo corpus and saves the results
# to out/. It is intended as both a working example and a vignette-style
# walkthrough of ALSI's capabilities.
#
# Sections 1–14 require only R packages (listed below).
# Sections 15–16 (embeddings and LLM surprisal) additionally require Python.
#   - Python is managed automatically via reticulate::py_require(); no manual
#     venv setup is needed.
#   - If Python is not available on your machine those sections are skipped
#     with a warning.
#
# Prerequisites:
#   R packages : data.table, udpipe, tidyverse, utf8, zoo, future,
#                future.apply, reticulate, effsize
#   Models     : models/french_gsd-remix_2.udpipe
#                models/pos_trigram_fr_gsd_alsi.Rds
#   Lexical DBs: lexical_dbs/dt_eqol.Rds, dt_franqus.Rds,
#                dt_manulex_token.Rds, dt_corpus_flelex.Rds, dt_lexconn.Rds
#   Demo corpus: demo_corpora/viki_wiki/   (200 paired Vikidia/Wikipedia docs)
#
# Output:
#   out/demo_corpus_parsed.Rds          — parsed corpus
#   out/demo_corpus_with_features.Rds   — full features list incl. parsed corpus
#   out/demo_corpus_features_only.Rds   — features only (no parsed corpus)
# ---------------------------------------------------------------------------

library(data.table)
library(udpipe)
library(tidyverse)
library(utf8)
library(zoo)
library(future)
library(future.apply)

source("R/fnt_corpus.R",        encoding = "UTF-8")
source("R/fnt_lexical.R",       encoding = "UTF-8")
source("R/fnt_heights.R",       encoding = "UTF-8")
source("R/fnt_counters.R",      encoding = "UTF-8")
source("R/fnt_pos_surprisal.R", encoding = "UTF-8")
source("R/fnt_extra_syntax.R",  encoding = "UTF-8")
source("R/fnt_cohesion.R",      encoding = "UTF-8")
source("R/fnt_utility.R",       encoding = "UTF-8")
source("R/fnt_embeddings.R",    encoding = "UTF-8")
source("R/fnt_burstiness.R",    encoding = "UTF-8")
source("R/fnt_mwe.R",           encoding = "UTF-8")
source("R/artefact_builders/build_pos_ngrams.R", encoding = "UTF-8")

corpus_dir <- "demo_corpora/viki_wiki/"

udmodel_french <- udpipe_load_model(file = "models/french_gsd-remix_2.udpipe")

# Demo workflow ----
# Each block showcases a capability (parsing, counts, lexical enrichment, syntax,
# surprisal, embeddings, and outputs), with saved artifacts in `out/` folder.

# 1) Read the corpus files
dt_txt <- build_corpus(corpus_dir)

# 2) Parse with UDPipe
dt_parsed_raw <- parse_text(dt_txt, n_cores = parallel::detectCores() - 1L)

# 3) Post-process (token-level cleaning and normalization)
dt_parsed_edit <- post_process_lexicon(dt_parsed_raw)

saveRDS(dt_parsed_edit, "out/demo_corpus_parsed.Rds")

features <- list(parsed_corpus = dt_parsed_edit)

# 4) Load lexical frequency databases
dt_eqol    <- readRDS("lexical_dbs/dt_eqol.Rds")
dt_franqus <- readRDS("lexical_dbs/dt_franqus.Rds")
dt_manulex <- readRDS("lexical_dbs/dt_manulex_token.Rds")
dt_flelex  <- readRDS("lexical_dbs/dt_corpus_flelex.Rds")
dt_lexconn <- readRDS("lexical_dbs/dt_lexconn.Rds")

# 5) Simple counts
features$simple_counts <- simple_count_features(features$parsed_corpus)

# 6) Verb tense counts
features$verb_tenses <- verb_tense_features(
  features$parsed_corpus,
  features$simple_counts$doc_level_counts
)

# 7) Lexical database matching (fuzzy)
features$lexical_db$eqol    <- fuzzy_match_lexical_db(features$parsed_corpus, dt_eqol,    prefix = "eqol")
features$lexical_db$franqus <- fuzzy_match_lexical_db(features$parsed_corpus, dt_franqus, prefix = "franqus")
features$lexical_db$manulex <- fuzzy_match_lexical_db(features$parsed_corpus, dt_manulex, prefix = "manulex")
features$lexical_db$flelex  <- fuzzy_match_lexical_db(features$parsed_corpus, dt_flelex,  prefix = "flelex")

# 8) Lexical diversity
features$lexical_diversity <- lexical_diversity_general(
  df = features$parsed_corpus, window_size = 50
)

# 9) Dependency tree depth and distance features
features$heights <- docwise_graph_stats(features$parsed_corpus)

# 10) Clausal and dependency complexity (Lu 2010, Liu 2008)
features$syntactic <- extra_syntactic_features(features$parsed_corpus)

# 11) POS trigram surprisal — uses the distributed model (PUNCT/SYM excluded,
#     sentence boundaries enabled). Swap for a custom model if needed.
pos_model <- readRDS("models/pos_trigram_fr_gsd_alsi.Rds")
features$pos_surprisal <- pos_surprisal(
  features$parsed_corpus, pos_model,
  use_sentence_boundaries = TRUE
)

# 12) Lexical cohesion
features$lexical_cohesion <- simple_lexical_cohesion(features$parsed_corpus)

# 13) Discourse connectives (LEXCONN)
features$connectives$matches <- match_multiword_sequences(
  features$parsed_corpus, dt_lexconn, pos_filter = TRUE
)
features$connectives$density <- connective_density_features(
  features$parsed_corpus, features$connectives$matches
)

# 14) Word burstiness (Altmann et al. 2009; Church & Gale 1995)
features$burstiness <- burstiness_doc_features(features$parsed_corpus)

# 15–16) Embeddings and LLM surprisal — require Python ----
#
# reticulate::py_require() manages the Python environment automatically.
# If Python is not installed on this machine these sections are skipped.

if (!reticulate::py_available(initialize = TRUE)) {
  warning(
    "Python not available — skipping sections 15-16 ",
    "(embeddings and LLM surprisal). ",
    "Install Python and rerun to include these features."
  )
} else {

  library(reticulate)
  py_require(c(
    "transformers>=4.41,<5",
    "torch",
    "tokenizers",
    "sentence-transformers",
    "numpy",
    "scipy"
  ))

  # 15) Embeddings and embedding coherence ----
  features$embeddings <- corpus_embeddings(
    dt_corpus  = features$parsed_corpus,
    batch_size = 8
  )

  # 16) LLM surprisal and entropy ----
  source("R/fnt_surprisal.R", encoding = "UTF-8")

  features$surprisal$mlm <- llm_surprisal_entropy(
    features$parsed_corpus,
    model_name = "almanach/moderncamembert-base",
    mode       = "mlm",
    batch_size = 8
  )

  features$surprisal$ar <- llm_surprisal_entropy(
    features$parsed_corpus,
    model_name       = "lightonai/pagnol-small",
    mode             = "ar",
    batch_size       = 8,
    add_prefix_space = TRUE
  )

}

# 17) Demo labels: Vikidia = 1, Wikipedia = 2
features$doc_classes <- data.table(
  doc_id = features$simple_counts$doc_level_counts$doc_id,
  class  = ifelse(grepl("^viki_", features$simple_counts$doc_level_counts$doc_id), 1L, 2L)
)

# 18) Save ----
saveRDS(features, "out/demo_corpus_with_features.Rds")

features_without_corpus <- features
features_without_corpus$parsed_corpus <- NULL
saveRDS(features_without_corpus, "out/demo_corpus_features_only.Rds")
