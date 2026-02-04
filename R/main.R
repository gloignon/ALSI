# ALSI / ILSA
# 
# Analyseur Lexicosyntaxique intégré
# Integrated lexicosyntactic analyzer
# 
# Par : Guillaume Loignon
#
# loignon.guillaume@uqam.ca
# 
# General workflow:
#
# 1. Parsing and annotation
# 2. Post parsing editing
# 3. Lexical database merging
# 4. More feature extraction
#
# Last update: 2026-01-29
#

library("data.table") 
library(udpipe)
library(tidyverse)
library(utf8)
library(igraph) # for node distances in fnt_heights.R
library(zoo)    # for rolling functions in fnt_lexical.R


source('R/fnt_corpus.R', encoding = 'UTF-8')
source('R/fnt_lexical.R', encoding = 'UTF-8')
source('R/fnt_heights.R', encoding = 'UTF-8')
source('R/fnt_counters.R', encoding = 'UTF-8')
source('R/fnt_pos_surprisal.R', encoding = 'UTF-8')
source('R/fnt_extra_syntax.R', encoding = 'UTF-8')
source('R/fnt_cohesion.R', encoding = 'UTF-8')
source('R/fnt_embeddings.R', encoding = 'UTF-8')

corpus_dir <- "demo_corpus/"  # this folder should contain your text files in .txt format

udmodel_french <-
  udpipe_load_model(file = "models/french_gsd-remix_2.udpipe") # this is the linguistic model, by default we are using
                                                               # a custom UD model for French

# Create a corpus ----

# Read the files
dt_txt <- constituerCorpus(corpus_dir)

# Parse the files using udpipe
dt_parsed_raw <- parse_text(dt_txt, n_cores = 12)  # analyse lexicale avec udpipe, pourrait prendre quelques minutes...

# # Edit the resulting dt
dt_parsed_edit <- postTraitementLexique(dt_parsed_raw)

saveRDS(dt_parsed_edit, "out/demo_corpus_parsed.Rds")

features <- list(parsed_corpus = dt_parsed_edit)  # post-traitement du corpus

#save the parsed corpus, in case you need to restart
# features <- readRDS("corpus/french_corpus_parsed_raw.Rds")

# The following will load lexical frequency databases
dt_eqol <- readRDS("lexical_dbs/dt_eqol.Rds")
dt_franqus <- readRDS("lexical_dbs/dt_franqus.Rds")
dt_manulex <- readRDS("lexical_dbs/dt_manulex_token.Rds")
dt_flelex <- readRDS("lexical_dbs/dt_corpus_flelex.Rds")

# Simple counts
features$simple_counts <- simple_count_features(features$parsed_corpus)

# Verb tense counts
features$verb_tenses <- verb_tense_features(features$parsed_corpus, features$simple_counts$doc_level_counts)

# # Add lexical information ----
features$lexical_db$eqol_imp <- add_lexical_freq_with_imputation(
  parsed_corpus = features$parsed_corpus,
  lexical_db = dt_eqol,
  prefix = "eqol",
  freq_col = "freq_u",
  mode = "u"
)
features$lexical_db$manulex_imp <- add_lexical_freq_with_imputation(
  parsed_corpus = features$parsed_corpus,
  lexical_db = dt_manulex,
  prefix = "manulex",
  freq_col = "freq_u",
  mode = "u"
)
features$lexical_db$flelex_imp <- add_lexical_freq_with_imputation(
  parsed_corpus = features$parsed_corpus,
  lexical_db = dt_flelex,
  prefix = "flelex",
  freq_col = "freq_u",
  mode = "u"
)
features$lexical_db$franqus_imp <- add_lexical_freq_with_imputation(
  parsed_corpus = features$parsed_corpus,
  lexical_db = dt_franqus,
  prefix = "franqus",
  freq_col = "freq_u",
  mode = "u"
)
features$lexical_db$eqol <- fuzzy_match_lexical_db(features$parsed_corpus, dt_eqol, prefix = "eqol")
features$lexical_db$franqus <- fuzzy_match_lexical_db(features$parsed_corpus, dt_franqus, prefix = "franqus")
features$lexical_db$manulex <- fuzzy_match_lexical_db(features$parsed_corpus, dt_manulex, prefix = "manulex")
features$lexical_db$flelex <- fuzzy_match_lexical_db(features$parsed_corpus, dt_flelex, prefix = "flelex")

# Lexical diversity indices
features$lexical_diversity <- lexical_diversity_general(df = features$parsed_corpus, window_size = 50)

# Syntactic depth/height
features$heights <- docwise_graph_stats(features$parsed_corpus)

# More dependency tree related features
features$syntactic <- extra_syntactic_features(features$parsed_corpus)

# Simple POS surprisal from ngram model
features$pos_surprisal <- pos_surprisal(features$parsed_corpus)

# Lexical cohesion 
features$lexical_cohesion <- simple_lexical_cohesion(features$parsed_corpus)

# Embeddings  ----
library(reticulate)
source('R/fnt_embeddings.R', encoding = 'UTF-8')
start_python_backend(venv_name = "textenv-gpu", use_gpu = TRUE)

features$embeddings <- corpus_embeddings(
  dt_corpus = features$parsed_corpus
)

# If you've got classes (e.g. school grades), now would be the time to add them to 
# the features list.
# For the demo corpus, we will add a column to tag viki as 1 and wiki as 2, based on 
# the file names.
dt_doc_classes <- data.table(
  doc_id = features$simple_counts$doc_level_counts$doc_id,
  class = ifelse(grepl("viki", features$simple_counts$doc_level_counts$doc_id), 1L, 2L)
)

features$doc_classes <- dt_doc_classes

# Save the corpus with features ----
saveRDS(features, "out/demo_corpus_with_features.Rds")

# If you want just the features,
features_without_corpus <- features
features_without_corpus$parsed_corpus <- NULL

saveRDS(features_without_corpus, "out/demo_corpus_features_only.Rds")
