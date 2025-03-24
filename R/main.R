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
# Last update: 2025-03-14
#

library("data.table") #together forever...
library(udpipe)
library(tidyverse)
library(utf8)

source('R/fnt_corpus.R', encoding = 'UTF-8')
source('R/fnt_lexical.R', encoding = 'UTF-8')
source('R/fnt_heights.R', encoding = 'UTF-8')
source('R/fnt_counters.R', encoding = 'UTF-8')
source('R/fnt_pos_surprisal.R', encoding = 'UTF-8')
source('R/fnt_extra_syntax.R', encoding = 'UTF-8')
source('R/fnt_cohesion.R', encoding = 'UTF-8')

corpus_dir <- "corpus/french_corpus_20240403/"  # this folder should contain your text files in .txt format

udmodel_french <-
  udpipe_load_model(file = "models/french_gsd-remix_2.udpipe") #chargement du modèle linguistique

dt_eqol <- readRDS("lexical_dbs/dt_eqol.Rds")
dt_franqus <- readRDS("lexical_dbs/dt_franqus.Rds")
dt_manulex <- readRDS("lexical_dbs/dt_manulex_token.Rds")
dt_flelex <- readRDS("lexical_dbs/dt_corpus_flelex.Rds")

# Create a corpus ----

# Read the files
dt_txt <- constituerCorpus(corpus_dir)

# Parse the files using udpipe
dt_parsed_raw <- parserTexte(dt_txt, nCores = 10)  # analyse lexicale avec udpipe, pourrait prendre quelques minutes...

# # Edit the resulting dt
features <- list(parsed_corpus = postTraitementLexique(dt_parsed_raw))  # post-traitement du corpus

## check for duplicated tokens in dt_parsed_edited
# 
# # Add lexical information ----
features$lexical_db$eqol <- fuzzy_match_lexical_db(features$parsed_corpus, dt_eqol, prefix = "eqol")
features$lexical_db$franqus <- fuzzy_match_lexical_db(features$parsed_corpus, dt_franqus, prefix = "franqus")
features$lexical_db$manulex <- fuzzy_match_lexical_db(features$parsed_corpus, dt_manulex, prefix = "manulex")
features$lexical_db$flelex <- fuzzy_match_lexical_db(features$parsed_corpus, dt_flelex, prefix = "flelex")

# Simple counts
features$simple_counts <- simple_count_features(features$parsed_corpus)

# Verb tense counts
features$verb_tenses <- verb_tense_features(features$parsed_corpus, features$simple_counts$doc_level_counts)

# Lexical diversity indices
features$lexical_diversity <- lexical_diversity_general(df = features$parsed_corpus, window_size = 50)

# Syntactic depth/height
features$heights <- docwise_graph_stats(features$parsed_corpus)

# More dependency tree related features
features$syntactic <- extra_syntactic_features(features$parsed_corpus)

# POS surprisal
features$pos_surprisal <- pos_surprisal(features$parsed_corpus)

# Lexical cohesion 
features$lexical_cohesion <- simple_lexical_cohesion(features$parsed_corpus)

# If you've got classes, now would be the time to add them to the features list
# Get the class from doc_id - the part before the first underscore, without the g prefix
# e.g. g01_pri_amélieetlesoeufs is class 01
features$parsed_corpus[, class := as.numeric(str_extract(doc_id, "(?<=g)\\d+"))]

# Save the features ----
saveRDS(features, "corpus/french_corpus_20240403_features.Rds")

