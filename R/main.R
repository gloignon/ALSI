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
source('R/fnt_lexical_experimental.R', encoding = 'UTF-8')
source('R/fnt_heights.R', encoding = 'UTF-8')
source('R/fnt_counters.R', encoding = 'UTF-8')
source('R/fnt_pos_surprisal.R', encoding = 'UTF-8')

corpus_dir <- "corpus/test/"  # this folder should contain your text files in .txt format

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
dt_parsed_raw <- parserTexte(dt_txt)  # analyse lexicale avec udpipe, pourrait prendre quelques minutes...

# # Edit the resulting dt
features <- list(parsed_corpus = postTraitementLexique(dt_parsed_raw))  # post-traitement du corpus
                 

## check for duplicated tokens in dt_parsed_edited
# # dt_parsed_edited[duplicated(vraiTokenId)]
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

# Syntactic depth/height
features$heights <- batch_graph_stats(features$parsed_corpus)

# Head final/initial tokens
features$head_final <- head_final_initial(features$parsed_corpus)

# Lexical diversity indices
features$lexical_diversity <- lexical_diversity_general(df = features$parsed_corpus, window_size = 50)

# POS surprisal
features$pos_surprisal <- pos_surprisal(features$parsed_corpus)
