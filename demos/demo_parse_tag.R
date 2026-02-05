# ALSI Demo: Corpus Parsing and Tagging
#
# This demo only builds a corpus, parses it with UDPipe, and applies
# the standard post-processing on the token-level output.
#
# Last update: 2026-02-05
#

library(data.table)
library(udpipe)

source('R/fnt_corpus.R', encoding = 'UTF-8')
# source('R/fnt_lexical.R', encoding = 'UTF-8')

corpus_dir <- 'demo_corpus/'  # folder with .txt files
udmodel_french <- udpipe_load_model(file = 'models/french_gsd-remix_2.udpipe')

n_cores <- max(1, parallel::detectCores() - 1)

# 1) Read the files into a corpus data.table

dt_txt <- constituerCorpus(corpus_dir)

# 2) Parse and tag the corpus using UDPipe
future::plan(future::sequential)

dt_parsed_raw <- parse_text(dt_txt, n_cores = 8)

# 3) Post-process the parsed output

dt_parsed_edit <- postTraitementLexique(dt_parsed_raw)

# Optional: save the parsed corpus for inspection

if (!dir.exists("out")) dir.create("out", recursive = TRUE)
saveRDS(dt_parsed_edit, 'out/demo_parsed_tagged.Rds')
