# ALSI Demo: Corpus Parsing and Tagging
#
# This demo only builds a corpus, parses it with UDPipe, and applies
# the standard post-processing on the token-level output.
#
# Last update: 2026-03-24
#

library(data.table)
library(udpipe)

source('R/fnt_corpus.R', encoding = 'UTF-8')
# source('R/fnt_lexical.R', encoding = 'UTF-8')

corpus_dir <- 'demo_corpus/'  # folder with .txt files
udmodel_french <- udpipe_load_model(file = 'models/french_gsd-remix_2.udpipe')

n_cores <- max(1, parallel::detectCores() - 1)

# Quick demos -----
# Parse a single example sentence to demo

example_sentence <- "Je crois que 80% des statistiques sont inventées."

dt_example_raw <- udpipe_annotate(udmodel_french, x = example_sentence) |>
  as.data.frame() |> as.data.table()

print(dt_example_raw[, .(token, lemma, upos, dep_rel, head_token_id)])

# Parse a single text file

txt_single <- fread('demo_corpus/viki_20729.txt', header = FALSE, sep = NULL, col.names = 'text')
dt_single_raw <- udpipe_annotate(udmodel_french, x = txt_single$text) |>
  as.data.frame() |> as.data.table()

print(dt_single_raw[, .(sentence_id, token, lemma, upos, dep_rel, head_token_id)])

# Full corpus parse & tag demo -----
# 1) Read the files into a corpus data.table

dt_txt <- constituerCorpus(corpus_dir)

# 2) Parse and tag the corpus using UDPipe
dt_parsed_raw <- parse_text(dt_txt, n_cores = n_cores)

# 3) Post-process the parsed output

dt_parsed_edit <- postTraitementLexique(dt_parsed_raw)

# Optional: save the parsed corpus for inspection

if (!dir.exists("out")) dir.create("out", recursive = TRUE)
saveRDS(dt_parsed_edit, 'out/demo_parsed_tagged.Rds')
