# ALSI Demo: Corpus Parsing and Tagging
#
# This is the starting point for most ALSI analyses. It shows how to:
#   - read a folder of plain-text files into a corpus data.table
#   - parse it with UDPipe (assigns a part-of-speech tag, lemma, and
#     syntactic head to every word)
#   - apply ALSI post-processing (cleans up lemmas, normalises tokens)
#
# The parsed corpus saved at the end of this script is the input for every
# other demo. Run this script first before running any other demo.
#
# Prerequisites:
#   - demo_corpora/viki_wiki/   — paired Vikidia / Wikipedia plain-text files
#   - demo_corpora/alector_corpus.csv — ALECTOR simplification corpus (optional)
#   - models/french_gsd-remix_3.udpipe — ALSI French UDPipe model
#
# Output:
#   - out/demo_parsed_tagged.Rds   — parsed Viki-Wiki corpus
#   - out/demo_alector_parsed.Rds  — parsed ALECTOR corpus (if CSV is present)
#
# Last update: 2026-03-24

library(data.table)  # fast data frames — used throughout ALSI
library(udpipe)      # interface to the UDPipe parser

source("R/fnt_corpus.R", encoding = "UTF-8")

# Where the text files live (one .txt per document)
corpus_dir <- "demo_corpora/viki_wiki/"

# Load the French UDPipe model into memory.
# This only needs to be done once per session — it is slow to load,
# but fast once loaded.
udmodel_french <- udpipe_load_model(file = "models/french_gsd-remix_3.udpipe")

# Detect the number of CPU cores available and reserve one for the OS.
# parse_text() will spread sentences across cores to speed things up.
n_cores <- max(1, parallel::detectCores() - 1)


# 1) Setup: single-sentence inspection ----
#
# Before processing a whole corpus it helps to see exactly what UDPipe
# produces for one sentence. The output table has one row per token.
#
# Key columns:
#   token        — the word as it appears in the text
#   lemma        — the dictionary form (infinitive for verbs, singular for nouns)
#   upos         — Universal POS tag (NOUN, VERB, ADJ, DET, ADP, …)
#   dep_rel      — syntactic function (nsubj = subject, obj = object, …)
#   head_token_id — the token_id of the syntactic head (governor)
#
# "Je crois" → PRON + VERB; "que" → SCONJ introducing the subordinate clause.

example_sentence <- "Je crois que 80% des statistiques sont inventées."

dt_example_raw <- udpipe_annotate(udmodel_french, x = example_sentence) |>
  as.data.frame() |>
  as.data.table()

print(dt_example_raw[, .(token, lemma, upos, dep_rel, head_token_id)])


# 2) Parse a single text file ----
#
# build_corpus() reads one or more .txt files and returns a data.table
# with columns: doc_id (filename without extension) and text.
#
# parse_text() sends each document through UDPipe and returns one row per
# token across all documents.
#
# post_process_lexicon() normalises lemmas and tokens (e.g. lowercases
# for matching purposes, handles contractions).

dt_single_txt <- build_corpus("demo_corpora/viki_wiki/viki_20729.txt")
dt_single_raw <- parse_text(dt_single_txt, n_cores = 1)
dt_single     <- post_process_lexicon(dt_single_raw)

# Show the first few rows: one row per token in the document.
print(dt_single[, .(sentence_id, token, lemma, upos, dep_rel, head_token_id)])


# 3) Full corpus: parse and tag ----
#
# This reads every .txt in corpus_dir, parses it, and post-processes.
# On a laptop with 4 cores and ~400 documents (200 Vikidia/Wikipedia pairs) this takes a few minutes.
#
# For encoding issues (Latin-1, Windows-1252), see demos/demo_corpus_read.R

# 3a) Read all files into a single data.table (one row per document)
dt_txt <- build_corpus(corpus_dir)
message(nrow(dt_txt), " documents loaded from ", corpus_dir)

# 3b) Parse and POS-tag with UDPipe.
#     n_cores spreads the work across CPU threads — adjust if you want
#     to reserve more cores for other tasks.
dt_parsed_raw <- parse_text(dt_txt, n_cores = n_cores)

# 3c) Apply ALSI post-processing (lemma normalisation, token cleaning).
dt_parsed_edit <- post_process_lexicon(dt_parsed_raw)

# Save to disk so other demos can load it without re-parsing.
if (!dir.exists("out")) dir.create("out", recursive = TRUE)
saveRDS(dt_parsed_edit, "out/demo_parsed_tagged.Rds")
message("Saved to out/demo_parsed_tagged.Rds")


# 4) Parsing from a CSV corpus (ALECTOR example) ----
#
# Some corpora come as a single CSV with one row per document rather than
# individual text files. ALECTOR is one example (columns: id, text, class).
#
# parse_text() accepts a data.frame directly — the only requirement is
# that it has columns named doc_id and text. If your CSV uses different
# column names, rename them first (as shown below).

dt_alector <- fread("demo_corpora/alector_corpus.csv", encoding = "UTF-8")

# Rename the id column to doc_id so parse_text() recognises it.
setnames(dt_alector, "id", "doc_id")

# parser = "none" skips dependency parsing (no syntactic head assignment).
# This is faster when you only need tokens and POS tags, not tree structure.
dt_alector_raw    <- parse_text(dt_alector, n_cores = n_cores, parser = "none")
dt_alector_parsed <- post_process_lexicon(dt_alector_raw)

saveRDS(dt_alector_parsed, "out/demo_alector_parsed.Rds")
message("Saved to out/demo_alector_parsed.Rds")
