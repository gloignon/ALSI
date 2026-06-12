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
#   - demo_corpora/viki_wiki.zip — paired Vikidia / Wikipedia plain-text files
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
source("R/fnt_setup.R", encoding = "UTF-8")

# Where the text files live after first-run extraction (one .txt per document).
# ensure_viki_wiki_demo_corpus() unzips demo_corpora/viki_wiki.zip if needed and
# returns the directory; we keep the path because section 2 reads a single file.
# (If you just want the whole corpus in one call, use load_demo_corpus() — see 3a.)
corpus_dir <- ensure_viki_wiki_demo_corpus()

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
  as_tibble()

print(dt_example_raw |> dplyr::select(token, lemma, upos, dep_rel, head_token_id))


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

dt_single_txt <- build_corpus(file.path(corpus_dir, "viki_20729.txt"))
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

# 3a) Read all files into a single data.table (one row per document).
# load_demo_corpus() unzips the bundled demo corpus if needed, then reads it
# with build_corpus() — one call instead of two.
dt_txt <- load_demo_corpus()
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
#
# ALSI ships the Alector corpus as unmodified .txt files in alector.zip
# (allowed under CC BY-NC-ND 4.0 "Share"). The CSV below is a derived
# artefact built locally on first run — do not redistribute it.

alector_csv <- "demo_corpora/alector_corpus.csv"

if (!file.exists(alector_csv)) {
  message(
    "\n--- Alector corpus CSV not found — building from bundled zip ---\n",
    "\n",
    "The Alector corpus (Gala et al., 2020) is included as unmodified source\n",
    "files under CC BY-NC-ND 4.0. ALSI does not redistribute derived artefacts;\n",
    "the CSV is built locally on first run.\n",
    "\n",
    "By continuing you confirm that your use is non-commercial.\n",
    "Building now..."
  )
  source("R/artefact_builders/build_alector_corpus.R", encoding = "UTF-8")
}

dt_alector <- fread(alector_csv, encoding = "UTF-8")

# Each numeric id identifies a *pair* of texts: the original ("source") and
# its simplified version ("target"). parse_text() requires unique doc_ids,
# so combine both columns (e.g. "012_source", "012_target").
dt_alector[, doc_id := sprintf("%03d_%s", as.integer(id), class)]
dt_alector <- dt_alector[, .(doc_id, text)]

# parser = "none" skips dependency parsing (no syntactic head assignment).
# This is faster when you only need tokens and POS tags, not tree structure.
dt_alector_raw    <- parse_text(dt_alector, n_cores = n_cores, parser = "none")
dt_alector_parsed <- post_process_lexicon(dt_alector_raw)

saveRDS(dt_alector_parsed, "out/demo_alector_parsed.Rds")
message("Saved to out/demo_alector_parsed.Rds")
