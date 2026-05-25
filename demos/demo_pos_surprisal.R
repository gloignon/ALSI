# Demo: POS-trigram surprisal and entropy
#
# This demo shows how to measure how *predictable* the grammatical structure
# of a sentence is, using a POS trigram model trained on French UD data.
#
# POS surprisal (-log2 p) tells us how unexpected a word's grammatical
# category is given the two preceding categories. High surprisal = unusual
# local POS sequence; low surprisal = very predictable structure.
#
# POS entropy is the uncertainty over what category comes next given the
# same bigram context. It is a property of the context, not the target token:
# high entropy means many continuations are possible from that position.
#
# Entropy reduction (entropy[t-1] - entropy[t]) captures how much the
# current token resolved the uncertainty created at the previous step.
#
# The model is a raw frequency table — no smoothing yet. Unseen trigrams
# receive NA surprisal. With use_sentence_boundaries = FALSE (default), tokens
# at sentence positions 1 and 2 (no full left context) are unscored.
#
# Prerequisites:
# - corpora/fr_gsd-ud-train_alsi-verbpos.conllu   (modified GSD training data)
# - models/fr_gsd_alsi_20260524_122826.udpipe      (matching ALSI UDPipe model)
# - out/demo_parsed_tagged.Rds                     (run demo_parse_tag.R first)

# 1) Setup ----

library(tidyverse)
library(udpipe)
library(data.table)

source("R/artefact_builders/build_pos_ngrams.R", encoding = "UTF-8")
source("R/fnt_pos_surprisal.R",                  encoding = "UTF-8")
source("R/fnt_utility.R",                         encoding = "UTF-8")

# Default model settings — match the distributed model in models/.
# Change these only if you want a custom model; a rebuild will be triggered
# automatically and cached separately.
EXCLUDE_POS    <- c("PUNCT", "SYM")
USE_BOUNDARIES <- TRUE

DISTRIBUTED_MODEL <- "models/pos_trigram_fr_gsd_alsi.Rds"

# 2) Load or build the POS trigram model ----

# The distributed model (trained on French GSD, PUNCT/SYM excluded, sentence
# boundaries enabled) is loaded directly if the settings above match the
# defaults. Changing EXCLUDE_POS or USE_BOUNDARIES triggers a custom rebuild
# that is cached under a separate filename in models/.

default_exclude    <- c("PUNCT", "SYM")
default_boundaries <- TRUE

using_defaults <- identical(EXCLUDE_POS, default_exclude) &&
                  identical(USE_BOUNDARIES, default_boundaries)

if (using_defaults) {
  message("Loading distributed POS model...")
  pos_model <- readRDS(DISTRIBUTED_MODEL)
} else {
  cache_model <- sprintf(
    "models/pos_model_excl%s_bnd%s.Rds",
    paste(EXCLUDE_POS, collapse = "-"),
    USE_BOUNDARIES
  )
  if (file.exists(cache_model)) {
    message("Loading cached custom POS model...")
    pos_model <- readRDS(cache_model)
  } else {
    message("Building custom POS trigram model...")
    ngrams <- build_pos_ngrams(
      "corpora/fr_gsd-ud-train_alsi-verbpos.conllu",
      exclude_pos             = EXCLUDE_POS,
      use_sentence_boundaries = USE_BOUNDARIES
    )
    message(sprintf("  %d trigram types observed.", nrow(ngrams)))
    print(head(ngrams, 10))

    pos_model <- prepare_pos_model(ngrams)
    saveRDS(pos_model, cache_model)
    message(sprintf("  Cached to %s", cache_model))
  }
}


# 3) Score individual sentences ----

udmodel <- udpipe_load_model("models/fr_gsd_alsi_20260524_122826.udpipe")

parse_one <- function(text) {
  setDT(as.data.table(udpipe_annotate(udmodel, x = text)))
}

# ── 3a. A syntactically predictable sentence ─────────────────────────────────
#
# "Le petit chat mange la souris."
# DET ADJ NOUN VERB DET NOUN — a canonical SVO sequence; every transition
# is common in French. Expect uniformly low surprisal.

message("--- Predictable: 'Le petit chat mange la souris.' ---")
dt_easy <- parse_one("Le petit chat mange la souris.")
res_easy <- pos_surprisal(dt_easy, pos_model, exclude_pos = EXCLUDE_POS,
                          use_sentence_boundaries = USE_BOUNDARIES)

print(res_easy$token_surprisal |>
  as_tibble() |>
  left_join(as_tibble(dt_easy)[, c("token_id", "token")], by = "token_id") |>
  select(token, upos, pos_surprisal, pos_entropy, pos_entropy_reduction) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))))

# ── 3b. A syntactically surprising sentence ───────────────────────────────────
#
# "Hier soir, étrangement, il pleuvait des cordes."
# ADV NOUN ADV PRON VERB DET NOUN — the opening ADV ADV sequence (temporal
# adverb immediately followed by manner adverb) is rare; expect high surprisal
# at the first non-context token and elevated entropy in adverbial contexts.

message("--- Surprising: 'Hier soir, étrangement, il pleuvait des cordes.' ---")
dt_hard <- parse_one("Hier soir, étrangement, il pleuvait des cordes.")
res_hard <- pos_surprisal(dt_hard, pos_model, exclude_pos = EXCLUDE_POS,
                          use_sentence_boundaries = USE_BOUNDARIES)

print(res_hard$token_surprisal |>
  as_tibble() |>
  left_join(as_tibble(dt_hard)[, c("token_id", "token")], by = "token_id") |>
  select(token, upos, pos_surprisal, pos_entropy, pos_entropy_reduction) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))))

# ── 3c. A nominalized / heavy-NP sentence ─────────────────────────────────────
#
# "La décision du conseil d'administration a été contestée."
# DET NOUN ADP NOUN ADP NOUN AUX AUX VERB — long nominal chain before the
# verb creates repeated ADP→DET→NOUN transitions (low surprisal there) but
# the double auxiliary sequence (AUX AUX VERB) is much rarer.

message("--- Nominalized: 'La décision du conseil d'administration a été contestée.' ---")
dt_nom <- parse_one("La décision du conseil d'administration a été contestée.")
res_nom <- pos_surprisal(dt_nom, pos_model, exclude_pos = EXCLUDE_POS,
                         use_sentence_boundaries = USE_BOUNDARIES)

print(res_nom$token_surprisal |>
  as_tibble() |>
  left_join(as_tibble(dt_nom)[, c("token_id", "token")], by = "token_id") |>
  select(token, upos, pos_surprisal, pos_entropy, pos_entropy_reduction) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))))


# 4) Full corpus scoring (on Viki-Wiki) ----

message("Loading parsed corpus...")
dt_corpus <- readRDS("out/demo_parsed_tagged.Rds") |> as_tibble() |> setDT()

message("Scoring POS surprisal on full corpus...")
corpus_res <- pos_surprisal(dt_corpus, pos_model, exclude_pos = EXCLUDE_POS,
                            use_sentence_boundaries = FALSE,
                            backoff_scale = NULL)

dt_doc <- corpus_res$doc_surprisal |>
  as_tibble() |>
  mutate(
    source  = if_else(str_detect(doc_id, "^viki_"), "Vikidia", "Wikipedia"),
    pair_id = str_extract(doc_id, "\\d+")
  )

message("Document-level means by source:")
dt_doc |>
  group_by(source) |>
  summarise(across(where(is.numeric) & !all_of("pair_id"), \(x) round(mean(x, na.rm = TRUE), 3))) |>
  print()


# 5) Visualisation ----
#
# Vikidia (simple French encyclopedia) uses more predictable syntactic patterns
# than Wikipedia. Documents are paired (same topic), so paired Cohen's d is used
# to remove between-topic variance — more sensitive than the unpaired estimate.

plot_faceted_boxplot(
  dt_doc, source,
  c(mean_pos_surprisal, mean_pos_entropy, mean_pos_entropy_reduction, sd_pos_surprisal, sd_pos_entropy),
  title    = "POS surprisal and entropy: Vikidia vs Wikipedia",
  y_lab    = NULL
)

