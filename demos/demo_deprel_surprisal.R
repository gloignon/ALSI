# Demo: dependency-triple surprisal and entropy
#
# This demo shows how to measure how *predictable* the syntactic attachments
# in a sentence are, using a dependency-triple model trained on French UD data.
#
# Where POS surprisal (demo_pos_surprisal.R) looks at linear windows of POS
# tags, this model looks at "bits of trees": each dependency arc is the triple
# (head UPOS, dependency relation, dependent UPOS). The model scores
#
#   surprisal = -log2 p(dependent_pos | head_pos, dep_rel)
#
# i.e. how unexpected a dependent's grammatical category is given the category
# of its head and the relation linking them. A determiner attached to a noun
# (NOUN -det-> DET) is almost certain (~0 bits); the category of a noun's
# modifier is far less constrained.
#
# deprel_entropy is the uncertainty over the dependent category for a given
# (head_pos, dep_rel) context — a property of the context, not the token.
# deprel_entropy_reduction is the token-to-token change in that entropy, taken
# in reading order within the sentence (arcs have no inherent linear order).
#
# The model is a raw frequency table — no smoothing unless backoff_scale is
# set. Root tokens carry no incoming arc and are never scored.
#
# Prerequisites:
# - out/demo_parsed_tagged.Rds — run demos/demo_parse_tag.R first (section 5).
# - models/french_gsd-remix_3.udpipe — the standard ALSI UDPipe model (same
#   verb-retagging convention as the dependency-triple model); downloaded
#   automatically from the GitHub release if missing.

# 1) Setup ----

library(tidyverse)
library(udpipe)
library(data.table)

source("R/fnt_deprel_surprisal.R", encoding = "UTF-8")
source("R/fnt_utility.R",          encoding = "UTF-8")

# Settings used when the distributed model was built. The same value must be
# passed to every deprel_surprisal() call so scoring matches the model.
EXCLUDE_POS <- c("PUNCT", "SYM")

# 2) Load the dependency-triple model ----

# Trained on French-GSD with the ALSI verb-retagging convention, PUNCT/SYM
# excluded. Ships with ALSI.
message("Loading distributed dependency-triple models...")
deprel_model <- readRDS("models/deprel_trigram_fr_gsd_alsi.Rds")
# Same triples, flipped target: predicts the relation from the two endpoint
# POS tags, i.e. how ambiguous the attachment label is.
attach_model <- readRDS("models/attach_trigram_fr_gsd_alsi.Rds")

# 3) Score individual sentences ----

alsi_udpipe_path <- "models/french_gsd-remix_3.udpipe"
if (!file.exists(alsi_udpipe_path)) {
  message("ALSI UDPipe model not found — downloading from GitHub (one-time, ~68 MB)...")
  source("R/artefact_builders/fetch_udpipe_models.R")
}
udmodel <- udpipe_load_model(alsi_udpipe_path)

parse_one <- function(text) {
  as_tibble(udpipe_annotate(udmodel, x = text))
}

show_arcs <- function(dt_parsed, res) {
  res$token_surprisal |>
    as_tibble() |>
    left_join(as_tibble(dt_parsed)[, c("token_id", "token", "head_token_id")],
              by = "token_id") |>
    left_join(as_tibble(dt_parsed)[, c("token_id", "token")],
              by = c("head_token_id" = "token_id"),
              suffix = c("", "_head")) |>
    select(token, upos, dep_rel, head = token_head,
           deprel_surprisal, deprel_entropy, deprel_entropy_reduction) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
}

# ── 3a. A syntactically predictable sentence ─────────────────────────────────
#
# "Le petit chat mange la souris."
# Canonical attachments: DET -det-> NOUN, ADJ -amod-> NOUN, NOUN -nsubj/obj->
# VERB. Every arc is a common French dependency triple; expect low surprisal.

message("--- Predictable: 'Le petit chat mange la souris.' ---")
dt_easy  <- parse_one("Le petit chat mange la souris.")
res_easy <- deprel_surprisal(dt_easy, deprel_model, exclude_pos = EXCLUDE_POS)
print(show_arcs(dt_easy, res_easy))

# ── 3b. A sentence with a less common attachment ─────────────────────────────
#
# "Courir vite fatigue les coureurs débutants."
# The subject of "fatigue" is an infinitive clause (VERB -csubj-> VERB), a
# rarer dependency triple than a nominal subject; expect higher surprisal on
# that arc.

message("--- Less common: 'Courir vite fatigue les coureurs débutants.' ---")
dt_hard  <- parse_one("Courir vite fatigue les coureurs débutants.")
res_hard <- deprel_surprisal(dt_hard, deprel_model, exclude_pos = EXCLUDE_POS,
                             backoff_scale = 0.4)
print(show_arcs(dt_hard, res_hard))


# ── 3c. Attachment-label ambiguity (relation conditioning) ───────────────────
#
# attach_surprisal() flips the target: given a head POS and a dependent POS,
# how predictable is the relation linking them? A determiner under a noun is
# unambiguous (det, ~0 bits); a noun under a verb is ambiguous between nsubj,
# obj and obl (high surprisal and high entropy). High attach_entropy marks the
# structurally ambiguous attachments in a sentence.

message("--- Attachment ambiguity: 'Le petit chat mange la souris.' ---")
res_attach <- attach_surprisal(dt_easy, attach_model, exclude_pos = EXCLUDE_POS,
                               backoff_scale = 0.4)
print(res_attach$token_surprisal |>
  as_tibble() |>
  left_join(as_tibble(dt_easy)[, c("token_id", "token")], by = "token_id") |>
  select(token, upos, dep_rel, attach_surprisal, attach_entropy) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))))


# 4) Full corpus scoring (on Viki-Wiki) ----

message("Loading parsed corpus...")
if (!file.exists("out/demo_parsed_tagged.Rds")) {
  stop("out/demo_parsed_tagged.Rds not found — run demos/demo_parse_tag.R first.",
       call. = FALSE)
}
dt_corpus <- readRDS("out/demo_parsed_tagged.Rds") |> as_tibble()

message("Scoring dependency-triple surprisal on full corpus...")
corpus_res <- deprel_surprisal(dt_corpus, deprel_model,
                               exclude_pos = EXCLUDE_POS,
                               backoff_scale = 0.4)

add_vikiwiki_metadata <- function(dt) {
  dt |>
    as_tibble() |>
    mutate(
      source  = if_else(str_detect(doc_id, "^viki_"), "Vikidia", "Wikipedia"),
      pair_id = str_extract(doc_id, "\\d+")
    )
}

dt_doc <- corpus_res$doc_surprisal |>
  add_vikiwiki_metadata()

message("Dependency-triple document-level means by source:")
dt_doc |>
  group_by(source) |>
  summarise(across(where(is.numeric) & !all_of("pair_id"),
                   \(x) round(mean(x, na.rm = TRUE), 3))) |>
  print()

message("Scoring attachment-label surprisal on full corpus...")
attach_corpus_res <- attach_surprisal(dt_corpus, attach_model,
                                      exclude_pos = EXCLUDE_POS,
                                      backoff_scale = 0.4)

dt_attach_doc <- attach_corpus_res$doc_surprisal |>
  add_vikiwiki_metadata()

message("Attachment-label document-level means by source:")
dt_attach_doc |>
  group_by(source) |>
  summarise(across(where(is.numeric) & !all_of("pair_id"),
                   \(x) round(mean(x, na.rm = TRUE), 3))) |>
  print()


# 5) Visualisation ----
#
# Vikidia (simple French encyclopedia) tends to use more canonical syntactic
# attachments than Wikipedia. Documents are paired (same topic), so paired
# Cohen's d removes between-topic variance.

plot_faceted_boxplot(
  dt_doc, source,
  title = "Dependency-triple surprisal and entropy: Vikidia vs Wikipedia",
  y_lab = NULL
)

plot_faceted_boxplot(
  dt_attach_doc, source,
  title = "Attachment-label surprisal and entropy: Vikidia vs Wikipedia",
  y_lab = NULL
)
