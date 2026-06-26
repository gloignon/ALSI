# Demo: T-unit complexity features
#
# In this demo you will:
# 1) inspect T-unit boundaries on toy French sentences;
# 2) compute Lu's (2010) MLT and related T-unit measures on the Vikidia/Wikipedia corpus;
# 3) compare Vikidia (simplified) vs Wikipedia (original) text complexity;
# 4) repeat the comparison on ALECTOR (79 paired original/simplified texts);
# 5) visualise both comparisons.
#
# Background:
#   A T-unit (Hunt 1965) is one independent clause plus everything attached
#   to it — relative clauses, adverbial clauses, embedded complements, etc.
#   It is the standard unit of syntactic maturity in L1/L2 writing research.
#   Longer mean T-units (MLT) indicate more complex sentence structure.
#
#   This implementation identifies T-unit boundaries from UD parses:
#   the sentence root anchors the first T-unit; each coordinated predicate
#   reachable via a chain of 'conj' arcs starts a new one.
#
# Prerequisites:
# - A UDPipe model at models/french_gsd-remix_3.udpipe (for the toy sentences).
# - Run demos/demo_parse_tag.R first. It saves out/demo_parsed_tagged.Rds,
#   the ~400-document Vikidia/Wikipedia paired corpus used in sections 2–3.
# - demo_corpora/alector.zip — bundled ALECTOR corpus (79 paired
#   original/simplified texts); unzipped, parsed, and cached automatically
#   on first run (section 4).
#
# Last update: 2026-05-27

# 1) Setup ----

library(tidyverse)
library(data.table)
library(udpipe)

source("R/fnt_corpus.R",       encoding = "UTF-8")
source("R/fnt_syntactic_complexity.R", encoding = "UTF-8")
source("R/fnt_tunits.R",       encoding = "UTF-8")
source("R/fnt_utility.R",      encoding = "UTF-8")
source("R/fnt_setup.R",        encoding = "UTF-8")  # ensure_alector_demo_corpus()

dir.create("out", showWarnings = FALSE)

udmodel_french <- udpipe_load_model("models/french_gsd-remix_3.udpipe")

# Helper: parse one sentence and add the 'compte' column the function needs.
# compte == TRUE marks tokens that count toward length (excludes PUNCT, SYM).
parse_sentence <- function(text, model) {
  dt <- udpipe(text, object = model) |>
    as_tibble() |>
    mutate(compte = !upos %in% c("PUNCT", "SYM"))
  return(dt)
}

# Helper: run tunit_features and show a tidy one-row result.
get_tunits <- function(dt) {
  tunit_features(dt) |>
    select(-doc_id) |>
    mutate(across(everything(), \(x) round(x, 2)))
}


# 2) Toy-sentence inspection ----

# ── 2a. Simple sentence (1 T-unit) ──────────────────────────────────────────
#
# "Le chat mange la souris."
# One finite verb, no coordination → 1 T-unit.
# MLT equals the total token count.

message("--- Simple: 'Le chat mange la souris.' ---")
parse_sentence("Le chat mange la souris.", udmodel_french) |> get_tunits() |> print()

# ── 2b. Embedded subordinate clause (still 1 T-unit) ────────────────────────
#
# "Je pense que le chat mange la souris."
# The 'que'-clause is a complement (ccomp), not a conj — it stays inside the
# same T-unit. This illustrates the key difference from clause-count measures:
# more clauses does not necessarily mean more T-units.

message("--- Embedded ccomp: 'Je pense que le chat mange la souris.' ---")
parse_sentence("Je pense que le chat mange la souris.", udmodel_french) |> get_tunits() |> print()

# ── 2c. Predicate coordination (2 T-units) ──────────────────────────────────
#
# "Marie chante et danse."
# 'danse' is a conj of 'chante' (the root). 'danse' is a VERB, so it starts a
# new T-unit. This is the shared-subject case: Hunt counts it as 2 T-units
# even though the subject is elided.

message("--- Shared-subject coordination: 'Marie chante et danse.' ---")
parse_sentence("Marie chante et danse.", udmodel_french) |> get_tunits() |> print()

# ── 2d. Overt-subject coordination (2 T-units) ──────────────────────────────
#
# "Marie chante et Pierre danse."
# Both clauses have overt subjects. 'danse' is still conj of 'chante'.

message("--- Overt-subject coordination: 'Marie chante et Pierre danse.' ---")
parse_sentence("Marie chante et Pierre danse.", udmodel_french) |> get_tunits() |> print()

# ── 2e. Nominal coordination (1 T-unit) ─────────────────────────────────────
#
# "Marie lit un grand et beau livre."
# 'beau' is conj of 'grand' — both are ADJ modifiers of 'livre' (a noun).
# Neither is a predicate (no cop child), so no new T-unit is opened.

message("--- Nominal coordination: 'Marie lit un grand et beau livre.' ---")
parse_sentence("Marie lit un grand et beau livre.", udmodel_french) |> get_tunits() |> print()

# ── 2f. Copular coordination (2 T-units) ────────────────────────────────────
#
# "Il est grand et elle est belle."
# 'grand' and 'belle' are each ADJ + cop, which qualifies as a predicate.
# 'est...belle' is conj of 'est...grand' → 2 T-units.

message("--- Copular coordination: 'Il est grand et elle est belle.' ---")
parse_sentence("Il est grand et elle est belle.", udmodel_french) |> get_tunits() |> print()

# ── 2g. Chain coordination (3 T-units) ──────────────────────────────────────
#
# "Marie chante, Pierre danse et Jean rit."
# Three coordinated predicates at root level → 3 T-units.

message("--- Three-way coordination: 'Marie chante, Pierre danse et Jean rit.' ---")
parse_sentence("Marie chante, Pierre danse et Jean rit.", udmodel_french) |> get_tunits() |> print()

# ── 2h. Coordination inside a subordinate clause (1 T-unit) ─────────────────
#
# "Je crois que Marie chante et danse."
# 'danse' is conj of 'chante', but 'chante' is attached to the root by ccomp,
# not conj — so the BFS from root via conj-only arcs never reaches 'danse'.
# n_tunits stays at 1.

message("--- Coord inside subordinate: 'Je crois que Marie chante et danse.' ---")
parse_sentence("Je crois que Marie chante et danse.", udmodel_french) |> get_tunits() |> print()


# 3) Full corpus application ----

if (!file.exists("out/demo_parsed_tagged.Rds")) {
  stop(
    "Parsed corpus not found.\n",
    "Run demos/demo_parse_tag.R first to create out/demo_parsed_tagged.Rds."
  )
}

# Load the ~400-document parsed corpus. Add 'compte' (TRUE = countable token,
# i.e. not PUNCT or SYM) — this is what tunit_features() uses for length counts.
dt_corpus <- readRDS("out/demo_parsed_tagged.Rds") |>
  as_tibble() |>
  mutate(compte = !upos %in% c("PUNCT", "SYM"))

message("Parsed corpus loaded: ", nrow(dt_corpus), " tokens across ",
        n_distinct(dt_corpus$doc_id), " documents.")


# 4) Compute T-unit features ----
#
# tunit_features() returns one row per document with:
#   n_tunits        — total T-units in the document
#   n_sentences     — total orthographic sentences
#   mlt             — mean length of T-unit in tokens (Lu 2010 MLT)
#   t_s             — mean T-units per sentence (Hunt 1966 coordination index)

dt_tu <- tunit_features(dt_corpus)
message("T-unit features computed.")
head(dt_tu)


# Adding group variable and pair ids for viki-wiki
dt_tu_grp <- dt_tu |>
  mutate(
    source = factor(
      if_else(str_starts(doc_id, "viki"), "Vikidia", "Wikipedia"),
      levels = c("Vikidia", "Wikipedia")
    ),
    pair_id = sub("^(viki|wiki)_", "", doc_id)
  )


# 6) ALECTOR: paired original vs simplified ----
#
# ALECTOR contains 79 matched pairs: each source text was professionally
# simplified for young readers (target). Because pairs are matched, we use
# paired = TRUE for Cohen's d — this removes between-text variance and gives
# a cleaner measure of the simplification effect.

alector_cache <- "out/demo_alector_tunits_parsed.Rds"

if (file.exists(alector_cache)) {
  dt_alector <- readRDS(alector_cache)
} else {
  # Unzips the bundled demo_corpora/alector.zip on first run.
  dt_alector <- parse_text(build_corpus(ensure_alector_demo_corpus()),
                           n_cores = max(1L, parallel::detectCores() - 1L))
  saveRDS(dt_alector, alector_cache)
}

dt_alector <- dt_alector |> mutate(
  compte  = !upos %in% c("PUNCT", "SYM"),
  version = if_else(grepl("_source", doc_id), "original", "simplified"),
  pair_id = sub("^(\\d+)_.*", "\\1", doc_id)
)

message("ALECTOR: ", nrow(dt_alector), " tokens, ",
        uniqueN(dt_alector$doc_id), " documents.")

dt_tu_alector <- tunit_features(dt_alector) |>
  mutate(
    version = factor(
      if_else(grepl("_source", doc_id), "original", "simplified"),
      levels = c("simplified", "original")
    ),
    pair_id = sub("^(\\d+)_.*", "\\1", doc_id)
  )


# 7) Visualisation ----

# Column renaming: full Lu (2010) 14-measure battery + prop_coord_sent.
# We rename to human-readable labels for the plot facets.
lu_renames <- c(
  "MLS (tokens/sentence)"       = "mls",
  "MLT (tokens/T-unit)"         = "mlt",
  "MLC (tokens/clause)"         = "mlc",
  "C/S (clauses/sentence)"      = "c_s",
  "T/S (T-units/sentence)"      = "t_s",
  "C/T (clauses/T-unit)"        = "c_t",
  "DC/C (dep. clauses/clause)"  = "dc_c",
  "DC/T (dep. clauses/T-unit)"  = "dc_t",
  "CT/T (complex T-unit ratio)" = "ct_t",
  "VP/T (verb phrases/T-unit)"  = "vp_t",
  "CP/T (coord. phrases/T-unit)"= "cp_t",
  "CP/C (coord. phrases/clause)"= "cp_c",
  "CN/T (complex noms/T-unit)"  = "cn_t",
  "CN/C (complex noms/clause)"  = "cn_c",
  "% coord. sentences"          = "prop_coord_sent"
)
lu_labels <- names(lu_renames)

# Vikidia vs Wikipedia (paired by article)
dt_tu_grp |>
  rename(all_of(lu_renames)) |>
  plot_faceted_boxplot(
    source,
    lu_labels,
    title = "T-unit features: Vikidia vs Wikipedia",
    y_lab = NULL,
    paired = TRUE,
    pair_col = "pair_id",
    ncol = 4
  ) |> print()

# ALECTOR original vs simplified (paired by text)
dt_tu_alector |>
  rename(all_of(lu_renames)) |>
  plot_faceted_boxplot(
    version,
    lu_labels,
    title    = "T-unit features: ALECTOR original vs simplified",
    y_lab    = NULL,
    paired   = TRUE,
    pair_col = "pair_id",
    ncol = 4
  ) |> print()
