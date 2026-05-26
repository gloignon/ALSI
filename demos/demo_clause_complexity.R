# Demo: extra syntactic complexity features
#
# In this demo you will:
# 1) inspect features on individual sentences (French then English);
# 2) see how each construction type affects the counts;
# 3) compute document-level features on the parsed demo corpus;
# 4) compare Vikidia (easy) vs Wikipedia (hard) with a quick summary.
#
# Operationalization follows Lu (2010), translated to Universal Dependencies:
#   - Clausal density  : clause-indicator dep_rels per sentence
#   - Complex nominals : NOUN heads with a substantive modifier child
#   - Complex verbs    : VERB heads with an aux / aux:pass child
#   - Dep. distance    : mean |position(head) - position(dependent)|  [Liu 2008]
#
# Prerequisite:
# - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).
# - A UDPipe model must be available at models/french_gsd-remix_3.udpipe.
# - For English sentences: install the English UD model once with
#     udpipe_download_model("english-ewt", model_dir = "models/")
#   then adjust the path below.

# 0) Setup ----

library(tidyverse)
library(udpipe)

source("R/fnt_corpus.R",       encoding = "UTF-8")
source("R/fnt_extra_syntax.R", encoding = "UTF-8")
source("R/fnt_utility.R",      encoding = "UTF-8")

dir.create("out", showWarnings = FALSE)

udmodel_french <- udpipe_load_model("models/french_gsd-remix_3.udpipe")

english_path <- list.files("models", "english.*udpipe$", full.names = TRUE)[1]
if (is.na(english_path)) {
  message("English UDPipe model not found — downloading to models/ (one-time, ~16 MB)...")
  english_path <- udpipe_download_model("english-ewt", model_dir = "models/")$file_model
}
udmodel_english <- udpipe_load_model(english_path)

# Helper: parse one sentence and add the two columns the function needs.
parse_sentence <- function(text, model) {
  udpipe(text, object = model) |>
    as_tibble() |>
    mutate(
      feats  = as.character(feats),
      compte = !upos %in% c("PUNCT", "SYM")
    )
}

# Helper: run features and return a tidy one-row tibble (no doc_id).
get_features <- function(dt) {
  extra_syntactic_features(dt) |>
    select(-doc_id) |>
    mutate(across(everything(), \(x) round(x, 2)))
}


# 1) French sentences ----

# ── 1a. Simple clause ────────────────────────────────────────────────────────
#
# "Le chat mange."
# One clause. 'mange' has no aux → not a complex verb.
# Note: whether 'chat' counts as a complex nominal depends on what the
# French GSD parser attaches to it (sometimes a spurious nmod).

message("--- FR simple: 'Le chat mange.' ---")
dt_fr_simple <- parse_sentence("Le chat mange.", udmodel_french)
get_features(dt_fr_simple) |> print()

# ── 1b. Modified noun (amod) ─────────────────────────────────────────────────
#
# "Le grand chat noir mange lentement."
# 'chat' has two amod children (grand, noir) → 1 complex nominal.

message("--- FR modified noun: 'Le grand chat noir mange lentement.' ---")
dt_fr_noun <- parse_sentence("Le grand chat noir mange lentement.", udmodel_french)
get_features(dt_fr_noun) |> print()

# ── 1c. Post-nominal modifier (nmod) ─────────────────────────────────────────
#
# "Le chat de la voisine mange."
# 'chat' has an nmod child (voisine) → 1 complex nominal.
# Dependency distance is also longer because head and dependent are further apart.

message("--- FR nmod: 'Le chat de la voisine mange.' ---")
dt_fr_nmod <- parse_sentence("Le chat de la voisine mange.", udmodel_french)
get_features(dt_fr_nmod) |> print()

# ── 1d. Periphrastic tense (complex verb) ────────────────────────────────────
#
# "Le chat a mangé la souris."
# 'mangé' has aux 'a' → 1 complex verb. Clausal density stays at 1.

message("--- FR complex verb: 'Le chat a mangé la souris.' ---")
dt_fr_cv <- parse_sentence("Le chat a mangé la souris.", udmodel_french)
get_features(dt_fr_cv) |> print()

# ── 1e. Embedded complement clause (ccomp) ───────────────────────────────────
#
# "Je pense que le chat a mangé la souris."
# ccomp adds one clause → clausal_density = 2.
# Note: the French GSD model sometimes tags 'a' (avoir) as VERB rather than
# AUX, so complex_verb may be 0 even though the construction is periphrastic.

message("--- FR ccomp: 'Je pense que le chat a mangé la souris.' ---")
dt_fr_ccomp <- parse_sentence("Je pense que le chat a mangé la souris.",
                              udmodel_french)
get_features(dt_fr_ccomp) |> print()

# ── 1f. Relative clause (acl:relcl) ─────────────────────────────────────────
#
# "Le livre que la professeure a écrit est fascinant."
# 'livre' has acl:relcl child → 1 complex nominal.
# 'écrit' has aux 'a' → 1 complex verb.
# acl:relcl adds one clause → clausal_density = 2.

message("--- FR relative: 'Le livre que la professeure a écrit est fascinant.' ---")
dt_fr_relcl <- parse_sentence("Le livre que la professeure a écrit est fascinant.",
                              udmodel_french)
get_features(dt_fr_relcl) |> print()

# ── 1g. Adverbial clause (advcl) ─────────────────────────────────────────────
#
# "Il mange parce qu'il a faim."
# advcl adds one clause → clausal_density = 2.

message("--- FR advcl: \"Il mange parce qu'il a faim.\" ---")
dt_fr_advcl <- parse_sentence("Il mange parce qu'il a faim.", udmodel_french)
get_features(dt_fr_advcl) |> print()


# 2) English sentences ----

# ── 2a. Simple clause ────────────────────────────────────────────────────────

message("--- EN simple: 'The dog barks.' ---")
dt_en_simple <- parse_sentence("The dog barks.", udmodel_english)
get_features(dt_en_simple) |> print()

# ── 2b. Modified noun ────────────────────────────────────────────────────────
#
# 'dog' has two amod children (big, black) → 1 complex nominal.

message("--- EN modified noun: 'The big black dog barks loudly.' ---")
dt_en_noun <- parse_sentence("The big black dog barks loudly.", udmodel_english)
get_features(dt_en_noun) |> print()

# ── 2c. Modal auxiliary ──────────────────────────────────────────────────────
#
# 'run' has aux 'can' → 1 complex verb.

message("--- EN modal: 'The dog can run fast.' ---")
dt_en_modal <- parse_sentence("The dog can run fast.", udmodel_english)
get_features(dt_en_modal) |> print()

# ── 2d. Passive ──────────────────────────────────────────────────────────────
#
# 'chased' has aux:pass 'was' → 1 complex verb.

message("--- EN passive: 'The cat was chased by the dog.' ---")
dt_en_passive <- parse_sentence("The cat was chased by the dog.", udmodel_english)
get_features(dt_en_passive) |> print()

# ── 2e. Embedded complement clause (ccomp) ───────────────────────────────────
#
# ccomp adds one clause → clausal_density = 2.

message("--- EN ccomp: 'She knows that he has been running.' ---")
dt_en_ccomp <- parse_sentence("She knows that he has been running.", udmodel_english)
get_features(dt_en_ccomp) |> print()

# ── 2f. Relative clause (acl:relcl) ─────────────────────────────────────────
#
# 'book' has acl:relcl child → 1 complex nominal.

message("--- EN relative: 'The book that she wrote is good.' ---")
dt_en_relcl <- parse_sentence("The book that she wrote is good.", udmodel_english)
get_features(dt_en_relcl) |> print()


# 3) Side-by-side comparison of all sentence types ----

sentences <- lst(
  "FR: simple"       = dt_fr_simple,
  "FR: mod. noun"    = dt_fr_noun,
  "FR: nmod"         = dt_fr_nmod,
  "FR: periphrastic" = dt_fr_cv,
  "FR: ccomp"        = dt_fr_ccomp,
  "FR: relative"     = dt_fr_relcl,
  "FR: advcl"        = dt_fr_advcl,
  "EN: simple"       = dt_en_simple,
  "EN: mod. noun"    = dt_en_noun,
  "EN: modal"        = dt_en_modal,
  "EN: passive"      = dt_en_passive,
  "EN: ccomp"        = dt_en_ccomp,
  "EN: relative"     = dt_en_relcl
)

dt_all <- imap(sentences, \(dt, nm) mutate(dt, doc_id = nm)) |>
  list_rbind()

dt_comparison <- extra_syntactic_features(dt_all) |>
  rename(sentence = doc_id) |>
  mutate(across(where(is.numeric), \(x) round(x, 2)))

print(dt_comparison, n = Inf)


# 4) Document-level features on the demo corpus ----

message("Loading parsed corpus...")
dt_corpus <- readRDS("out/demo_parsed_tagged.Rds") |>
  as_tibble() |>
  mutate(compte = !upos %in% c("PUNCT", "SYM"))

dt_syntax <- extra_syntactic_features(dt_corpus) |>
  mutate(source = if_else(str_detect(doc_id, "viki"), "Vikidia", "Wikipedia"))

message("Means per source:")
dt_syntax |>
  group_by(source) |>
  summarise(across(where(is.numeric), \(x) round(mean(x, na.rm = TRUE), 2))) |>
  print()


# 5) Boxplot: key features by source ----
#
# The per-clause ratios (dc_per_clause, cn_per_clause, cv_per_clause) normalize
# CN and CV counts by clausal structure rather than sentence count — useful when
# documents vary in sentence length.

plot_faceted_boxplot(
  dt_syntax, source,
  c(clausal_density, dc_per_clause, cn_per_clause, cv_per_clause, avg_dep_dist),
  title = "Extra syntactic features: Vikidia vs Wikipedia",
  y_lab = NULL
)
