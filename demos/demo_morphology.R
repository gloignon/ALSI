# Demo: Morphological complexity features
#
# This demo covers two families of morphological complexity features:
#
# 1. MorphoLex-FR word structure (add_morpholex_features)
#    Based on Mailhot, Wilson, Macoir, Deacon & Sánchez-Gutiérrez (2020)
#    MorphoLex-FR, which provides morphemic segmentation and family-size
#    statistics for 38,840 French words.
#    Features: morpheme count, morphological family size, affix presence.
#    These are peer-reviewed database-derived lexical structure measures.
#    The psycholinguistic literature on morphological decomposition and
#    family-size effects motivates their interpretation, but the document-level
#    aggregates below should be treated as corpus features, not validated
#    readability effects by themselves.
#
# 2. UD inflectional profile (inflectional_profile)
#    Parses the UDPipe feats column (e.g. "Mood=Sub|VerbForm=Fin|...") to
#    compute count/proportion summaries over morphosyntactic annotations:
#    grammatical marking density, participial verb forms, subjunctive marking,
#    passive voice tags, and finite-verb mood diversity. These are empirical
#    corpus features derived from UD annotations. Psycholinguistic and L2
#    acquisition work motivates some interpretations (e.g., subjunctive
#    marking), but these aggregates should not be treated as independently
#    validated processing or readability effects by themselves. In particular,
#    the current participle-based feature is a proxy for compound/periphrastic
#    tense use, not a full auxiliary-plus-participle construction detector.
#
# Prerequisites:
#   - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds,
#     the ~400-document Vikidia/Wikipedia corpus used in sections 3–5).
#   - A UDPipe model at models/french_gsd-remix_3.udpipe (for toy sentences
#     in sections 1–2). Only needed if you want to re-parse toy examples.
#   - lexical_dbs/dt_morpholex_fr.Rds — ships with ALSI, or rebuild with
#     Rscript R/artefact_builders/build_morpholex_fr.R
#
# Last update: 2026-06-04


# 1) Setup ----

library(tidyverse)
library(data.table)
library(udpipe)
library(effsize)  # for cohen.d()

source("R/fnt_corpus.R",      encoding = "UTF-8")
source("R/fnt_fr_norms.R",    encoding = "UTF-8")
source("R/fnt_morphology.R",  encoding = "UTF-8")
source("R/fnt_utility.R",     encoding = "UTF-8")

dir.create("out", showWarnings = FALSE)

udmodel_french <- udpipe_load_model("models/french_gsd-remix_3.udpipe")

# Helper: parse one or more sentences and add the 'compte' column.
# compte == TRUE marks tokens that count toward length (excludes PUNCT, SYM).
parse_sentences <- function(text, model) {
  dt <- udpipe(text, object = model) |> as.data.table()
  dt[, compte := !upos %in% c("PUNCT", "SYM")]
  return(dt)
}

# Load MorphoLex-FR (ships as a pre-built .Rds in lexical_dbs/).
dt_morpholex <- readRDS("lexical_dbs/dt_morpholex_fr.Rds")
message(sprintf("MorphoLex-FR: %d word forms", nrow(dt_morpholex)))


# 2) MorphoLex-FR inspection ----
#
# MorphoLex-FR provides morphemic segmentation (canon_segm) and morphological
# family sizes for 38,840 French word forms.
# Notation in canon_segm: (root), <prefix<, >suffix>, [verbal-suffix]
#
# Key variables:
#   n_morphemes    — total number of morphemes (root + affixes)
#   root_fam_size  — how many distinct words share the same root
#                    (a peer-reviewed MorphoLex-FR family-size variable)
#   pref_fam_size  — family size of prefix 1 (NA = no prefix)
#   suff_fam_size  — family size of suffix 1 (NA = no suffix)

# Simple monomorphemic words — 1 morpheme, often high root family size.
simple_words <- c("chat", "maison", "eau", "blanc", "vite")
print(dt_morpholex[word %in% simple_words,
                   .(word, n_morphemes, canon_segm, root_fam_size)])

# Complex words — multiple morphemes; note how prefixes and suffixes are marked.
# "développement": (développ-) + >ement> — nominalising suffix
# "incompréhensible": <in< + (compreh-) + >ible> — prefix + suffix
# "réorganisation": <ré< + (organ-) + >ation> — prefix + nominaliser
complex_words <- c("développement", "incompréhensible", "réorganisation",
                   "abaissement", "incomplet")
print(dt_morpholex[word %in% complex_words,
                   .(word, n_morphemes, canon_segm, root_fam_size,
                     pref_fam_size, suff_fam_size)])

# Distribution of morpheme counts in the full database.
# Most words are mono- or bimorphemic; highly complex words are rare.
dt_morpholex |>
  as_tibble() |>
  count(n_morphemes) |>
  mutate(prop = n / sum(n)) |>
  print()


# 3) Inflectional profile on toy sentences ----
#
# We contrast a morphologically lean sentence (simple indicative, no periphrasis)
# with a morphologically rich one (subjunctive, compound tense, passive).
# inflectional_profile() counts grammatical feature markers in the feats column.

# 3a) Simple sentence — present indicative, no compound tense, no subjunctive.
#     Expected: low morph_feats_per_word, simple verb morphology.
sentence_simple <- "Le chat mange la souris dans le jardin."
dt_simple <- parse_sentences(sentence_simple, udmodel_french)
message("\n--- Simple sentence feats ---")
print(dt_simple[compte == TRUE, .(token, upos, feats)])
print(inflectional_profile(dt_simple))

# 3b) Complex sentence — subjunctive after "vouloir que", compound past tense,
#     and a passive construction. Note: French GSD may not tag "soit parti"
#     as passive in all contexts — see the feats column to verify.
#     Expected: higher morph_feats_per_word, morph_prop_compound_tense > 0,
#               morph_prop_passive > 0.
sentence_complex <- paste(
  "Il faut que tu aies remarqué que la fenêtre avait été fermée",
  "par quelqu'un avant que nous soyons arrivés."
)
dt_complex <- parse_sentences(sentence_complex, udmodel_french)
message("\n--- Complex sentence feats ---")
print(dt_complex[compte == TRUE & upos %in% c("VERB", "AUX"),
                 .(token, upos, feats)])
print(inflectional_profile(dt_complex))


# 4) MorphoLex join on toy sentences ----
#
# add_morpholex_features() joins by inflected token form first, then falls
# back to the lemma. morph_match_rate tells you what fraction of content words
# were found in the database.

# Give each toy sentence its own doc_id for the join.
dt_toy <- rbindlist(list(
  dt_simple[,  doc_id := "simple"],
  dt_complex[, doc_id := "complex"]
))

df_mlex_toy <- add_morpholex_features(dt_toy, dt_morpholex)
print(df_mlex_toy)

# Interpretation:
#   morph_mean_n_morphemes   — more morphemes in the complex sentence
#   morph_prop_complex       — larger share of morphologically complex words
#   morph_mean_root_famsize  — average root family size (log scale)


# 5) Full corpus: Vikidia vs Wikipedia ----
#
# Vikidia is the simplified (child-oriented) French encyclopedia.
# Wikipedia is the original adult text.
# We expect Vikidia to be morphologically simpler on most features.

dt_corpus <- as.data.table(readRDS("out/demo_parsed_tagged.Rds"))
message(sprintf("\nLoaded corpus: %d documents", uniqueN(dt_corpus$doc_id)))

# Label: viki_ prefix = Vikidia (simplified), everything else = Wikipedia.
dt_labels <- unique(dt_corpus[, .(
  doc_id,
  source = ifelse(grepl("^viki_", doc_id), "Vikidia", "Wikipedia")
)])

# Compute all morphological features. This may take 30–60 seconds.
message("Computing morphological features...")
df_morph <- merge(add_morpholex_features(dt_corpus, dt_morpholex),
                  inflectional_profile(dt_corpus),
                  by = "doc_id") |>
  as_tibble() |>
  merge(as_tibble(dt_labels), by = "doc_id")
message(sprintf("Features computed: %d documents × %d feature columns",
                nrow(df_morph), ncol(df_morph) - 2L))

# Quick per-group means for all morph_* columns.
morph_cols <- names(df_morph)[startsWith(names(df_morph), "morph_")]
df_morph |>
  group_by(source) |>
  summarise(across(all_of(morph_cols), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(-source, names_to = "feature", values_to = "mean") |>
  pivot_wider(names_from = source, values_from = mean) |>
  mutate(wiki_minus_viki = Wikipedia - Vikidia) |>
  arrange(desc(abs(wiki_minus_viki))) |>
  print(n = Inf)


# 6) Effect sizes ----
#
# Cohen's d: how well does each feature separate the two text varieties?
# Positive d = Wikipedia (adult) scores higher (= more complex).
# Negative d = Vikidia scores higher.
#
# Note on morph_feats_per_word direction:
#   This feature can go negative (Vikidia > Wikipedia) despite Wikipedia being
#   more complex. The reason is a register confound: morph_feats_per_word is a
#   document mean over all feats-bearing tokens. Verbs carry ~3.7–3.8 features
#   each, nouns only ~2.0. Vikidia's narrative style uses more finite verbs
#   (~11.5% of tokens vs ~9.2% in Wikipedia), which inflates its document mean
#   even though each individual verb is simpler. This is the "nominal style"
#   effect: encyclopedic text packs complexity into noun phrases, not verb
#   morphology (Biber 1988, Variation across Speech and Writing).
#   Use morph_verb_feat_density for a UPOS-controlled measure of verb complexity.

df_cohen <- data.frame(
  feature = morph_cols,
  cohen_d = purrr::map_dbl(morph_cols, function(f) {
    x_viki <- df_morph[[f]][df_morph$source == "Vikidia"]
    x_wiki <- df_morph[[f]][df_morph$source == "Wikipedia"]
    tryCatch(
      cohen.d(x_wiki, x_viki, na.rm = TRUE)$estimate,
      error = function(e) NA_real_
    )
  })
) |>
  arrange(desc(abs(cohen_d)))

print(df_cohen)


# 7) Faceted boxplot ----
#
# One panel per feature; each box shows the distribution across documents.
# Cohen's d is annotated in the top-right corner of each facet.
# Features like morph_prop_compound_tense and morph_mean_n_morphemes should
# show clear separation; morph_feats_per_word may go in the unexpected
# direction for the register reason documented in section 6.

p_box <- plot_faceted_boxplot(
  df        = df_morph,
  group_col = source,
  title     = "Morphological complexity: Wikipedia vs Vikidia",
  y_lab     = NULL,
  ncol      = 4,
  show_d    = TRUE
)
print(p_box)
ggsave("out/demo_morphology_boxplots.pdf", p_box, width = 14, height = 10)
message("Boxplot saved to out/demo_morphology_boxplots.pdf")


# 8) Cohen's d bar chart ----
#
# Summary of effect sizes ranked by magnitude.
# Features where |d| > 0.5 are reasonable candidates for a readability model.

df_cohen |>
  filter(!is.na(cohen_d)) |>
  mutate(
    feature = sub("morph_", "", feature),
    direction = ifelse(cohen_d > 0, "Wikipedia > Vikidia", "Vikidia > Wikipedia")
  ) |>
  ggplot(aes(x = reorder(feature, cohen_d), y = cohen_d, fill = direction)) +
  geom_col() +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("Wikipedia > Vikidia" = "#2166ac",
                                "Vikidia > Wikipedia" = "#d6604d")) +
  labs(
    title = "Morphological complexity: Wikipedia vs Vikidia",
    subtitle = "Cohen's d (positive = Wikipedia more complex)",
    x = NULL, y = "Cohen's d", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("out/demo_morphology_effect_sizes.pdf", width = 8, height = 6)
message("Bar chart saved to out/demo_morphology_effect_sizes.pdf")


# 9) ALECTOR corpus: source vs simplified ----
#
# ALECTOR (Gala et al. 2020) is a parallel corpus of 79 French text pairs:
# each original (source) text has been manually simplified (target).
# Unlike Vikidia/Wikipedia (both encyclopedic), ALECTOR spans genres
# (narrative and expository). This provides a second independent evaluation
# of whether morphological features discriminate simplified from original text.
#
# The ALECTOR corpus is distributed with ALSI (out/alector_parsed.Rds).
# Citation: Gala, N., Tack, A., Javourey-Drevet, L., François, T., & Ziegler, J. C. (2020).
#   Alector: A Parallel Corpus of Simplified French Texts with Alignments
#   of Misreadings by Poor and Dyslexic Readers. In LREC 2020.

message("\nLoading ALECTOR corpus...")
dt_alector <- as.data.table(readRDS("out/alector_parsed.Rds"))

# doc_id format: "alector_source_NN" or "alector_target_NN"
dt_alector[, source := ifelse(grepl("_source_", doc_id), "source", "target")]
message(sprintf("  %d documents", uniqueN(dt_alector$doc_id)))

# Compute morphological features for ALECTOR.
df_alector_morph <- merge(
  add_morpholex_features(dt_alector, dt_morpholex),
  inflectional_profile(dt_alector),
  by = "doc_id"
) |>
  as_tibble() |>
  merge(
    unique(dt_alector[, .(doc_id, source)]),
    by = "doc_id"
  )

message(sprintf("  Features computed: %d documents × %d columns",
                nrow(df_alector_morph),
                ncol(df_alector_morph) - 2L))

# Per-source means.
morph_cols_alector <- names(df_alector_morph)[startsWith(names(df_alector_morph), "morph_")]

df_alector_means <- df_alector_morph |>
  group_by(source) |>
  summarise(across(all_of(morph_cols_alector), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(-source, names_to = "feature", values_to = "mean") |>
  pivot_wider(names_from = source, values_from = mean) |>
  mutate(source_minus_target = source - target) |>
  arrange(desc(abs(source_minus_target)))

print(df_alector_means)

# Effect sizes (Cohen's d) for ALECTOR.
df_alector_cohen <- data.frame(
  feature = morph_cols_alector,
  cohen_d = purrr::map_dbl(morph_cols_alector, function(f) {
    x_source <- df_alector_morph[[f]][df_alector_morph$source == "source"]
    x_target <- df_alector_morph[[f]][df_alector_morph$source == "target"]
    tryCatch(
      cohen.d(x_source, x_target, na.rm = TRUE)$estimate,
      error = function(e) NA_real_
    )
  })
) |>
  arrange(desc(abs(cohen_d)))

print(df_alector_cohen)

# Boxplot comparison.
p_alector_box <- plot_faceted_boxplot(
  df        = df_alector_morph,
  group_col = source,
  title     = "Morphological complexity: ALECTOR source vs simplified",
  y_lab     = NULL,
  ncol      = 4,
  show_d    = TRUE
)
print(p_alector_box)
ggsave("out/demo_morphology_alector_boxplots.pdf", p_alector_box, width = 14, height = 10)
message("ALECTOR boxplot saved to out/demo_morphology_alector_boxplots.pdf")

# Bar chart of effect sizes.
df_alector_cohen |>
  filter(!is.na(cohen_d)) |>
  mutate(
    feature = sub("morph_", "", feature),
    direction = ifelse(cohen_d > 0, "Source > Target", "Target > Source")
  ) |>
  ggplot(aes(x = reorder(feature, cohen_d), y = cohen_d, fill = direction)) +
  geom_col() +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("Source > Target" = "#2166ac",
                                "Target > Source" = "#d6604d")) +
  labs(
    title = "Morphological complexity: ALECTOR source vs simplified",
    subtitle = "Cohen's d (positive = source more complex)",
    x = NULL, y = "Cohen's d", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("out/demo_morphology_alector_effect_sizes.pdf", width = 8, height = 6)
message("ALECTOR effect-size chart saved to out/demo_morphology_alector_effect_sizes.pdf")
