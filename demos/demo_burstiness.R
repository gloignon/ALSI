# Demo: Word burstiness features
#
# Scores Alector (79 original/simplified French text pairs) against
# pre-built wikiviki burstiness norms. Reuses the parsed ALECTOR corpus cached
# at out/alector_parsed.Rds by demo_surprisal.R (parses it if not yet cached).
#
# Prerequisites:
# - norms/burstiness_viki.Rds — pre-built burstiness reference norms, bundled
#   with ALSI. (They are aggregate per-word statistics fitted on 12,221
#   Vikidia articles; the underlying corpus is not distributed.)
#
# References:
#   Altmann, Pierrehumbert & Motter (2009), PLoS ONE 4(11): e7678
#   Church & Gale (1995), Natural Language Engineering 1(2): 163-190
#   Goh & Barabási (2008), Europhys Lett 81: 48002 (within-document B statistic)
#
library(data.table)
library(udpipe)
library(tidyverse)

source("R/fnt_corpus.R",     encoding = "UTF-8")
source("R/fnt_burstiness.R", encoding = "UTF-8")
source("R/fnt_utility.R",    encoding = "UTF-8")
source("R/fnt_setup.R",      encoding = "UTF-8")  # ensure_alector_demo_corpus()

dir.create("out", showWarnings = FALSE)


# ── 1. Load reference norms ───────────────────────────────────────────────────

if (!file.exists("norms/burstiness_viki.Rds")) {
  stop(
    "norms/burstiness_viki.Rds not found. The norms ship with ALSI — ",
    "your copy of the repository may be incomplete.",
    call. = FALSE
  )
}
norms     <- readRDS("norms/burstiness_viki.Rds")
beta_ref  <- norms$beta
adapt_ref <- norms$adaptation

message(sprintf("Norms: %d words with beta, %d with adaptation score",
                sum(!is.na(beta_ref$beta)), sum(!is.na(adapt_ref$adaptation))))


# ── 2. Load parsed Alector ────────────────────────────────────────────────────

# Several demos (demo_surprisal.R, demo_cohesion.R, demo_mwe_matching.R) parse
# and cache the same ALECTOR corpus to out/alector_parsed.Rds. Reuse it here
# rather than parsing (and caching) our own copy.
cache_alector_parsed <- "out/alector_parsed.Rds"

if (file.exists(cache_alector_parsed)) {
  message("Loading cached ALECTOR parsed corpus")
  dt_alector <- readRDS(cache_alector_parsed)
} else {
  message("Parsing Alector with UDPipe...")
  # Unzips the bundled demo_corpora/alector.zip on first run.
  dt_alector_raw <- parse_text(build_corpus(ensure_alector_demo_corpus()),
                               n_cores = max(1L, parallel::detectCores() - 1L))
  dt_alector     <- post_process_lexicon(dt_alector_raw)
  saveRDS(dt_alector, cache_alector_parsed)
  message("Saved to ", cache_alector_parsed)
}

dt_alector <- dt_alector |>
  mutate(version = if_else(grepl("_source", doc_id), "original", "simplified"))
message(sprintf("Alector: %d tokens, %d documents", nrow(dt_alector), uniqueN(dt_alector$doc_id)))


# ── 3. Score documents ────────────────────────────────────────────────────────

word_props <- merge(beta_ref[,  .(word, beta)],
                    adapt_ref[, .(word, adaptation)],
                    by = "word", all = TRUE)

dt_scored <- merge(
  dt_alector[, .(doc_id, token = tolower(token), upos, version)],
  word_props, by.x = "token", by.y = "word", all.x = TRUE
)

content_upos <- c("NOUN", "VERB", "ADJ", "ADV")

doc_burst <- dt_scored[, .(
  mean_beta         = mean(beta,        na.rm = TRUE),
  prop_bursty       = mean(beta < 0.6,  na.rm = TRUE),
  mean_adaptation   = mean(adaptation,  na.rm = TRUE)
), by = .(doc_id, version)]

doc_burst <- merge(doc_burst,
  dt_scored[upos %in% content_upos, .(
    mean_beta_content       = mean(beta,       na.rm = TRUE),
    prop_bursty_content     = mean(beta < 0.6, na.rm = TRUE),
    mean_adaptation_content = mean(adaptation, na.rm = TRUE)
  ), by = doc_id],
  by = "doc_id", all.x = TRUE)


# ── 4. Within-document burstiness (no reference norms needed) ────────────────

# burstiness_within_doc() derives the Goh-Barabási B statistic for each word
# from its own gap sequence inside each document — unlike mean_beta and
# mean_adaptation above, it needs no wikiviki reference norms at all.
doc_burst_B <- burstiness_within_doc(dt_alector, content_upos = content_upos)

doc_burst <- merge(doc_burst, doc_burst_B, by = "doc_id", all.x = TRUE)


# ── 5. Compare original vs simplified (paired by text pair) ──────────────────

# Extract pair index from doc_id (e.g. "alector_source_03" -> "03")
doc_burst <- doc_burst |> mutate(pair_id = sub(".*_(\\d+)$", "\\1", doc_id))

# Paired rank-biserial r: effect size for paired Wilcoxon
paired_r_rb <- function(d) {
  n <- length(d)
  w <- wilcox.test(d, exact = FALSE)$statistic
  1 - 2 * w / (n * (n + 1) / 2)
}

# Metrics computed against the wikiviki reference norms (need beta_ref/adapt_ref).
metrics_norm <- c(
  mean_beta               = "Mean β (all)",
  prop_bursty             = "Prop. bursty (all)",
  mean_adaptation         = "Mean adaptation (all)",
  mean_beta_content       = "Mean β (content)",
  prop_bursty_content     = "Prop. bursty (content)",
  mean_adaptation_content = "Mean adaptation (content)"
)

# Metrics computed purely within each document (no reference norms needed).
metrics_within <- c(
  mean_B         = "Mean B, within-doc (all)",
  mean_B_content = "Mean B, within-doc (content)"
)

metrics <- c(metrics_norm, metrics_within)

results <- doc_burst |>
  as_tibble() |>
  select(pair_id, version, all_of(names(metrics))) |>
  pivot_longer(all_of(names(metrics)), names_to = "metric", values_to = "value") |>
  pivot_wider(names_from = version, values_from = value) |>
  drop_na() |>
  group_by(metric) |>
  summarise(
    M_orig   = round(mean(original),   3),
    M_simp   = round(mean(simplified), 3),
    cohens_d = round(mean(original - simplified) / sd(original - simplified), 3),
    r_rb     = round(paired_r_rb(original - simplified), 3),
    p_value  = round(wilcox.test(original, simplified, paired = TRUE, exact = FALSE)$p.value, 4),
    .groups  = "drop"
  )

print(results)


# ── 6. Faceted boxplots ───────────────────────────────────────────────────────

# Plot the two metric families separately, with feature_labels giving each
# facet a human-readable title — this makes it unambiguous which features
# rely on the wikiviki reference norms and which are computed within-document.

doc_burst |>
  as_tibble() |>
  select(version, all_of(names(metrics_norm))) |>
  plot_faceted_boxplot(version, all_of(names(metrics_norm)),
    feature_labels = metrics_norm,
    title = "Burstiness vs wikiviki reference norms: Alector original vs simplified",
    y_lab = NULL, ncol = 3, notch = TRUE
  )

doc_burst |>
  as_tibble() |>
  select(version, all_of(names(metrics_within))) |>
  plot_faceted_boxplot(version, all_of(names(metrics_within)),
    feature_labels = metrics_within,
    title = "Within-document burstiness, no reference norms: Alector original vs simplified",
    y_lab = NULL, ncol = 2, notch = TRUE
  )
