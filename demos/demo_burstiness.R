# Demo: Word burstiness features
#
# Scores Alector (79 original/simplified French text pairs) against
# pre-built wikiviki burstiness norms. Run R/artefact_builders/build_wikiviki_norms.R
# once before running this demo.
#
# References:
#   Altmann, Pierrehumbert & Motter (2009), PLoS ONE 4(11): e7678
#   Church & Gale (1995), Natural Language Engineering 1(2): 163-190
#
library(data.table)
library(udpipe)

source("R/fnt_corpus.R",     encoding = "UTF-8")
source("R/fnt_burstiness.R", encoding = "UTF-8")
source("R/fnt_utility.R",    encoding = "UTF-8")

dir.create("out", showWarnings = FALSE)


# ── 1. Load reference norms ───────────────────────────────────────────────────

norms     <- readRDS("norms/burstiness_viki.Rds")
beta_ref  <- norms$beta
adapt_ref <- norms$adaptation

message(sprintf("Norms: %d words with beta, %d with adaptation score",
                sum(!is.na(beta_ref$beta)), sum(!is.na(adapt_ref$adaptation))))


# ── 2. Parse Alector ──────────────────────────────────────────────────────────

cache <- "out/demo_burstiness_alector_parsed.Rds"

if (file.exists(cache)) {
  dt_alector <- readRDS(cache)
} else {
  message("Parsing Alector with UDPipe...")
  dt_alector <- parse_text(build_corpus("demo_corpora/alector/"),
                            n_cores = max(1L, parallel::detectCores() - 1L))
  saveRDS(dt_alector, cache)
}

dt_alector[, version := fifelse(grepl("_source", doc_id), "original", "simplified")]
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


# ── 4. Compare original vs simplified (paired by text pair) ──────────────────

# Extract pair index from doc_id (e.g. "alector_source_03" -> "03")
doc_burst[, pair_id := sub("^(\\d+)_.*", "\\1", doc_id)]

# Paired rank-biserial r: effect size for paired Wilcoxon
paired_r_rb <- function(d) {
  n <- length(d)
  w <- wilcox.test(d, exact = FALSE)$statistic
  1 - 2 * w / (n * (n + 1) / 2)
}

metrics <- c(
  mean_beta               = "Mean β (all)",
  prop_bursty             = "Prop. bursty (all)",
  mean_adaptation         = "Mean adaptation (all)",
  mean_beta_content       = "Mean β (content)",
  prop_bursty_content     = "Prop. bursty (content)",
  mean_adaptation_content = "Mean adaptation (content)"
)

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


# ── 5. Faceted boxplot ────────────────────────────────────────────────────────

library(tidyverse)

doc_burst |>
  as_tibble() |>
  select(version, all_of(names(metrics))) |>
  plot_faceted_boxplot(version, all_of(names(metrics)),
    title = "Burstiness: Alector original vs simplified (wikiviki norms)",
    y_lab = NULL, ncol = 3, notch = TRUE
  )
