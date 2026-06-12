# ALSI Demo: LLM Surprisal and Entropy (Masked Language Model)
#
# "Surprisal" measures how unexpected each word is given its context.
# A masked language model (MLM, e.g. CamemBERT) predicts the probability
# of each word given the words around it. Surprisal = -log2(probability):
# a common word in a typical context gets low surprisal; a rare or
# unexpected word gets high surprisal.
#
# "Entropy" is a complementary measure: it captures how uncertain the model
# is about what word *could* come next, regardless of which word actually did.
# High entropy = many plausible continuations; low entropy = constrained context.
#
# Simpler texts (e.g. children's texts) tend to use more predictable words →
# lower mean surprisal. This demo uses the ALECTOR corpus: 79 pairs of French
# texts for children, each consisting of an original source text and a
# simplified target version.
#
# In this demo you will:
#   1) download the ALECTOR corpus (if not already cached);
#   2) parse it with UDPipe;
#   3) compute MLM surprisal and entropy with CamemBERT;
#   4) aggregate to document level and compare source vs target;
#   5) visualise with boxplots and print a summary statistics table.
#
# Prerequisites:
#   - Python with transformers, torch, tokenizers, numpy (auto-installed via py_require)
#   - Internet access for first download of corpus and model
#   - models/french_gsd-remix_3.udpipe
#   - Best run in a fresh R session: reticulate locks parts of the Python
#     configuration once Python starts, so a session that already ran another
#     Python-based demo can fail to add packages.
#
# Note: computing surprisal on 158 documents takes several minutes on CPU;
#   much faster with a GPU. Results are cached in out/ to avoid recomputing.

library(tidyverse)
library(data.table)
library(reticulate)  # bridges R and Python
library(udpipe)

# py_require() automatically installs the listed Python packages if they are
# not already present. You do not need to manage a virtual environment manually.
# action = "add" is required (rather than the default "set") because Python
# may already be initialised by a prior demo in the same R session — the
# default action is only allowed before initialisation.
py_require(c(
  "transformers>=4.41,<5",
  "torch",
  "tokenizers",
  "numpy"
), action = "add")

source("R/fnt_surprisal.R", encoding = "UTF-8")
source("R/fnt_corpus.R",    encoding = "UTF-8")
source("R/fnt_utility.R",   encoding = "UTF-8")


# 1) Download ALECTOR (if not already present) ----
#
# ALECTOR contains 79 paired texts: each pair has an original (source)
# and a simplified version (target) written for primary-school children.
# Files are named 000_source.txt, 000_target.txt, 001_source.txt, etc.

alector_dir  <- "out/alector_corpus"
alector_base <- "https://raw.githubusercontent.com/gloignon/alector_corpus/master"
n_alector    <- 79

dir.create(alector_dir, recursive = TRUE, showWarnings = FALSE)

# Build a table listing every file: 79 pairs × 2 variants = 158 rows.
alector_files <- tibble(
  i      = rep(0:(n_alector - 1), each = 2),
  suffix = rep(c("source", "target"), times = n_alector)
) |>
  mutate(
    fname  = sprintf("%03d_%s.txt", i, suffix),
    dest   = file.path(alector_dir, fname),
    url    = sprintf("%s/corpus/%s", alector_base, fname),
    doc_id = sprintf("alector_%s_%02d", suffix, i)
  )

# Only download files that are not already on disk.
alector_files |>
  filter(!file.exists(dest)) |>
  pwalk(function(url, dest, fname, ...) {
    tryCatch(
      download.file(url, dest, quiet = TRUE),
      error = function(e) warning(sprintf("Download failed: %s", fname))
    )
  })


# 2) Parse ALECTOR with UDPipe ----
#
# Parsing is slow (several minutes for 158 documents), so we cache the
# result. On subsequent runs this block is skipped and the cache is loaded.

cache_alector_parsed <- "out/alector_parsed.Rds"

if (file.exists(cache_alector_parsed)) {
  message("Loading cached ALECTOR parsed corpus")
  dt_alector <- readRDS(cache_alector_parsed)
} else {
  message("Parsing ALECTOR corpus with UDPipe (this may take a few minutes)...")

  # Read every file into a data.table with one row per document.
  # rowwise() + mutate() allows reading each file's text inline.
  dt_alector_txt <- alector_files |>
    filter(file.exists(dest)) |>
    rowwise() |>
    mutate(text = paste(readLines(dest, encoding = "UTF-8", warn = FALSE), collapse = " ")) |>
    ungroup() |>
    select(doc_id, text) |>
    as_tibble()

  message(nrow(dt_alector_txt), " documents loaded")

  n_cores        <- max(1, parallel::detectCores() - 1)
  dt_alector_raw <- parse_text(dt_alector_txt, n_cores = n_cores)
  dt_alector     <- post_process_lexicon(dt_alector_raw)
  saveRDS(dt_alector, cache_alector_parsed)
  message("Saved to ", cache_alector_parsed)
}

message("ALECTOR parsed: ", nrow(dt_alector), " tokens, ",
        n_distinct(dt_alector$doc_id), " documents")


# 3) Compute MLM surprisal on ALECTOR ----
#
# llm_surprisal_entropy() returns a token-level data.table with columns:
#   llm_surprisal — -log2 probability of this token given its context
#   llm_entropy   — model uncertainty about what token could come next
#
# CamemBERT (ModernCamemBERT variant) is a French BERT-style model trained
# on French web text. Mode = "mlm" uses both left and right context (masked).

cache_al_mlm <- "out/demo_surprisal_al_mlm.Rds"

if (file.exists(cache_al_mlm)) {
  message("Loading cached ALECTOR MLM surprisal scores")
  dt_alector_mlm <- readRDS(cache_al_mlm)
} else {
  message("Computing MLM surprisal (may take several minutes)...")
  dt_alector_mlm <- llm_surprisal_entropy(
    dt_alector,
    model_name = "almanach/moderncamembert-base",
    mode       = "mlm",
    batch_size = 8  # reduce to 1 if you get memory errors
  )
  saveRDS(dt_alector_mlm, cache_al_mlm)
  message("Saved to ", cache_al_mlm)
}


# 4) Aggregate to document level ----
#
# We average surprisal and entropy over all tokens in each document.
# pair_id lets us link each source text to its corresponding target text
# for paired analyses.

df_docs <- dt_alector_mlm |>
  as_tibble() |>
  mutate(
    source  = if_else(str_detect(doc_id, "_source_"), "source", "target"),
    pair_id = as.integer(str_remove(doc_id, "alector_(source|target)_"))
  ) |>
  group_by(doc_id, source, pair_id) |>
  summarise(
    llm_surprisal = mean(llm_surprisal, na.rm = TRUE),
    llm_entropy   = mean(llm_entropy,   na.rm = TRUE),
    .groups = "drop"
  )

print(df_docs)


# 5) Faceted boxplots ----
#
# Source texts should have higher surprisal and entropy than target texts
# if simplification makes language more predictable.
# The plot_faceted_boxplot() utility handles paired labeling automatically.

source("R/fnt_utility.R", encoding = "UTF-8")

df_docs |>
  rename(Surprisal = llm_surprisal, Entropy = llm_entropy) |>
  plot_faceted_boxplot(
    source, c(Surprisal, Entropy),
    title = "ALECTOR: Original vs Simplified (CamemBERT MLM)",
    y_lab = "Mean per document",
    notch = TRUE
  )


# 6) Summary statistics table ----
#
# Paired Cohen's d uses within-pair differences (source − target) to
# remove between-topic variance — a more sensitive test when observations
# are paired by topic.
#
# Spearman r: how consistently one group outranks the other across pairs.
# Cohen's kappa: agreement between binary classifications (above/below median).

cohens_d_paired <- function(x, y) {
  d <- x - y
  return(mean(d) / sd(d))
}

cohens_kappa <- function(x, y) {
  # Median-split both vectors, compute agreement on the 2×2 table.
  med <- median(c(x, y))
  tab <- table(x > med, y > med)
  p_o <- sum(diag(tab)) / sum(tab)
  p_e <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  return((p_o - p_e) / (1 - p_e))
}

# Pivot to wide format so each row is one pair: columns source and target.
df_paired <- df_docs |>
  pivot_longer(
    cols = c(llm_surprisal, llm_entropy),
    names_to  = "metric",
    values_to = "value"
  ) |>
  mutate(metric = recode(metric, llm_surprisal = "Surprisal", llm_entropy = "Entropy")) |>
  pivot_wider(id_cols = c(pair_id, metric), names_from = source, values_from = value)

df_summary <- df_paired |>
  group_by(metric) |>
  summarise(
    mean_source = round(mean(source, na.rm = TRUE), 2),
    mean_target = round(mean(target, na.rm = TRUE), 2),
    cohens_d    = round(cohens_d_paired(source, target), 2),
    spearman_r  = round(cor(source, target, method = "spearman"), 2),
    kappa       = round(cohens_kappa(source, target), 2),
    p_value     = t.test(source, target, paired = TRUE)$p.value,
    n_pairs     = n(),
    .groups = "drop"
  ) |>
  mutate(p = if_else(p_value < 0.001, "< .001", sprintf("%.3f", p_value)))

message("\n=== ALECTOR: Source vs Target (paired, CamemBERT MLM) ===\n")
print(df_summary |> select(metric, mean_source, mean_target, cohens_d, spearman_r, kappa, p, n_pairs))
