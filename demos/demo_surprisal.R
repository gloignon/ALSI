# Demo: LLM surprisal and entropy (masked language model).
#
# Alector corpus: 79 pairs of French texts for children
# (original "source" + simplified "target").
# Genres: tales, novels, fables, documentaries.
#
library(tidyverse)
library(reticulate)
library(udpipe)

py_require(c(
  "transformers>=4.41,<5",
  "torch",
  "tokenizers",
  "numpy"
))

source('R/fnt_surprisal.R', encoding = 'UTF-8')
source('R/fnt_corpus.R', encoding = 'UTF-8')


# 1) Download Alector (if not already present) ----

alector_dir <- "out/alector_corpus"
dir.create(alector_dir, recursive = TRUE, showWarnings = FALSE)

alector_base <- "https://raw.githubusercontent.com/gloignon/alector_corpus/master"
n_alector <- 79

alector_files <- tibble(
  i      = rep(0:(n_alector - 1), each = 2),
  suffix = rep(c("source", "target"), times = n_alector)
) %>%
  mutate(
    fname  = sprintf("%03d_%s.txt", i, suffix),
    dest   = file.path(alector_dir, fname),
    url    = sprintf("%s/corpus/%s", alector_base, fname),
    doc_id = sprintf("alector_%s_%02d", suffix, i)
  )

# Download missing files
alector_files %>%
  filter(!file.exists(dest)) %>%
  pwalk(function(url, dest, fname, ...) {
    tryCatch(download.file(url, dest, quiet = TRUE),
             error = function(e) warning(sprintf("Failed: %s", fname)))
  })

# 2) Read and parse Alector with UDPipe ----

cache_alector_parsed <- "out/alector_parsed.Rds"

if (file.exists(cache_alector_parsed)) {
  cat("Loading cached Alector parsed corpus\n")
  dt_alector <- readRDS(cache_alector_parsed)
} else {
  cat("Parsing Alector corpus with UDPipe...\n")

  dt_alector_txt <- alector_files %>%
    filter(file.exists(dest)) %>%
    rowwise() %>%
    mutate(text = paste(readLines(dest, encoding = "UTF-8", warn = FALSE), collapse = " ")) %>%
    ungroup() %>%
    select(doc_id, text) %>%
    as.data.table()

  cat(sprintf("Alector: %d documents loaded\n", nrow(dt_alector_txt)))

  n_cores <- max(1, parallel::detectCores() - 1)
  dt_alector_raw <- parse_text(dt_alector_txt, n_cores = n_cores)
  dt_alector <- post_process_lexicon(dt_alector_raw)
  saveRDS(dt_alector, cache_alector_parsed)
}

cat(sprintf("Alector parsed: %d tokens, %d documents\n",
            nrow(dt_alector), n_distinct(dt_alector$doc_id)))

# 3) Compute surprisal on Alector ----

cache_al_mlm <- "out/demo_surprisal_al_mlm.Rds"
if (file.exists(cache_al_mlm)) {
  cat("Loading cached Alector MLM scores\n")
  dt_alector_mlm <- readRDS(cache_al_mlm)
} else {
  dt_alector_mlm <- llm_surprisal_entropy(
    dt_alector,
    model_name = "almanach/moderncamembert-base",
    mode = "mlm",
    batch_size = 8  # auto-reduced to 1 on MPS (Apple Silicon) due to PyTorch bug
  )
  saveRDS(dt_alector_mlm, cache_al_mlm)
}

# 4) Aggregate and compare source vs target ----

df_docs <- dt_alector_mlm %>%
  as_tibble() %>%
  mutate(
    source  = if_else(str_detect(doc_id, "_source_"), "source", "target"),
    pair_id = as.integer(str_remove(doc_id, "alector_(source|target)_"))
  ) %>%
  group_by(doc_id, source, pair_id) %>%
  summarise(
    llm_surprisal = mean(llm_surprisal, na.rm = TRUE),
    llm_entropy   = mean(llm_entropy,   na.rm = TRUE),
    .groups = "drop"
  )

# 5) Faceted boxplots ----

df_docs %>%
  pivot_longer(cols = c(llm_surprisal, llm_entropy),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         llm_surprisal = "Surprisal",
                         llm_entropy   = "Entropy")) %>%
  ggplot(aes(x = source, y = value, fill = source)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Alector: Original vs Simplified (CamemBERT MLM)",
       x = NULL, y = "Mean per document") +
  theme_minimal() +
  theme(legend.position = "none")

# 6) Summary table ----

cohens_d_paired <- function(x, y) {
  d <- x - y
  mean(d) / sd(d)
}

cohens_kappa <- function(x, y) {
  # Median-split both vectors, compute Cohen's kappa on the 2x2 agreement
  med <- median(c(x, y))
  tab <- table(x > med, y > med)
  p_o <- sum(diag(tab)) / sum(tab)
  p_e <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  (p_o - p_e) / (1 - p_e)
}

df_paired <- df_docs %>%
  pivot_longer(cols = c(llm_surprisal, llm_entropy),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, llm_surprisal = "Surprisal", llm_entropy = "Entropy")) %>%
  pivot_wider(id_cols = c(pair_id, metric), names_from = source, values_from = value)

df_summary <- df_paired %>%
  group_by(metric) %>%
  summarise(
    mean_source = round(mean(source), 2),
    mean_target = round(mean(target), 2),
    cohens_d    = round(cohens_d_paired(source, target), 2),
    spearman_r  = round(cor(source, target, method = "spearman"), 2),
    kappa       = round(cohens_kappa(source, target), 2),
    p_value     = t.test(source, target, paired = TRUE)$p.value,
    n_pairs     = n(),
    .groups = "drop"
  ) %>%
  mutate(p = if_else(p_value < 0.001, "< .001", sprintf("%.3f", p_value)))

cat("\n=== Alector: Source vs Target (paired, CamemBERT MLM) ===\n\n")
print(df_summary %>% select(metric, mean_source, mean_target, cohens_d, spearman_r, kappa, p, n_pairs))
