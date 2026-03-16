# Demo: embedding coherence features (document-level comparison)
#
# Sentence embeddings capture topic; we derive 8 structural features
# from how sentences relate to each other within a document:
#   - thematic_dispersion:    mean cosine distance to document centroid
#   - centroid_distance_sd:   spread of sentence distances to centroid
#   - sequential_similarity:  mean cosine sim between consecutive sentences
#   - mean_semantic_gap:      mean cosine gap between consecutive sentences
#   - max_semantic_gap:       largest cosine gap between consecutive sentences
#   - topic_drift:            mean cosine distance between consecutive 3-sent blocks
#   - mean_novelty:           mean cosine distance to running centroid
#   - n_topics:               optimal sentence cluster count (silhouette, k=1..5)
#   - convexity:              conceptual convexity (Gärdenfors): midpoints near data = 1
#   - local_convexity:        same, but only on consecutive sentence pairs (local continuity)
#
# In this demo you will:
#   1) compute coherence features on the wiki/viki corpus;
#   2) compare wiki (complex) vs viki (simple) with effect sizes;
#   3) apply the same features to the Alector corpus (source vs simplified);
#   4) compare effect sizes across both corpora.
#
# Prerequisite:
#   - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).

# 0) Setup ----

library(tidyverse)
library(reticulate)
library(effsize)

py_require(c(
  "transformers>=4.41,<5",
  "torch",
  "tokenizers",
  "sentence-transformers",
  "numpy",
  "scipy"
))

source("R/fnt_utility.R", encoding = "UTF-8")
source("R/fnt_embeddings.R", encoding = "UTF-8")

REUSE_EMBEDDINGS <- TRUE
MODEL_NAME  <- "dangvantuan/french-document-embedding"
MODEL_MODE  <- "basic"
MODEL_LABEL <- "dangvantuan_french_document_embedding"

feat_cols <- c("emb_thematic_dispersion", "emb_centroid_distance_sd",
               "emb_sequential_similarity", "emb_mean_semantic_gap",
               "emb_max_semantic_gap", "emb_topic_drift",
               "emb_mean_novelty", "emb_n_topics",
               "emb_convexity", "emb_local_convexity")

# Helper: safe Cohen's d (returns NA for insufficient or zero-variance data).
safe_cohen_d <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  if (length(x1) < 2 || length(x2) < 2) return(NA_real_)
  if (sd(x1) == 0 && sd(x2) == 0) return(0)
  cohen.d(x1, x2)$estimate
}

# Compare two groups on all features, return tibble with Cohen's d + Wilcoxon p.
compare_groups <- function(df, grp_col, grp_a, grp_b, feat_cols,
                           corpus_label, paired = FALSE, pair_col = NULL) {
  map_dfr(feat_cols, function(f) {
    fname <- str_remove(f, "^emb_")

    if (paired && !is.null(pair_col)) {
      df_wide <- df %>%
        select(all_of(c(pair_col, grp_col, f))) %>%
        pivot_wider(names_from = all_of(grp_col), values_from = all_of(f)) %>%
        rename(a = all_of(grp_a), b = all_of(grp_b)) %>%
        filter(is.finite(a), is.finite(b))

      p_val <- if (nrow(df_wide) >= 2)
        wilcox.test(df_wide$a, df_wide$b, paired = TRUE)$p.value else NA_real_
      d_val <- if (nrow(df_wide) >= 2) {
        diffs <- df_wide$a - df_wide$b
        mean(diffs) / sd(diffs)
      } else NA_real_
    } else {
      x_a <- df %>% filter(.data[[grp_col]] == grp_a) %>% pull(all_of(f))
      x_b <- df %>% filter(.data[[grp_col]] == grp_b) %>% pull(all_of(f))
      x_a <- x_a[is.finite(x_a)]; x_b <- x_b[is.finite(x_b)]
      p_val <- if (length(x_a) >= 2 && length(x_b) >= 2)
        wilcox.test(x_a, x_b)$p.value else NA_real_
      d_val <- safe_cohen_d(x_a, x_b)
    }

    tibble(corpus = corpus_label, feature = fname, d = d_val, p = p_val)
  })
}


# 1) Load wiki/viki corpus and compute embeddings ----

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")
cat(sprintf("Wiki/viki corpus: %d documents, %d tokens\n",
            n_distinct(dt_parsed_corpus$doc_id), nrow(dt_parsed_corpus)))

cache_doc  <- sprintf("out/demo_emb_%s.Rds", MODEL_LABEL)
cache_sent <- sprintf("out/demo_emb_sent_%s.Rds", MODEL_LABEL)

if (REUSE_EMBEDDINGS && file.exists(cache_doc) && file.exists(cache_sent)) {
  cat("Loading cached wiki/viki embeddings\n")
  dt_doc  <- readRDS(cache_doc)
  dt_sent <- readRDS(cache_sent)
} else {
  cat("Computing wiki/viki embeddings (this may take a while)...\n")
  emb <- corpus_embeddings(dt_parsed_corpus,
                           model_name = MODEL_NAME,
                           mode = MODEL_MODE,
                           batch_size = 8)
  dt_doc  <- emb$dt_doc_embeddings
  dt_sent <- emb$dt_sent_embeddings
  saveRDS(dt_doc, cache_doc)
  saveRDS(dt_sent, cache_sent)
}


# 2) Compute coherence features for wiki/viki ----

coherence_wv <- embedding_coherence(dt_sent) %>%
  mutate(source = if_else(str_detect(doc_id, "^wiki_"), "wiki", "viki"))

cat(sprintf("Wiki/viki coherence: %d documents\n", nrow(coherence_wv)))


# 3) Effect sizes for wiki/viki ----

effects_wv <- compare_groups(coherence_wv, "source", "wiki", "viki",
                             feat_cols, "wiki/viki")


# 4) Download Alector corpus (Gala et al., 2020) ----
# 79 pairs of French texts for children (original + simplified).
# Genres: tales, novels, fables, documentaries.

alector_dir <- "out/alector_corpus"
dir.create(alector_dir, recursive = TRUE, showWarnings = FALSE)

alector_base <- "https://raw.githubusercontent.com/gloignon/alector_corpus/master"
n_alector <- 79

# Download any missing files
alector_files <- crossing(i = 0:(n_alector - 1), suffix = c("source", "target")) %>%
  mutate(fname = sprintf("%03d_%s.txt", i, suffix),
         dest  = file.path(alector_dir, fname),
         url   = sprintf("%s/corpus/%s", alector_base, fname))

alector_files %>%
  filter(!file.exists(dest)) %>%
  pwalk(function(url, dest, fname, ...) {
    tryCatch(download.file(url, dest, quiet = TRUE),
             error = function(e) warning(sprintf("Failed: %s", fname)))
  })


# 5) Read Alector texts ----

dt_alector <- alector_files %>%
  filter(file.exists(dest)) %>%
  mutate(doc_id = sprintf("alector_%s_%02d", suffix, i)) %>%
  pmap(function(dest, doc_id, ...) read_sentences(dest, doc_id)) %>%
  compact() %>%
  bind_rows()

cat(sprintf("\nAlector: %d documents, %d sentences\n",
            n_distinct(dt_alector$doc_id), nrow(dt_alector)))


# 6) Embed Alector and compute coherence ----

cache_alector_sent <- sprintf("out/demo_emb_alector_sent_%s.Rds", MODEL_LABEL)

if (REUSE_EMBEDDINGS && file.exists(cache_alector_sent)) {
  cat("Loading cached Alector sentence embeddings\n")
  dt_alector_sent <- readRDS(cache_alector_sent)
} else {
  cat("Computing Alector embeddings...\n")
  emb_alector <- corpus_embeddings(dt_alector,
                                   model_name = MODEL_NAME,
                                   mode = MODEL_MODE,
                                   batch_size = 8)
  dt_alector_sent <- emb_alector$dt_sent_embeddings
  saveRDS(emb_alector$dt_doc_embeddings,
          sprintf("out/demo_emb_alector_%s.Rds", MODEL_LABEL))
  saveRDS(dt_alector_sent, cache_alector_sent)
}

coherence_al <- embedding_coherence(dt_alector_sent) %>%
  mutate(source  = if_else(str_detect(doc_id, "_source_"), "source", "target"),
         pair_id = str_remove(doc_id, "alector_(source|target)_"))

cat(sprintf("Alector coherence: %d documents\n", nrow(coherence_al)))


# 7) Effect sizes for Alector (paired) ----

effects_al <- compare_groups(coherence_al, "source", "source", "target",
                             feat_cols, "alector",
                             paired = TRUE, pair_col = "pair_id")


# 8) Combined comparison table ----

effects_both <- bind_rows(effects_wv, effects_al) %>%
  mutate(d   = round(d, 3),
         p   = round(p, 4),
         sig = case_when(p < .001 ~ "***",
                         p < .01  ~ "**",
                         p < .05  ~ "*",
                         TRUE     ~ "")) %>%
  arrange(corpus, desc(abs(d)))

cat("\nCoherence feature comparison (Cohen's d + Wilcoxon p):\n\n")
print(effects_both, n = Inf)
