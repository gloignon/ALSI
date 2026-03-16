# Demo: dependency-tree features (document-level comparison)
#
# In this demo you will:
# 1) inspect dependency metrics on single sentences;
# 2) compute document-level metrics on the demo corpus;
# 3) compare Vikidia vs Wikipedia with effect sizes;
# 4) check correlation with sentence length;
# 5) visualize interesting features with boxplots.
#
# Prerequisite:
# - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).

# 0) Setup ----

library(data.table)
library(tidyverse)
library(udpipe)
library(effsize)

source("R/fnt_heights.R", encoding = "UTF-8")

udmodel_french <- udpipe_load_model(file = "models/french_gsd-remix_2.udpipe")

# Robust Pearson correlation:
# returns 0 when undefined (not enough values or zero variance).
safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L) return(0)
  x_ok <- x[ok]
  y_ok <- y[ok]
  if (sd(x_ok) == 0 || sd(y_ok) == 0) return(0)
  cor(x_ok, y_ok, method = "pearson")
}

# Helper: safe Cohen's d (returns 0 for zero-variance features).
safe_cohen_d <- function(x1, x2) {
  if (sd(x1) == 0 && sd(x2) == 0) return(0)
  cohen.d(x1, x2)$estimate
}


# 1) Single-sentence inspection ----

sentence_simple <- "Le chat mange la petite souris verte qui est entrée par le trou."
dt_sentence <- as.data.table(udpipe(x = sentence_simple, object = udmodel_french))

# Token-level parsed output
dt_sentence

# Sentence-level tree metrics (includes Gibson DLT incomplete dependency count)
sentence_graph_stats(dt_sentence, verbose = TRUE)

# Token-level head direction/distance
head_final_initial(dt_sentence)

# Same metrics in batch form (single sentence input)
batch_graph_stats(dt_sentence)

# Doc-level aggregation over one sentence (sanity check)
docwise_graph_stats(dt_sentence)

# Optional: a longer sentence to inspect parse output quickly
sentence_long <- "Durant la guerre froide, il désignait les nations qui n'appartenaient ni au bloc soviétique, ni au monde occidental."
dt_sentence_long <- as.data.table(udpipe(x = sentence_long, object = udmodel_french))
dt_sentence_long[, .(token, token_id, head_token_id, upos, dep_rel)]

# Incomplete dependency count (Gibson DLT) on a more complex sentence:
# At each word position (left to right), counts how many dependencies are
# still unresolved (dependent seen, head not yet reached). The max gives
# the peak "memory load" during incremental parsing.
sentence_complex <- "Le livre que l'auteur dont tout le monde parle a écrit est fascinant."
dt_complex <- as.data.table(udpipe(x = sentence_complex, object = udmodel_french))
sentence_graph_stats(dt_complex, verbose = TRUE)
batch_graph_stats(dt_complex)


# 2) Punctuation sensitivity (sentence level) ----
# batch_graph_stats now defaults to include_punct_in_metrics = TRUE (tree structure).
# Compare with and without to see the effect on a single sentence.

dt_sentence_punct <- dt_sentence_long
batch_graph_stats(dt_sentence_punct)                                    # default: PUNCT included
batch_graph_stats(dt_sentence_punct, include_punct_in_metrics = FALSE)  # without PUNCT

dt_sentence_alt <- as.data.table(
  udpipe("C'est un problème que personne ne peut résoudre facilement.", object = udmodel_french)
)
batch_graph_stats(dt_sentence_alt)


# 3) Corpus-level docwise features ----
# docwise_graph_stats() now uses the optimal punctuation setting per feature group:
#   - Tree height/depth metrics: PUNCT included (adds meaningful structure)
#   - Head direction/distance metrics: PUNCT excluded (reduces noise)

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")

# Label docs: 1 = Vikidia, 2 = Wikipedia
dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class = ifelse(grepl("viki", dt_parsed_corpus$doc_id), 1L, 2L)
))

dt_heights <- docwise_graph_stats(dt_parsed_corpus)
dt_heights <- merge(dt_heights, dt_doc_classes, by = "doc_id")


# 4) Effect sizes + sentence-length correlation ----

non_features <- c("doc_id", "n", "s", "total_paths", "class")
features <- setdiff(names(dt_heights), non_features)

# Cohen's d: how well each feature separates Vikidia from Wikipedia
df_effect_sizes <- data.frame(
  feature = features,
  cohen_d = sapply(features, function(f) {
    safe_cohen_d(dt_heights[[f]][dt_heights$class == 1],
                 dt_heights[[f]][dt_heights$class == 2])
  })
)

# Correlation with document-wise average sentence length: n / s
dt_heights <- dt_heights %>%
  mutate(doc_sentence_length = n / pmax(s, 1L))

df_effect_sizes$corr_sentence_length <- sapply(features, function(f) {
  safe_cor(dt_heights[[f]], dt_heights$doc_sentence_length)
})

df_effect_sizes %>%
  arrange(desc(abs(cohen_d))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# 5) Facetted boxplots (docwise) ----

df_heights_long <- dt_heights %>%
  select(doc_id, class, all_of(features)) %>%
  pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")

# Guardrail: ensure this plot input remains document-level.
stopifnot(n_distinct(df_heights_long$doc_id) == nrow(dt_heights))
stopifnot(nrow(df_heights_long) == nrow(dt_heights) * length(features))

# Plot only features with substantial separation (|d| > 0.4).
df_heights_long %>%
  filter(feature %in% df_effect_sizes$feature[abs(df_effect_sizes$cohen_d) > 0.4]) %>%
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Distribution of Dependency Features by Class",
    x = "Class (1 = Vikidia, 2 = Wikipedia)",
    y = "Feature value"
  ) +
  theme_minimal()
