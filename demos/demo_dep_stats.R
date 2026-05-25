# ALSI Demo: Dependency Tree Features (Document-Level Comparison)
#
# Dependency parsing assigns each word a syntactic "head" — the word it
# grammatically depends on. From these head-dependent links you can build a
# tree and compute structural features that reflect syntactic complexity:
#
#   max_path / avg_sent_height  — how deep the tree grows (tall = complex)
#   avg_dependency_depth        — mean distance of each word from the root
#   avg_head_distance           — mean distance (in words) between a word and its head
#   branching_factor            — average number of dependents per non-leaf node
#   avg_incomplete_deps         — Gibson DLT: average number of open dependencies
#                                 at each position (working-memory load)
#
# Simpler texts (Vikidia) tend to have shallower trees and shorter head
# distances than more complex texts (Wikipedia).
#
# In this demo you will:
#   1) inspect dependency metrics for individual sentences;
#   2) compute document-level metrics on the full demo corpus;
#   3) compare Vikidia vs Wikipedia with Cohen's d effect sizes;
#   4) check how each feature correlates with sentence length (a potential confounder);
#   5) visualise features with boxplots.
#
# Prerequisites:
#   - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds)
#   - models/french_gsd-remix_2.udpipe


# 0) Setup ----

library(data.table)
library(tidyverse)
library(udpipe)
library(effsize)  # for cohen.d()

source("R/fnt_heights.R", encoding = "UTF-8")
source("R/fnt_utility.R", encoding = "UTF-8")

udmodel_french <- udpipe_load_model(file = "models/french_gsd-remix_2.udpipe")


# 1) Single-sentence inspection ----
#
# We start with one sentence to make the output readable.
# sentence_graph_stats() prints a summary of all tree metrics (verbose = TRUE).
# head_final_initial() shows, for each word, whether its head is to the
# left (head-initial) or right (head-final), and how far away it is.

sentence_simple <- "Le chat mange la petite souris verte qui est entrée par le trou."
dt_sentence <- as.data.table(udpipe(x = sentence_simple, object = udmodel_french))

# Raw UDPipe output: one row per token
print(dt_sentence[, .(token, token_id, head_token_id, upos, dep_rel)])

# Tree statistics (depth, branching, Gibson DLT)
sentence_graph_stats(dt_sentence, verbose = TRUE)

# Token-level head direction and distance
print(head_final_initial(dt_sentence))

# batch_graph_stats() is the corpus-scale version of sentence_graph_stats().
# It returns a data.table with one row per sentence.
print(batch_graph_stats(dt_sentence))

# docwise_graph_stats() aggregates over all sentences in a document.
# On a single sentence this just verifies the pipeline is working end-to-end.
print(docwise_graph_stats(dt_sentence))

# A longer, more complex sentence with a relative clause — useful to see
# how the metrics scale with syntactic embedding depth.
sentence_long <- "Durant la guerre froide, il désignait les nations qui n'appartenaient ni au bloc soviétique, ni au monde occidental."
dt_sentence_long <- as.data.table(udpipe(x = sentence_long, object = udmodel_french))
print(dt_sentence_long[, .(token, token_id, head_token_id, upos, dep_rel)])

# A doubly-embedded relative clause: high Gibson DLT score expected at
# "parle" because both relative clauses are open at that point.
sentence_complex <- "Le livre que l'auteur dont tout le monde parle a écrit est fascinant."
dt_complex <- as.data.table(udpipe(x = sentence_complex, object = udmodel_french))
sentence_graph_stats(dt_complex, verbose = TRUE)
print(batch_graph_stats(dt_complex))


# 2) Punctuation sensitivity ----
#
# batch_graph_stats() includes punctuation tokens by default because they
# are part of the tree structure in UDPipe's GSD model.
# Passing include_punct_in_metrics = FALSE removes them before computing
# depth and distance — useful if you want a pure lexical complexity measure.
# The difference is usually small but worth being aware of.

print(batch_graph_stats(dt_sentence_long))                                    # PUNCT included (default)
print(batch_graph_stats(dt_sentence_long, include_punct_in_metrics = FALSE))  # without PUNCT


# 3) Corpus-level docwise features ----
#
# docwise_graph_stats() processes an entire parsed corpus and returns one
# row per document with all tree metrics averaged over sentences.
# It uses the optimal punctuation setting for each feature group internally:
#   - tree height/depth: PUNCT included (structure)
#   - head direction/distance: PUNCT excluded (less noise)

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")
message("Loaded corpus: ", uniqueN(dt_parsed_corpus$doc_id), " documents")

# Label each document: Vikidia = 1, Wikipedia = 2
# (doc_id starts with "viki_" for Vikidia documents)
dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class  = ifelse(grepl("^viki_", dt_parsed_corpus$doc_id), 1L, 2L)
))

# This call may take a minute on a large corpus.
dt_heights <- docwise_graph_stats(dt_parsed_corpus)
dt_heights  <- merge(dt_heights, dt_doc_classes, by = "doc_id")


# 4) Effect sizes + sentence-length correlation ----
#
# Cohen's d measures how well each feature separates the two groups.
# d = 0.2 is small, d = 0.5 medium, d = 0.8 large.
# A negative d here means the feature is lower for Vikidia (class 1) than
# for Wikipedia (class 2) — i.e., Vikidia is simpler on that metric.
#
# We also compute the Pearson correlation with average sentence length,
# because many syntactic complexity features are confounded by sentence
# length. A high correlation means the feature mostly tracks length,
# not true structural complexity.

non_features <- c("doc_id", "n", "s", "total_paths", "class")
features <- setdiff(names(dt_heights), non_features)

df_effect_sizes <- data.frame(
  feature = features,
  cohen_d = purrr::map_dbl(features, function(f) {
    cohen.d(dt_heights[[f]][dt_heights$class == 1],
            dt_heights[[f]][dt_heights$class == 2])$estimate
  })
)

# Add average sentence length (total tokens / number of sentences) as a
# potential confounder to check correlation against.
dt_heights <- dt_heights |>
  mutate(doc_sentence_length = n / pmax(s, 1L))

df_effect_sizes$corr_sentence_length <- purrr::map_dbl(features, function(f) {
  cor(dt_heights[[f]], dt_heights$doc_sentence_length, use = "complete.obs")
})

# Print sorted by absolute effect size — largest effects at the top.
print(
  df_effect_sizes |>
    arrange(desc(abs(cohen_d))) |>
    mutate(across(where(is.numeric), ~round(.x, 2)))
)


# 5) Faceted boxplots ----
#
# Focus on features with at least a medium effect size (|d| > 0.4) to keep
# the plot readable. Each panel shows one feature; x-axis is class label.

df_heights_long <- dt_heights |>
  select(doc_id, class, all_of(features)) |>
  pivot_longer(cols = all_of(features), names_to = "feature", values_to = "value")

df_heights_long |>
  filter(feature %in% df_effect_sizes$feature[abs(df_effect_sizes$cohen_d) > 0.4]) |>
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  facet_wrap(~feature, scales = "free_y") +
  labs(
    title = "Distribution of Dependency Features by Class",
    x     = "Class (1 = Vikidia, 2 = Wikipedia)",
    y     = "Feature value"
  ) +
  theme_minimal()
