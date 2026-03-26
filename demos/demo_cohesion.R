# Demo: lexical cohesion features
#
# In this demo you will:
# 1) inspect token-level cohesion on a small hand-built corpus;
# 2) compute document-level cohesion on the parsed demo corpus;
# 3) compare Vikidia vs Wikipedia with effect sizes.
#
# Prerequisite:
# - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).

# 0) Setup ----

library(data.table)
library(tidyverse)
library(effsize)

source("R/fnt_cohesion.R", encoding = "UTF-8")


# 1) Token-level inspection on a toy corpus ----

# Two controled documents to demo how each feature works.
dt_toy <- data.table(
  doc_id       = c(rep("doc1", 9), rep("doc2", 6)),
  paragraph_id = rep(1L, 15),
  sentence_id  = c(rep(1L, 3), rep(2L, 3), rep(3L, 3),
                   rep(1L, 3), rep(2L, 3)),
  token_id     = c("1","2","3","1","2","3","1","2","3",
                   "1","2","3","1","2","3"),
  head_token_id = c("2","0","2","2","0","2","2","0","2",
                    "2","0","2","2","0","2"),
  token = c("le","chat","mange",         # doc1 s1
            "le","chat","dort",           # doc1 s2: "chat" repeats
            "un","chien","mange",         # doc1 s3: "mange" repeats from s1
            "la","fille","parle",         # doc2 s1
            "le","garcon","court"),       # doc2 s2: nothing shared
  lemma = c("le","chat","manger",
            "le","chat","dormir",
            "un","chien","manger",
            "le","fille","parler",
            "le","garcon","courir"),
  upos  = c("DET","NOUN","VERB",
            "DET","NOUN","VERB",
            "DET","NOUN","VERB",
            "DET","NOUN","VERB",
            "DET","NOUN","VERB"),
  dep_rel = c("det","root","obj",
              "det","root","obj",
              "det","root","obj",
              "det","root","obj",
              "det","root","obj"),
  compte = rep(TRUE, 15)
)

# Document-level recurrence: how often does each token appear elsewhere in the doc?
dt_doc_match <- compute_document_match(copy(dt_toy))
dt_doc_match[, .(doc_id, sentence_id, token, normalized_match)]

# Previous-sentence overlap: does this token appear in the previous sentence?
dt_prev <- compute_previous_sentence_match(copy(dt_toy), value_col = "token", n_prev = 1)
dt_prev[, .(doc_id, sentence_id, token, token_prev1_prop)]

# Adjacent-sentence cosine similarity
compute_sentence_similarity(copy(dt_toy), method = "cosine")

# Full pipeline → one row per document
simple_lexical_cohesion(copy(dt_toy), n_sent_context = c(1))


# 2) Corpus-level features ----

# This parsed corpus you can generate by running demos/demo_parse_tag.R 
dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")

dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class = ifelse(grepl("viki", dt_parsed_corpus$doc_id), 1L, 2L)
))

dt_cohesion <- simple_lexical_cohesion(dt_parsed_corpus, n_sent_context = c(1, 5))
dt_cohesion <- merge(dt_cohesion, dt_doc_classes, by = "doc_id")

dt_cohesion


# 3) Helper: effect sizes ----

cohesion_effect_sizes <- function(dt_cohesion, dt_corpus, label) {
  non_features <- c("doc_id", "class", "word_count")
  features <- setdiff(names(dt_cohesion), non_features)

  dt_word_counts <- dt_corpus[compte == TRUE, .(word_count = .N), by = doc_id]
  dt_cohesion <- merge(dt_cohesion, dt_word_counts, by = "doc_id")

  df_effect_sizes <- data.frame(
    feature = features,
    r = sapply(features, function(f) {
      cor(dt_cohesion[[f]], dt_cohesion$class, use = "complete.obs")
    }),
    cohen_d = sapply(features, function(f) {
      cohen.d(dt_cohesion[[f]][dt_cohesion$class == 1],
              dt_cohesion[[f]][dt_cohesion$class == 2])$estimate
    }),
    corr_length = sapply(features, function(f) {
      cor(dt_cohesion[[f]], dt_cohesion$word_count, use = "complete.obs")
    })
  )

  cat("\n===", label, "===\n\n")
  print(df_effect_sizes %>%
    arrange(desc(abs(cohen_d))) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))))

  invisible(list(dt_cohesion = dt_cohesion, features = features))
}


# 4) Effect sizes: Vikidia vs Wikipedia ----

res_vw <- cohesion_effect_sizes(dt_cohesion, dt_parsed_corpus,
                                "Vikidia (1) vs Wikipedia (2)")


# 5) ALECTOR: source vs target ----

dt_alector <- readRDS("out/alector_parsed.Rds")
cat("\nALECTOR corpus:", nrow(dt_alector), "tokens,",
    uniqueN(dt_alector$doc_id), "documents\n")

dt_classes_al <- unique(data.table(
  doc_id = dt_alector$doc_id,
  class = ifelse(grepl("_target_", dt_alector$doc_id), 1L, 2L)
))

dt_cohesion_al <- simple_lexical_cohesion(dt_alector, n_sent_context = c(1, 5))
dt_cohesion_al <- merge(dt_cohesion_al, dt_classes_al, by = "doc_id")

res_al <- cohesion_effect_sizes(dt_cohesion_al, dt_alector,
                                "ALECTOR: target/simplified (1) vs source/original (2)")


# 6) Boxplots (Vikidia vs Wikipedia) ----

df_long <- res_vw$dt_cohesion %>%
  select(doc_id, class, all_of(res_vw$features)) %>%
  pivot_longer(cols = all_of(res_vw$features), names_to = "feature", values_to = "value")

df_long %>%
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Lexical Cohesion: Vikidia (1) vs Wikipedia (2)",
    x = "Class", y = "Feature value"
  ) +
  theme_minimal()

# Boxplots (ALECTOR)
df_long_al <- res_al$dt_cohesion %>%
  select(doc_id, class, all_of(res_al$features)) %>%
  pivot_longer(cols = all_of(res_al$features), names_to = "feature", values_to = "value")

df_long_al %>%
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Lexical Cohesion: ALECTOR target (1) vs source (2)",
    x = "Class", y = "Feature value"
  ) +
  theme_minimal()
