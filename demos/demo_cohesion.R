# ALSI Demo: Lexical Cohesion Features
#
# "Cohesion" refers to the way sentences in a text are connected through
# shared vocabulary and meaning. A text where the same words recur across
# sentences (e.g. "chat" in sentence 1, "chat" in sentence 2) is highly
# cohesive. Simple texts (Vikidia) tend to use repetition as a cohesion
# strategy, while complex texts (Wikipedia) use more varied vocabulary.
#
# ALSI computes three complementary cohesion metrics:
#
#   normalized_match     — how often each token also appears elsewhere in
#                          the same document (document-level recurrence)
#   token_prev1_prop     — proportion of tokens in this sentence that also
#                          appeared in the immediately preceding sentence
#                          (local, adjacent-sentence overlap)
#   cosine_sent          — cosine similarity between consecutive sentences
#                          based on token overlap (similarity ∈ [0, 1])
#
# In this demo you will:
#   1) inspect cohesion metrics on a small hand-built corpus;
#   2) compute document-level cohesion on the full demo corpus;
#   3) compare Vikidia vs Wikipedia and ALECTOR with Cohen's d effect sizes;
#   4) visualise with faceted boxplots.
#
# Prerequisites:
#   - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds)
#   - out/alector_parsed.Rds (optional — ALECTOR section can be skipped)


# 0) Setup ----

library(data.table)
library(tidyverse)
library(effsize)  # for cohen.d()

source("R/fnt_cohesion.R", encoding = "UTF-8")
source("R/fnt_utility.R",  encoding = "UTF-8")


# 1) Single-sentence inspection on a toy corpus ----
#
# We build a tiny corpus by hand so you can see exactly what each function
# computes and verify the output makes intuitive sense.
#
# doc1 has three sentences:
#   s1: "le chat mange"
#   s2: "le chat dort"   — "chat" repeats from s1 → high local overlap
#   s3: "un chien mange" — "mange" repeats from s1, but "chien" is new
# doc2 has two sentences with no vocabulary shared between them.

dt_toy <- data.table(
  doc_id       = c(rep("doc1", 9), rep("doc2", 6)),
  paragraph_id = rep(1L, 15),
  sentence_id  = c(rep(1L, 3), rep(2L, 3), rep(3L, 3),
                   rep(1L, 3), rep(2L, 3)),
  token_id     = c("1","2","3","1","2","3","1","2","3",
                   "1","2","3","1","2","3"),
  head_token_id = c("2","0","2","2","0","2","2","0","2",
                    "2","0","2","2","0","2"),
  token  = c("le","chat","mange",         # doc1 s1
             "le","chat","dort",           # doc1 s2: "chat" repeats
             "un","chien","mange",         # doc1 s3: "mange" repeats
             "la","fille","parle",         # doc2 s1
             "le","garcon","court"),       # doc2 s2: nothing shared
  lemma  = c("le","chat","manger",
             "le","chat","dormir",
             "un","chien","manger",
             "le","fille","parler",
             "le","garcon","courir"),
  upos   = c("DET","NOUN","VERB",
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

# Document-level recurrence: for each token, what fraction of sentences in
# the same document also contain this token?
# "chat" in doc1 appears in 2 out of 3 sentences → normalized_match ≈ 0.67
dt_doc_match <- compute_document_match(copy(dt_toy))
print(dt_doc_match[, .(doc_id, sentence_id, token, normalized_match)])

# Previous-sentence overlap: did this token appear in the immediately preceding
# sentence? token_prev1_prop is the proportion of tokens that did.
# doc1 s2 shares "le" and "chat" with s1 → prop = 2/3 ≈ 0.67
dt_prev <- compute_previous_sentence_match(copy(dt_toy), value_col = "token", n_prev = 1)
print(dt_prev[, .(doc_id, sentence_id, token, token_prev1_prop)])

# Cosine similarity between consecutive sentences based on shared token types.
# s1–s2 in doc1 share two types out of three → similarity > 0.
# s1–s2 in doc2 share nothing → similarity = 0.
print(compute_sentence_similarity(copy(dt_toy), method = "cosine"))

# Full pipeline: one row per document with all cohesion features averaged.
print(simple_lexical_cohesion(copy(dt_toy), n_sent_context = c(1)))


# 2) Corpus-level features ----
#
# simple_lexical_cohesion() processes the whole corpus and returns one row
# per document. n_sent_context = c(1, 5) computes both a narrow window
# (look back 1 sentence) and a broader window (look back 5 sentences).
# The broader window captures paragraph-scale coherence.

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")
message("Loaded corpus: ", uniqueN(dt_parsed_corpus$doc_id), " documents")

dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class  = ifelse(grepl("^viki_", dt_parsed_corpus$doc_id), 1L, 2L)
))

dt_cohesion <- simple_lexical_cohesion(dt_parsed_corpus, n_sent_context = c(1, 5))
dt_cohesion  <- merge(dt_cohesion, dt_doc_classes, by = "doc_id")

print(dt_cohesion)


# 3) Helper: compute and print effect sizes ----
#
# Cohen's d tells us how large the difference between two groups is.
# r is the point-biserial correlation between the feature and class label.
# corr_length is the Pearson correlation with document word count —
# a high correlation means the feature is partly a proxy for text length.

cohesion_effect_sizes <- function(dt_cohesion, dt_corpus, label) {
  non_features <- c("doc_id", "class", "word_count")
  features     <- setdiff(names(dt_cohesion), non_features)

  dt_word_counts <- dt_corpus[compte == TRUE, .(word_count = .N), by = doc_id]
  dt_cohesion    <- merge(dt_cohesion, dt_word_counts, by = "doc_id")

  df_effect_sizes <- data.frame(
    feature = features,
    r = purrr::map_dbl(features, function(f) {
      cor(dt_cohesion[[f]], dt_cohesion$class, use = "complete.obs")
    }),
    cohen_d = purrr::map_dbl(features, function(f) {
      cohen.d(dt_cohesion[[f]][dt_cohesion$class == 1],
              dt_cohesion[[f]][dt_cohesion$class == 2])$estimate
    }),
    corr_length = purrr::map_dbl(features, function(f) {
      cor(dt_cohesion[[f]], dt_cohesion$word_count, use = "complete.obs")
    })
  )

  message("\n=== ", label, " ===\n")
  print(
    df_effect_sizes |>
      arrange(desc(abs(cohen_d))) |>
      mutate(across(where(is.numeric), ~round(.x, 2)))
  )

  return(invisible(list(dt_cohesion = dt_cohesion, features = features)))
}


# 4) Effect sizes: Vikidia vs Wikipedia ----
#
# Expectation: Vikidia should show higher local overlap (simpler, more
# repetitive vocabulary) and possibly higher cosine similarity between
# adjacent sentences.

res_vw <- cohesion_effect_sizes(dt_cohesion, dt_parsed_corpus,
                                "Vikidia (1) vs Wikipedia (2)")


# 5) ALECTOR: source vs target ----
#
# ALECTOR pairs each original (source) text with a simplified (target) version.
# If simplification increases cohesion, target texts should have higher overlap.

dt_alector <- readRDS("out/alector_parsed.Rds")
message("ALECTOR corpus: ", uniqueN(dt_alector$doc_id), " documents, ",
        nrow(dt_alector), " tokens")

dt_classes_al <- unique(data.table(
  doc_id = dt_alector$doc_id,
  class  = ifelse(grepl("_target_", dt_alector$doc_id), 1L, 2L)
))

dt_cohesion_al <- simple_lexical_cohesion(dt_alector, n_sent_context = c(1, 5))
dt_cohesion_al  <- merge(dt_cohesion_al, dt_classes_al, by = "doc_id")

res_al <- cohesion_effect_sizes(dt_cohesion_al, dt_alector,
                                "ALECTOR: target/simplified (1) vs source/original (2)")


# 6) Boxplots ----

# Vikidia vs Wikipedia
plot_faceted_boxplot(
  res_vw$dt_cohesion, class, all_of(res_vw$features),
  title = "Lexical Cohesion: Vikidia (1) vs Wikipedia (2)",
  x_lab = "Class", y_lab = "Feature value"
)

# ALECTOR source vs target
plot_faceted_boxplot(
  res_al$dt_cohesion, class, all_of(res_al$features),
  title = "Lexical Cohesion: ALECTOR target (1) vs source (2)",
  x_lab = "Class", y_lab = "Feature value"
)
