# Demo: multi-word expression (MWE) matching on parsed data
#
# In this demo you will:
# 1) match simple hand-built expressions against a toy corpus;
# 2) match the LEXCONN connective database against the demo corpus;
# 3) compute connective density features per document;
# 4) compare Vikidia vs Wikipedia with effect sizes.
#
# Prerequisite:
# - Run demos/demo_parse_tag.R first (creates out/demo_parsed_tagged.Rds).
# - Run scrapers/fetch_lexconn.R first (creates lexical_dbs/dt_lexconn.Rds).

# 0) Setup ----

library(data.table)
library(tidyverse)
library(effsize)

source("R/fnt_utility.R", encoding = "UTF-8")


# 1) Toy example: hand-built lexicon ----

# A small sentence with known multi-word expressions
dt_toy <- data.table(
  doc_id      = "d1",
  sentence_id = 1L,
  token_id    = as.character(1:12),
  token       = c("Il", "a", "réussi", "afin", "de", "partir",
                   ",", "mais", "au", "lieu", "de", "dormir")
)

# A tiny lexicon with 1-, 2-, and 3-word entries
dt_lex_toy <- data.table(
  forme_lower = c("mais", "afin de", "au lieu de", "cependant"),
  n_tokens    = c(1L, 2L, 3L, 1L),
  relation    = c("contrast", "goal", "contrast", "concession"),
  cat         = c("cco", "prep", "prep", "adv")
)

matches_toy <- match_multiword_sequences(dt_toy, dt_lex_toy)
print(matches_toy)


# 2) Multi-document toy example ----

# Two docs to verify per-document isolation
dt_two_docs <- rbind(
  data.table(doc_id = "d1", sentence_id = 1L,
             token_id = as.character(1:5),
             token = c("donc", "il", "est", "parti", ".")),
  data.table(doc_id = "d2", sentence_id = 1L,
             token_id = as.character(1:6),
             token = c("bien", "que", "ce", "soit", "tard", "."))
)

dt_lex_two <- data.table(
  forme_lower = c("donc", "bien que"),
  n_tokens    = c(1L, 2L),
  relation    = c("result", "concession")
)

matches_two <- match_multiword_sequences(dt_two_docs, dt_lex_two)
print(matches_two)


# 3) LEXCONN on the parsed corpus ----

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")
dt_lexconn <- readRDS("lexical_dbs/dt_lexconn.Rds")

cat("Corpus:", nrow(dt_parsed_corpus), "tokens,",
    uniqueN(dt_parsed_corpus$doc_id), "documents\n")
cat("LEXCONN:", uniqueN(dt_lexconn$forme_lower), "connective forms\n\n")

matches_raw <- match_multiword_sequences(dt_parsed_corpus, dt_lexconn)
cat("Total matches (no POS filter):", nrow(matches_raw), "\n")

# POS-filtered: only keep matches where first token's UPOS matches LEXCONN category
matches <- match_multiword_sequences(dt_parsed_corpus, dt_lexconn, pos_filter = TRUE)
cat("Total matches (POS-filtered):", nrow(matches), "\n")
cat("Removed:", nrow(matches_raw) - nrow(matches), "spurious matches\n\n")

# Most frequent connectives (POS-filtered)
print(matches[, .N, by = matched_forme][order(-N)][1:20])

# Matches by relation group
print(matches[, .N, by = relation_group][order(-N)])


# 4) Helper: connective density features + effect sizes ----

connective_density_effects <- function(dt_corpus, matches, dt_doc_classes, label) {
  dt_word_counts <- dt_corpus[compte == TRUE, .(word_count = .N), by = doc_id]

  dt_conn_total <- matches[, .(n_connectives = uniqueN(paste(sentence_id, token_id_start))),
                           by = doc_id]
  dt_conn_group <- dcast(
    matches[, .(n = uniqueN(paste(sentence_id, token_id_start))),
            by = .(doc_id, relation_group)],
    doc_id ~ relation_group, value.var = "n", fill = 0L
  )
  dt_conn_cat <- dcast(
    matches[, .(n = uniqueN(paste(sentence_id, token_id_start))),
            by = .(doc_id, cat)],
    doc_id ~ cat, value.var = "n", fill = 0L
  )
  setnames(dt_conn_cat, setdiff(names(dt_conn_cat), "doc_id"),
           paste0("cat_", setdiff(names(dt_conn_cat), "doc_id")))

  dt_features <- merge(dt_word_counts, dt_conn_total, by = "doc_id", all.x = TRUE)
  dt_features <- merge(dt_features, dt_conn_group, by = "doc_id", all.x = TRUE)
  dt_features <- merge(dt_features, dt_conn_cat, by = "doc_id", all.x = TRUE)
  dt_features <- merge(dt_features, dt_doc_classes, by = "doc_id")

  conn_cols <- setdiff(names(dt_features), c("doc_id", "word_count", "class"))
  for (col in conn_cols) set(dt_features, which(is.na(dt_features[[col]])), col, 0L)

  density_cols <- paste0(conn_cols, "_per100w")
  dt_features[, (density_cols) := lapply(.SD, function(x) x / word_count * 100),
              .SDcols = conn_cols]

  df_effect_sizes <- data.frame(
    feature = density_cols,
    r = sapply(density_cols, function(f) {
      cor(dt_features[[f]], dt_features$class, use = "complete.obs")
    }),
    cohen_d = sapply(density_cols, function(f) {
      cohen.d(dt_features[[f]][dt_features$class == 1],
              dt_features[[f]][dt_features$class == 2])$estimate
    }),
    corr_length = sapply(density_cols, function(f) {
      cor(dt_features[[f]], dt_features$word_count, use = "complete.obs")
    })
  )

  cat("\n===", label, "===\n\n")
  print(df_effect_sizes %>%
    arrange(desc(abs(cohen_d))) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))))

  invisible(list(dt_features = dt_features, density_cols = density_cols))
}


# 5) Vikidia vs Wikipedia ----

dt_classes_vw <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class = ifelse(grepl("viki", dt_parsed_corpus$doc_id), 1L, 2L)
))

res_vw <- connective_density_effects(dt_parsed_corpus, matches, dt_classes_vw,
                                     "Vikidia (1) vs Wikipedia (2)")


# 6) ALECTOR: source vs target ----

dt_alector <- readRDS("out/alector_parsed.Rds")
cat("\nALECTOR corpus:", nrow(dt_alector), "tokens,",
    uniqueN(dt_alector$doc_id), "documents\n")

matches_alector <- match_multiword_sequences(dt_alector, dt_lexconn, pos_filter = TRUE)
cat("POS-filtered matches:", nrow(matches_alector), "\n")

dt_classes_al <- unique(data.table(
  doc_id = dt_alector$doc_id,
  class = ifelse(grepl("_target_", dt_alector$doc_id), 1L, 2L)
))

res_al <- connective_density_effects(dt_alector, matches_alector, dt_classes_al,
                                     "ALECTOR: target/simplified (1) vs source/original (2)")


# 7) Boxplots (Vikidia vs Wikipedia) ----

df_long <- res_vw$dt_features %>%
  select(doc_id, class, all_of(res_vw$density_cols)) %>%
  pivot_longer(cols = all_of(res_vw$density_cols), names_to = "feature", values_to = "value")

df_long %>%
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Connective Density: Vikidia (1) vs Wikipedia (2)",
    x = "Class", y = "Per 100 words"
  ) +
  theme_minimal()

# Boxplots (ALECTOR)
df_long_al <- res_al$dt_features %>%
  select(doc_id, class, all_of(res_al$density_cols)) %>%
  pivot_longer(cols = all_of(res_al$density_cols), names_to = "feature", values_to = "value")

df_long_al %>%
  ggplot(aes(x = factor(class), y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    title = "Connective Density: ALECTOR target (1) vs source (2)",
    x = "Class", y = "Per 100 words"
  ) +
  theme_minimal()
