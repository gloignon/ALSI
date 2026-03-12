# Calibrate sentence-level dependency features against sentence length using GAMs.
#
# Sentence length is confounded with tree height/depth features: longer sentences
# mechanically produce deeper trees. Simple normalization (/n, /log(n)) either
# over-corrects or kills discriminative signal.
#
# Instead, we fit a GAM: feature ~ s(sentence_length) on a reference corpus,
# then residualize (observed - predicted). The residual captures the part of the
# feature that is NOT explained by sentence length alone.
#
# The fitting uses 5-fold cross-validation so residuals on the reference corpus
# are out-of-fold (no data leakage). The final GAM is then re-fit on ALL data
# and saved to models/ for use on new corpora.
#
# Usage:
#   1. Run this script with a parsed corpus to fit and save the GAMs.
#   2. Use apply_calibration() from fnt_heights.R on new data.
#
# loignon.guillaume@uqam.ca

library(data.table)
library(mgcv)

source("R/fnt_heights.R", encoding = "UTF-8")


# 1) Load reference corpus ----

dt_parsed_corpus <- readRDS("out/demo_parsed_tagged.Rds")


# 2) Compute sentence-level features ----

dt_sent <- batch_graph_stats(dt_parsed_corpus, include_punct_in_metrics = TRUE)


# 3) Features to calibrate ----
# Only features with strong sentence-length correlation benefit from this.
# Head direction/distance features are handled separately (no PUNCT, weaker
# length dependence).

features_to_calibrate <- c(
  "max_path",
  "avg_dependency_depth",
  "avg_dependency_depth_adj",
  "sd_depth",
  "branching_factor"
)


# 4) 5-fold CV: out-of-fold residuals ----

set.seed(42)
n_folds <- 5L
dt_sent[, fold := sample(rep(1:n_folds, length.out = .N))]

# Store out-of-fold predictions for each feature
oof_pred <- matrix(NA_real_, nrow = nrow(dt_sent), ncol = length(features_to_calibrate))
colnames(oof_pred) <- features_to_calibrate

cv_results <- list()

for (feat in features_to_calibrate) {
  message("CV for: ", feat)

  fold_r2 <- numeric(n_folds)

  for (f in seq_len(n_folds)) {
    train_idx <- dt_sent$fold != f
    test_idx  <- dt_sent$fold == f

    df_train <- data.frame(
      y = dt_sent[[feat]][train_idx],
      sentence_length = dt_sent$sentence_length[train_idx]
    )
    df_train <- df_train[is.finite(df_train$y) & df_train$sentence_length > 0, ]

    n_unique_len <- length(unique(df_train$sentence_length))
    k_val <- min(10, n_unique_len - 1)

    if (k_val < 3) {
      warning("Fold ", f, ": too few unique sentence lengths for ", feat, "; skipping.")
      next
    }

    gam_fold <- gam(y ~ s(sentence_length, k = k_val, bs = "tp"), data = df_train)
    fold_r2[f] <- summary(gam_fold)$r.sq

    # Out-of-fold predictions
    df_test <- data.frame(sentence_length = dt_sent$sentence_length[test_idx])
    oof_pred[test_idx, feat] <- predict(gam_fold, newdata = df_test)
  }

  cv_results[[feat]] <- data.frame(
    feature = feat,
    mean_r2 = mean(fold_r2, na.rm = TRUE),
    sd_r2 = sd(fold_r2, na.rm = TRUE)
  )
  message("  CV R-squared: ", round(mean(fold_r2, na.rm = TRUE), 3),
          " (+/- ", round(sd(fold_r2, na.rm = TRUE), 3), ")")
}

df_cv_summary <- do.call(rbind, cv_results)
df_cv_summary


# 5) Out-of-fold residuals ----

dt_sent_oof <- copy(dt_sent)

for (feat in features_to_calibrate) {
  resid_col <- paste0(feat, "_resid")
  dt_sent_oof[[resid_col]] <- dt_sent_oof[[feat]] - oof_pred[, feat]
}

# Check: OOF residuals should have near-zero correlation with sentence length
resid_cols <- paste0(features_to_calibrate, "_resid")
message("\nOut-of-fold residual correlations with sentence length:")
for (rc in resid_cols) {
  r <- cor(dt_sent_oof[[rc]], dt_sent_oof$sentence_length,
           use = "complete.obs")
  message("  ", rc, ": r = ", round(r, 4))
}


# 6) OOF evaluation: do residuals still discriminate? ----
# Aggregate OOF residuals to document level, then compute Cohen's d
# comparing raw vs residualized features.

library(effsize)
library(tidyverse)

dt_doc_classes <- unique(data.table(
  doc_id = dt_sent_oof$doc_id,
  class = ifelse(grepl("viki", dt_sent_oof$doc_id), 1L, 2L)
))

# Doc-level means of raw and residualized features
raw_and_resid_cols <- c(features_to_calibrate, resid_cols)
dt_doc_oof <- dt_sent_oof[, lapply(.SD, mean, na.rm = TRUE),
                          by = doc_id, .SDcols = raw_and_resid_cols]
dt_doc_oof <- merge(dt_doc_oof, dt_doc_classes, by = "doc_id")

# Cohen's d + correlation with doc-level mean sentence length
dt_doc_oof[, doc_sent_len := dt_sent_oof[, mean(sentence_length), by = doc_id]$V1]

df_eval <- data.frame(
  feature = raw_and_resid_cols,
  cohen_d = sapply(raw_and_resid_cols, function(f) {
    x1 <- dt_doc_oof[[f]][dt_doc_oof$class == 1]
    x2 <- dt_doc_oof[[f]][dt_doc_oof$class == 2]
    if (sd(x1) == 0 && sd(x2) == 0) 0 else cohen.d(x1, x2)$estimate
  }),
  corr_sent_len = sapply(raw_and_resid_cols, function(f) {
    cor(dt_doc_oof[[f]], dt_doc_oof$doc_sent_len, use = "complete.obs")
  })
)

df_eval %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(feature)


# 7) Fit final GAMs on full data ----

gam_models <- list()

for (feat in features_to_calibrate) {
  message("Fitting final GAM for: ", feat)

  df_fit <- data.frame(
    y = dt_sent[[feat]],
    sentence_length = dt_sent$sentence_length
  )
  df_fit <- df_fit[is.finite(df_fit$y) & df_fit$sentence_length > 0, ]

  n_unique_len <- length(unique(df_fit$sentence_length))
  k_val <- min(10, n_unique_len - 1)

  if (k_val < 3) {
    warning("Too few unique sentence lengths to fit GAM for ", feat, "; skipping.")
    next
  }

  gam_fit <- gam(y ~ s(sentence_length, k = k_val, bs = "tp"), data = df_fit)
  gam_models[[feat]] <- gam_fit

  message("  Full-data R-squared: ", round(summary(gam_fit)$r.sq, 3))
}


# 7) Inspect fits ----

par(mfrow = c(2, 3))
for (feat in names(gam_models)) {
  plot(gam_models[[feat]], main = feat, shade = TRUE, residuals = TRUE, pch = 1, cex = 0.5)
}
par(mfrow = c(1, 1))


# 8) Save calibration models ----

saveRDS(gam_models, "models/gam_sent_length_calibration.Rds")
message("Saved GAM calibration models to models/gam_sent_length_calibration.Rds")
