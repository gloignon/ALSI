# Feature selection utilities
#
# Reusable functions for variable selection / feature ranking.
# Methods: MRMR (minimum redundancy, maximum relevance) and LASSO (L1-penalized GLM).
#
# Dependencies: data.table, mRMRe, glmnet

library(data.table)
library(mRMRe)
library(glmnet)


#' Run MRMR feature ranking
#'
#' Ranks features by mutual information with the target minus redundancy
#' with already-selected features (Ding & Peng, 2005).
#'
#' @param dt       data.table with feature columns and target column.
#' @param features Character vector of feature column names.
#' @param target   Name of the target column (numeric or integer).
#' @param top_n    Number of top features to return.
#' @returns data.table with columns: rank, feature, mrmr_score.
mrmr_ranking <- function(dt, features, target = "class", top_n = 5L) {
  dt_sel <- dt[, c(features, target), with = FALSE]
  dt_sel <- dt_sel[complete.cases(dt_sel)]
  dt_sel[, (target) := as.numeric(get(target))]

  mrmr_data <- mRMR.data(data = as.data.frame(dt_sel))
  mrmr_res <- mRMR.classic(data = mrmr_data,
                            target_indices = which(names(dt_sel) == target),
                            feature_count = min(top_n, length(features)))
  mrmr_idx <- solutions(mrmr_res)[[1]]
  mrmr_scores <- scores(mrmr_res)[[1]]

  data.table(
    rank = seq_along(mrmr_idx),
    feature = names(dt_sel)[mrmr_idx],
    mrmr_score = round(mrmr_scores, 4)
  )
}


#' Run LASSO feature selection
#'
#' Fits an L1-penalized logistic regression via cross-validation and returns
#' coefficients at lambda.1se (most regularized model within 1 SE of minimum).
#'
#' @param dt       data.table with feature columns and target column.
#' @param features Character vector of feature column names.
#' @param target   Name of the target column (binary: 0/1 or 1/2).
#' @param nfolds   Number of CV folds.
#' @param lambda   Which lambda to use: "lambda.1se" (default, sparser) or "lambda.min".
#' @returns data.table with columns: feature, lasso_coef, ordered by |coef|.
lasso_ranking <- function(dt, features, target = "class", nfolds = 10L,
                          lambda = "lambda.1se") {
  dt_sel <- dt[, c(features, target), with = FALSE]
  dt_sel <- dt_sel[complete.cases(dt_sel)]
  dt_sel[, (target) := as.numeric(get(target))]

  X <- as.matrix(dt_sel[, ..features])
  y <- dt_sel[[target]]

  cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = nfolds)
  coefs <- coef(cv_fit, s = lambda)[-1, ]  # drop intercept

  data.table(
    feature = names(coefs),
    lasso_coef = round(coefs, 4)
  )[order(-abs(lasso_coef))]
}


#' Combined MRMR + LASSO feature selection
#'
#' Runs both methods and merges results into a single table.
#'
#' @param dt       data.table with feature columns and target column.
#' @param features Character vector of feature column names.
#' @param target   Name of the target column.
#' @param top_n    Number of top MRMR features to return.
#' @param nfolds   Number of LASSO CV folds.
#' @returns data.table with columns: feature, rank, mrmr_score, lasso_coef.
run_feature_selection <- function(dt, features, target = "class", top_n = 5L,
                                  nfolds = 10L) {
  dt_mrmr <- mrmr_ranking(dt, features, target, top_n)
  dt_lasso <- lasso_ranking(dt, features, target, nfolds)
  merge(dt_mrmr, dt_lasso, by = "feature", all = TRUE)
}


#' MRMR incremental evaluation (elbow analysis)
#'
#' Runs MRMR to rank all features, then evaluates a logistic regression at each
#' subset size k = 1..p using the MRMR-ordered features. Reports AUC and accuracy
#' at each step so you can identify the optimal number of features (elbow point).
#'
#' @param dt       data.table with feature columns and target column.
#' @param features Character vector of feature column names.
#' @param target   Name of the target column (binary: 1/2 or 0/1).
#' @param nfolds   Number of CV folds for AUC estimation (leave-one-out if NULL).
#' @returns data.table with columns: k, feature_added, features_in_model, auc, accuracy.
mrmr_incremental <- function(dt, features, target = "class", nfolds = 5L) {
  # Full MRMR ranking over all features
  dt_mrmr <- mrmr_ranking(dt, features, target, top_n = length(features))
  ordered_features <- dt_mrmr$feature

  # Prepare data
  dt_sel <- dt[, c(features, target), with = FALSE]
  dt_sel <- dt_sel[complete.cases(dt_sel)]
  y <- as.integer(dt_sel[[target]])
  # Normalize to 0/1
  if (min(y) == 1L) y <- y - 1L

  # Compute AUC from predictions using the rank-based (Mann-Whitney) formula.
  auc_from_pred <- function(y, score) {
    n_pos <- sum(y == 1L)
    n_neg <- sum(y == 0L)
    if (n_pos == 0L || n_neg == 0L) return(NA_real_)
    r <- rank(score, ties.method = "average")
    (sum(r[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  }

  # Evaluate at each k
  results <- rbindlist(lapply(seq_along(ordered_features), function(k) {
    feat_k <- ordered_features[1:k]
    X <- as.matrix(dt_sel[, ..feat_k])

    # CV for honest AUC
    set.seed(42)
    folds <- sample(rep(seq_len(nfolds), length.out = nrow(X)))
    preds <- rep(NA_real_, nrow(X))

    for (fold in seq_len(nfolds)) {
      idx_test <- which(folds == fold)
      idx_train <- which(folds != fold)
      fit <- tryCatch(
        glm(y ~ ., data = data.frame(y = y[idx_train], X[idx_train, , drop = FALSE]),
            family = binomial()),
        warning = function(w) suppressWarnings(
          glm(y ~ ., data = data.frame(y = y[idx_train], X[idx_train, , drop = FALSE]),
              family = binomial())
        )
      )
      preds[idx_test] <- predict(fit, newdata = data.frame(X[idx_test, , drop = FALSE]),
                                  type = "response")
    }

    data.table(
      k = k,
      feature_added = ordered_features[k],
      mrmr_score = dt_mrmr$mrmr_score[k],
      auc_cv = round(auc_from_pred(y, preds), 4),
      acc_cv = round(mean((preds >= 0.5) == (y == 1L), na.rm = TRUE), 4)
    )
  }))

  results[, features_in_model := sapply(seq_len(.N), function(i) {
    paste(ordered_features[1:i], collapse = " + ")
  })]

  results
}
