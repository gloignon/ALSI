# Demo classification script

library(data.table)
library(tidyverse)
library(caret)

# Load the docwise aggregated features
df <- readRDS(file = "corpus/french_corpus_20250430_features.Rds")

# # Add embedding based features
# df_embed <- readRDS("out/df_sentence_embeddings_result.Rds")

# # merge
# df <- merge(df, df_embed, by = "doc_id")

ordinal_cross_entropy <- function(data, lev = NULL, model = NULL) {
  obs <- as.integer(data$obs)
  pred <- as.matrix(data[, lev])  # predicted probabilities
  
  K <- ncol(pred)  # Number of classes
  
  # Convert to cumulative probabilities
  cum_prob <- t(apply(pred, 1, cumsum))
  
  # Build ordinal cross-entropy loss
  loss <- 0
  for (k in 1:K) {
    # If true label <= k => maximize log of cumulative probability
    loss <- loss - mean(ifelse(obs <= k, log(pmax(cum_prob[, k], 1e-15)), 0))
    
    # If true label > k => maximize log of 1 - cumulative probability
    loss <- loss - mean(ifelse(obs > k, log(pmax(1 - cum_prob[, k], 1e-15)), 0))
  }
  
  loss <- loss / K
  
  # Accuracy — convert from probabilities to class predictions
  predicted_class <- apply(pred, 1, which.max)
  acc <- mean(predicted_class == obs)
  
  # Pearson correlation for prediction tracking
  pearson <- cor(predicted_class, obs)
  
  # MAE (for reporting)
  mae <- mean(abs(predicted_class - obs))
  
  # Adjacent accuracy
  correct <- abs(predicted_class - obs) <= 1  # True if prediction is within ±1
  adj_acc <- mean(correct)
  
  return(c(oce_loss = loss,
           acc = acc,
           adj_acc = adj_acc,
           r = pearson,
           mae = mae))
}


adjacent_accuracy_loss <- function(data, lev = NULL, model = NULL) {
  obs <- as.numeric(data$obs)
  pred <- as.numeric(data$pred)
  
  correct <- abs(pred - obs) <= 1  # True if prediction is within ±1
  adj_acc <- mean(correct)
  
  # calculate accuracy while we're at it
  acc <- sum(pred == obs) / length(pred)
  
  # add pearson r, to help comparison with other classifiers that use it
  pearson <- cor(pred, obs)
  
  # and MAE is often reported so let's compute it
  mae <- mean(abs(pred - obs))
  
  # Huber-style loss for robustness to outliers
  delta <- 1
  huber_loss <- ifelse(abs(pred - obs) < delta,
                       0.5 * (pred - obs)^2,
                       delta * (abs(pred - obs) - 0.5 * delta))
  huber_mae <- mean(huber_loss)
  
  return(c(adj_acc = adj_acc,
    acc = acc,
    r = pearson,
    mae = mae,
    huber_mae = huber_mae))
}


# Get the correlation of each feature with text length (word count)
correlation <- cor(df %>% select(-class, -doc_id, -word_count), df$word_count, method = "spearman", use = "pairwise.complete.obs")
sum_correlation <- data.frame(
  feature = row.names(correlation),
  correlation = correlation
)
# make a list of features that have too high correlation with word count
high_correlation <- sum_correlation %>% filter(abs(correlation) > 0.5) %>% pull(feature)

df_select <- df %>% select(-doc_id) %>% 
  # remove features that start or end with "count"
  select(-starts_with("count_"), -ends_with("_count")) %>% 
  # remove features that have "_count_" in the name
  select(-contains("_count_")) %>% 
  # exclude n_complex_nominals, n_complex_verbs, n_clause
  select(-n_complex_nominal, -n_complex_verb, -n_clause, -TTR, -TTR_content) %>% 
  # remove any that are in high_correlation
  select(-any_of(high_correlation)) %>% 
  na.omit() %>% 
  mutate(class = as.factor(paste0("L", str_pad(class, 2, pad = "0"))))
  
# what columns have NAs in df_select ?
#colSums(is.na(df_select))

# use caret to run a simple 5-fold cv classification experiment
# first with random forest
set.seed(123)


# Correlation with class
correlation_class <- cor(df_select %>% select(-class),
                         as.numeric(df_select$class),
                         method = "spearman",
                         use = "pairwise.complete.obs") %>% 
  as.data.frame() %>%
  rownames_to_column("feature") %>% 
  rename(cor = V1) %>% 
  arrange(-abs(cor))
correlation_class

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = ordinal_cross_entropy
)
ctrl_repeat <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 200,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = ordinal_cross_entropy
)

ctrl_simple <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE
)

ranger_grid <- expand.grid(
  mtry = c(3),
  splitrule = c("extratrees"),
  min.node.size = c(1)
)

# Experiment 1 -----

m1 <- train(class ~ .,
            data = df_select,
            method = "ranger",
            trControl = ctrl,
            tuneGrid = ranger_grid,
            metric = "oce_loss"
)
m1  # acc 43% / 68% adj acc  / rho 
caret::confusionMatrix(m1, norm = "none")

# get the average acc across the repetitions
quantile(m1$resample$acc, c(0.025, .5, 0.975))

# get the 95% CI for the adj_acc
quantile(m1$resample$adj_acc, c(0.025, .5, 0.975))

# get the median and 95% CI for pearson r
quantile(m1$resample$r, c(0.025, .5, 0.975))

# get MAE
mean(abs(as.numeric(m1$pred$pred) - as.numeric(m1$pred$obs)))


m1_rf <- train(class ~ .,
               data = df_select,
               method = "rf",
               trControl = ctrl,
               metric = "oce_loss"  
)
m1_rf
varImp(m1_rf)


# # same but treat class as a numeric var
# m2 <- train(as.ordered(class) ~ ., data = df_select, method = "rf", trControl = ctrl)
# m2  # acc 40.1%
# varImp(m2)
# caret::confusionMatrix(m2, norm = "none")


# m2 <- train(class ~ ., data = df_select, method = "multinom", trControl = ctrl)
# m2  # acc 37,7%
# m3 <- train(class ~ ., data = df_select, method = "svmRadial", trControl = ctrl)
# m3  # acc 26,5%

m4 <- train(
  class ~ .,
  data = df_select,
  method = "xgbTree",
  trControl = ctrl,
  verbosity = 0
)
m4  # average acc 39,1%

m5 <- train(
  class ~ .,
  data = df_select,
  method = "svmRadial",
  trControl = ctrl,
  tuneLength = 10, 
)
m5  
# ranger: adj_acc 0.6525467 acc  0.4072339
# multinom:  decay 0.0013335214  0.6040980  0.3550591
# svmRadial: 


# show confusion matrix for m4
confusionMatrix(m4, norm = "none")

# Experiment 2 : include a feature that leaks the primary / secondary level
df_select_niv <- df_select %>% 
  # if > 6 then "secondary" else "primary"
  mutate(niv = if_else(as.numeric(str_sub(class, 2, 3)) > 6, "secondary", "primary"))
table(df_select_niv$niv)

mn1 <- train(class ~ .,
            data = df_select_niv,
            method = "rf",
            trControl = ctrl,
            tuneGrid = rf_grid,
            metric = "adj_acc"  
)
mn1  # acc 47% / adj acc 72 %
caret::confusionMatrix(mn1, norm = "none")
# pearson R between class and predicted class
cor(as.numeric(mn1$pred$pred), as.numeric(mn1$pred$obs))  # .876

## Experiment 3: classify primary and secondary separately ----
df_select_prim <- df_select_niv %>% filter(niv == "primary") %>% 
  select(-any_of(c("niv", "niv_pred"))) %>% 
  # drop empty class levels
  droplevels()

m1_prim <- train(class ~ .,
            data = df_select_prim,
            method = "rf",
            trControl = ctrl,
            metric = "adj_acc"  
)
m1_prim  # acc 51.9% / adj acc 77.5% 
confusionMatrix(m1_prim, norm = "none")
# show MAE
mean(abs(as.numeric(m1_prim$pred$pred) - as.numeric(m1_prim$pred$obs)))
# show pearson R
cor(as.numeric(m1_prim$pred$pred), as.numeric(m1_prim$pred$obs))
varImp(m1_prim)

# redo for secondary levels
df_select_sec <- df_select_niv %>% filter(niv == "secondary") %>% 
  select(-any_of(c("niv", "niv_pred"))) %>% 
  # drop empty class levels
  droplevels()
m1_sec <- train(class ~ .,
            data = df_select_sec,
            method = "rf",
            trControl = ctrl,
            metric = "adj_acc"  
)
m1_sec  # acc 41.9% / adj acc 65.1%
# show MAE
mean(abs(as.numeric(m1_sec$pred$pred) - as.numeric(m1_sec$pred$obs)))
# show pearson r
cor(as.numeric(m1_sec$pred$pred), as.numeric(m1_sec$pred$obs))
confusionMatrix(m1_sec, norm = "none")
varImp(m1_sec)

# get the 10 most important vars
m1_prim_vip <- varImp(m1_prim)$importance %>% 
  rownames_to_column("Feature") %>%
  as.data.frame() %>%
  arrange(-Overall) %>% 
  head(16)
m1_sec_vip <- varImp(m1_sec)$importance %>% 
  rownames_to_column("Feature") %>%
  as.data.frame() %>%
  arrange(-Overall) %>% 
  head(16)

# plot the most important vars, boxplot by class
df_select_prim %>% 
  select(class, all_of(m1_prim_vip$Feature)) %>% 
  # exclude those that start with "log2_"
  select(-starts_with("log2_")) %>%
  # make sure class levels are L01 to L06
  mutate(class = factor(class, levels = levels(class)[order(as.numeric(str_sub(levels(class), 2, 3)))]) ) %>%
  pivot_longer(-class) %>%
  # reoder levels of name by importance in m1_prim_vip
  mutate(name = factor(name, levels = m1_prim_vip$Feature)) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outliers = FALSE, notch = TRUE) +
  facet_wrap(~name, scales = "free_y", ncol = 4)

df_select_sec %>% 
  select(class, all_of(m1_sec_vip$Feature)) %>% 
  # order features by importance
  mutate(class = factor(class, levels = levels(class)[order(as.numeric(str_sub(levels(class), 2, 3)))]) ) %>%
  pivot_longer(-class) %>%
  mutate(name = factor(name, levels = m1_sec_vip$Feature)) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outliers = FALSE, notch = TRUE) +
  facet_wrap(~name, scales = "free_y", ncol = 4)

## Experiment 4: first predict level (primary vs secondary) then route to the correct model ----
df_select_niv <- df_select_niv %>% 
  mutate(niv = as.factor(niv))

prim_vs_sec <- train(niv ~ .,
            data = df_select_niv %>% select(-class),
            method = "ranger",
            trControl = ctrl,
            metric = "acc"
)
prim_vs_sec  # acc 85%
confusionMatrix(prim_vs_sec, norm = "none")

# get the predicted level (primary or secondary)
df_select_niv <- df_select_niv %>% 
  mutate(niv_pred = predict(prim_vs_sec, newdata = df_select_niv %>% select(-class)))

# split the data into primary and secondary based on predicted level
df_select_prim_pred <- df_select_niv %>% 
  filter(niv_pred == "primary") %>% select(-niv_pred) %>% droplevels()
df_select_sec_pred <- df_select_niv %>% 
  filter(niv_pred == "secondary") %>% select(-niv_pred) %>% droplevels()

# train the primary model

m1_prim_pred <- train(class ~ .,
                data = df_select_prim_pred %>% select(-niv),
                method = "ranger",
                trControl = ctrl,
                tuneGrid = rf_grid,
                metric = "adj_acc"  
)
m1_sec_pred <- train(class ~ .,
                      data = df_select_sec_pred %>% select(-niv),
                      method = "ranger",
                      trControl = ctrl,
                      tuneGrid = rf_grid,
                      metric = "adj_acc"  
)
m1_prim_pred  # 0.7772928  0.5261886
m1_sec_pred  # 0.6268817  0.4012389

# More plots ----

df_select%>%
  select(class, mean_pos_surprisal, sd_pos_surprisal, avg_sent_height, content_sent_overlap,
         token_sent_overlap) %>%
  pivot_longer(-class) %>%
  ggplot(aes(x = as.factor(class), y = value)) +
  geom_boxplot(outliers = TRUE, notch = F) +
  # add a regression line
  geom_line(aes(
    group = name,
  ), color = "red",
  stat = "smooth",
  method = "lm") +
  facet_wrap( ~ name, scales = "free_y", ncol = 3)
  # display rho correlation, once per facet

