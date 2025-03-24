library(data.table)
library(tidyverse)
library(caret)

# accept a vector of probabilities and return the surprisal
# if 0, return 0, otherwise return -log2
smooth_surprisal <- function(x) {
  s <- ifelse(x == 0, 0, -log2(x))
  return(s)
}

adjacent_accuracy <- function(pred, actual) {
  sum((pred == actual | pred == actual - 1 | pred == actual + 1)) / length(pred)
}
adjacent_accuracy_loss <- function(data, lev = NULL, model = NULL) {
  obs <- as.numeric(data$obs)
  pred <- as.numeric(data$pred)
  
  correct <- abs(pred - obs) <= 1  # True if prediction is within ±1
  adj_acc <- mean(correct)
  
  # calculate accuracy while we're at it
  acc <- sum(pred == obs) / length(pred)
  
  return(c(adj_acc = adj_acc,
    acc = acc))
}

features <- readRDS("corpus/french_corpus_20240403_features.Rds")

# Assemble a feature set from the list of tables in features


# make doc summaries for lexical frequencies
# TODO: send to main pipeline
df_lexical_basic_features <- features$lexical_db$eqol %>% select(doc_id, sentence_id, token_id, eqol_freq_u, compte) %>%  
  left_join(features$lexical_db$franqus %>% select(doc_id, sentence_id, token_id, franqus_freq_u, franqus_grade)) %>% 
  left_join(features$lexical_db$manulex %>% select(doc_id, sentence_id, token_id, manulex_freq_u, manulex_grade)) %>%
  left_join(features$lexical_db$flelex %>% select(doc_id, sentence_id, token_id, flelex_freq_u, flelex_grade))

df_lexical_token <- df_lexical_basic_features %>% 
  filter(compte == TRUE) %>%
  group_by(doc_id) %>%
  summarise(
    avg_freq_eqol = mean(log10(eqol_freq_u) + 3, na.rm = TRUE),
    avg_freq_franqus = mean(log10(franqus_freq_u) + 3, na.rm = TRUE),
    avg_grade_franqus = mean(as.numeric(franqus_grade), na.rm = TRUE),
    avg_freq_manulex = mean(log10(manulex_freq_u) + 3, na.rm = TRUE),
    avg_freq_flelex = mean(log10(flelex_freq_u) + 3, na.rm = TRUE),
    # proportion in reference corpus
    prop_franqus = mean(!is.na(franqus_freq_u), na.rm = TRUE),
    prop_manulex = mean(!is.na(manulex_freq_u), na.rm = TRUE),
    prop_eqol = mean(!is.na(eqol_freq_u), na.rm = TRUE),
    prop_flelex = mean(!is.na(flelex_freq_u), na.rm = TRUE)
  )

df_lexical_type <- df_lexical_basic_features %>% 
  filter(compte == TRUE) %>% 
  group_by(doc_id) %>%
  distinct(doc_id, sentence_id, token_id, .keep_all = TRUE) %>%
  summarise(
    avg_freq_eqol_type = mean(log10(eqol_freq_u) + 3, na.rm = TRUE),
    avg_freq_franqus_type = mean(log10(franqus_freq_u) + 3, na.rm = TRUE),
    avg_freq_manulex_type = mean(log10(manulex_freq_u) + 3, na.rm = TRUE),
    avg_freq_flelex_type = mean(log10(flelex_freq_u) + 3, na.rm = TRUE),
    # proportion in reference corpus
    prop_franqus_type = mean(!is.na(franqus_freq_u), na.rm = TRUE),
    prop_manulex_type = mean(!is.na(manulex_freq_u), na.rm = TRUE),
    prop_eqol_type = mean(!is.na(eqol_freq_u), na.rm = TRUE),
    prop_flelex_type = mean(!is.na(flelex_freq_u), na.rm = TRUE)
  )

df_head <- features$head_final %>% 
  filter(!is.na(head_final)) %>% 
  group_by(doc_id) %>%
  summarise(prop_head_final = mean(head_final, na.rm = TRUE))

df_surprisal <- features$pos_surprisal$doc_surprisal %>% 
  group_by(doc_id) %>%
  summarise(mean_pos_surprisal = mean(mean_pos_surprisal, na.rm = TRUE),
            sd_pos_surprisal = mean(sd_pos_surprisal, na.rm = TRUE))

df <- features$simple_counts$upos_counts %>% 
  left_join(features$parsed_corpus %>% select(doc_id, class) %>% distinct()) %>%
  left_join(features$simple_counts$doc_level_counts) %>% 
  left_join(features$verb_tenses$proportions %>% select(-word_count)) %>% 
  left_join(features$lexical_diversity) %>%
  left_join(features$heights %>% select(-n, -s, -total_paths)) %>% 
  left_join(features$syntactic) %>%
  left_join(df_surprisal) %>%
  left_join(df_lexical_token) %>% 
  left_join(df_lexical_type) %>% 
  # experimental, create -log2 features from all PROP features
  mutate(across(starts_with("prop_"), ~ smooth_surprisal(.))) %>% 
  # also rename those to "surprisal_" insteand of "prop_"
  rename_with(~ str_replace(., "prop_", "log2_"), starts_with("prop_"))
hist(df$log2_PROPN)

# save features (df) to Rds file
saveRDS(df, file = "corpus/french_corpus_20240403_features.Rds") 

df <- readRDS(file = "corpus/french_corpus_20240403_features.Rds") 

# Classification ----

# Get the correlation of each feature with text length (word count)
correlation <- cor(df %>% select(-class, -doc_id, -word_count), df$word_count, method = "spearman", use = "pairwise.complete.obs")
sum_correlation <- data.frame(
  feature = row.names(correlation),
  correlation = correlation
)
# make a list of features that have too high correlation with word count
high_correlation <- sum_correlation %>% filter(abs(correlation) > 0.5) %>% pull(feature)

df_4_training <- df %>% select(-doc_id) %>% na.omit() %>% 
  # remove features that start or end with "count"
  select(-starts_with("count_"), -ends_with("_count")) %>% 
  # remove features that have "_count_" in the name
  select(-contains("_count_")) %>% 
  # exclude n_complex_nominals, n_complex_verbs, n_clause
  select(-n_complex_nominal, -n_complex_verb, -n_clause, -TTR, -TTR_content) %>% 
  # remove any that are in high_correlation
  select(-any_of(high_correlation))
  
# use caret to run a simple 5-fold cv classification experiment
# first with random forest
set.seed(123)

# check interesting features using Boruta package
library(Boruta)

boruta <- Boruta(class ~ ., data = df_4_training)
sum_boruta <- data.frame(
  feature = names(boruta$finalDecision),
  decision = boruta$finalDecision
)

# keep the Confirmed features only
df_select <- df_4_training %>% 
  select(class, all_of((sum_boruta %>% filter(decision == "Confirmed") %>% pull(feature)))) %>% 
  # add a L prefix to class and pad with a 0
  mutate(class = as.factor(paste0("L", str_pad(class, 2, pad = "0"))))

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = adjacent_accuracy_loss
)
ctrl_simple <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE
)

rf_grid <- expand.grid(
  mtry = c(3,4),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 2)
)

m1 <- train(class ~ .,
            data = df_select,
            method = "ranger",
            trControl = ctrl,
            tuneGrid = rf_grid,
            metric = "adj_acc"  
)
m1  # acc 42.3% / 66.2% adj acc 
# varImp(m1)
caret::confusionMatrix(m1, norm = "none")


# same but treat class as a numeric var
m2 <- train(as.ordered(class) ~ ., data = df_select, method = "rf", trControl = ctrl)
m2  # acc 40.1%
varImp(m2)
caret::confusionMatrix(m2, norm = "none")


# m2 <- train(class ~ ., data = df_select, method = "multinom", trControl = ctrl)
# m2  # acc 37,7%
# m3 <- train(class ~ ., data = df_select, method = "svmRadial", trControl = ctrl)
# m3  # acc 26,5%

# again but use iteration_range instead of ntree_limit
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

df_4_training %>%
  select(class, mean_pos_surprisal, sd_pos_surprisal, avg_sent_height ) %>%
  pivot_longer(-class) %>%
  ggplot(aes(x = as.factor(class), y = value)) +
  geom_boxplot(outliers = FALSE, notch = TRUE) +
  # add a regression line
  geom_line(aes(
    group = name,
  ), color = "red",
  stat = "smooth",
  method = "lm") +
  facet_wrap( ~ name, scales = "free_y", ncol = 3)
