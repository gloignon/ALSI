# Demo of features that DEPEND on dependency tree analyses (dad joke)

library(data.table)
library(tidyverse)
source('R/fnt_heights.R', encoding = 'UTF-8')

# Controlled demo on a single sentence ----
library(udpipe)
udmodel_french <-
  udpipe_load_model(file = "models/french_gsd-remix_2.udpipe") # this is the linguistic model, by default we are using
    
dt_sentence <- udpipe(x = "Le chat mange la petite souris verte qui est entrée par le trou.", object = udmodel_french)
dt_sentence
sentence_graph_stats(dt_sentence, verbose = TRUE)

# Demo on vikidia vs wikipedia articles ----
dt_parsed_corpus <- readRDS('out/demo_parsed_tagged.Rds')

dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class = ifelse(grepl("viki", dt_parsed_corpus$doc_id), 1L, 2L)
))

dt_heights <- docwise_graph_stats(dt_parsed_corpus)

# Add class labels for plotting
dt_heights <- merge(dt_heights, dt_doc_classes, by = "doc_id")
# boxplots of avg_sent_height 
ggplot(dt_heights, aes(x = factor(class), y = avg_sent_height)) +
  geom_boxplot() +
  labs(title = "Average Sentence Height by Class", x = "Class", y = "Average Sentence Height") +
  theme_minimal()

# prop_hf by class
ggplot(dt_heights, aes(x = factor(class), y = prop_hf)) +
  geom_boxplot() +
  labs(title = "Proportion of Head Final Sentences by Class", x = "Class", y = "Proportion of Head Final Sentences") +
  theme_minimal()
