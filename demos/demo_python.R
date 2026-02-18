# This is a demo of functions that require a python backend, in that case embeddings
# and LLM surprisal/entropy.
# The code can be run as-is if you previously generated the parsed corpus with demo_parse_tag.R
# or can be adapted for your own needs.
library(data.table)
library(reticulate)
py_require("transformers")
py_require("torch")
py_require("tokenizers")
py_require("sentence_transformers")
py_require("torch")
py_require("numpy")
py_require("scipy")
py_require("nltk")

source('R/fnt_embeddings.R', encoding = 'UTF-8')

dt_parsed_corpus <- readRDS('out/demo_parsed_tagged.Rds')

dt_doc_classes <- unique(data.table(
  doc_id = dt_parsed_corpus$doc_id,
  class = ifelse(grepl("viki", dt_parsed_corpus$doc_id), 1L, 2L)
))

# Select 100 texts for the demo (you can adjust this number based on your needs and computational resources)
set.seed(123)  # for reproducibility
sampled_docs <- sample(unique(dt_parsed_corpus$doc_id), size = min(100, length(unique(dt_parsed_corpus$doc_id))), replace = FALSE)
dt_parsed_corpus <- dt_parsed_corpus[doc_id %in% sampled_docs]

# Embeddings
list_embeddings <- corpus_embeddings(
  dt_corpus = dt_parsed_corpus, 
  batch_size = 8
)
setDT(list_embeddings$dt_sent_embeddings)

dt_embeddings <- list_embeddings$dt_doc_embeddings
# optionnal: flatten embeddings to 2D and plot by class
# this is for demonstration purposes, you do not need if you don't want to visualize the embeddings
# For the demo data, there should not be any clear pattern as we are comparing pairs of texts aligned by theme
library(Rtsne)
library(ggplot2)
tsne_result <- Rtsne(dt_embeddings, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
dt_embeddings[, tsne_x := tsne_result$Y[,1]]
dt_embeddings[, tsne_y := tsne_result$Y[,2]]
dt_embeddings <- merge(dt_embeddings, dt_doc_classes, by = "doc_id")
ggplot(dt_embeddings, aes(x = tsne_x, y = tsne_y, color = factor(class))) +
  geom_point() +
  labs(title = "t-SNE of Document Embeddings", color = "Class") +
  theme_minimal()

# LLM surprisal and entropy
dt_surprisal_mlm <- llm_surprisal_entropy(
  dt_parsed_corpus,
  model_name = "almanach/moderncamembert-base",
  mode = "mlm",
  # context = "The following is a French sentence from vikipedia or wikidia.", 
  batch_size = 8
)

dt_surprisal_ar <- llm_surprisal_entropy(
  dt_parsed_corpus,
  model_name = "lightonai/pagnol-small",
  mode = "ar",
  add_prefix_space=TRUE,
  # context = "The following is a French sentence from vikipedia or wikidia.", 
  batch_size = 8
)


# Optional - boxplots of surprisal and entropy by class
dt_surprisal_mlm <- merge(dt_surprisal_mlm, dt_doc_classes, by = "doc_id")

dt_surprisal_mlm_docs <- dt_surprisal_mlm[, .(llm_surprisal = mean(llm_surprisal), llm_entropy = mean(llm_entropy)), by = .(doc_id, class)]

ggplot(dt_surprisal_mlm_docs, aes(x = factor(class), y = llm_surprisal)) +
  geom_boxplot() +
  labs(title = "Surprisal by Class (MLM)", x = "Class", y = "Surprisal") +
  theme_minimal()

ggplot(dt_surprisal_mlm_docs, aes(x = factor(class), y = llm_entropy)) +
  geom_boxplot() +
  labs(title = "Entropy by Class (MLM)", x = "Class", y = "Surprisal") +
  theme_minimal()



