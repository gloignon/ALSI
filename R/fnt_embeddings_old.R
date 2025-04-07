library(reticulate)  # yes, sadly we must use a python back-end for this
library(tidyverse)
library(data.table)


# Create a new environment (name it 'textenv' for example)
# virtualenv_create("textenv")
#
# # Install required Python packages
# virtualenv_install("textenv", packages = c("torch", "transformers", "sentence-transformers", "scipy", "numpy", "nltk"))

use_virtualenv("textenv", required = TRUE)
# py_run_string("
# import nltk
# nltk.download('punkt_tab')
# ")

cosine_distance <- function(x, y) {
  1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}



features <- readRDS("D:/OD_UQAM/OneDrive - UQAM/_RECHERCHE/ALSI_dev/ALSI/corpus/french_corpus_parsed_raw.Rds")
all_sentences <- features$parsed_corpus %>% distinct(doc_id, sentence_id, sentence) %>% pull(sentence)
  
# Embed the sentences
embeddings <- textEmbed(
  texts = all_sentences,
  #model = "intfloat/multilingual-e5-large-instruct",
  model = "dangvantuan/sentence-camembert-base",
  remove_non_ascii = FALSE,
  tokenizer_parallelism = TRUE,
  logging_level = "error",
  keep_token_embeddings = FALSE, 
  batch_size = 16,
  dim_name = "dim"  # will produce Dim1, Dim2, etc. as it doesn't want to respect case
)
# save just the embeddings
# saveRDS(embeddings$text$text, "out/all_sentence_embeddings.Rds")

# make a data.frame for unique sentences, with doc_id
df_all_sentences <- features$parsed_corpus %>% distinct(doc_id, sentence_id, sentence) 

# add back the doc_id information
df_embeddings <- embeddings$texts$texts
df_all_sentences <- df_all_sentences %>% 
  bind_cols(df_embeddings) %>% data.table

# Get list of dimension column names (e.g., Dim1, Dim2, ...)
dim_cols <- grep("^Dim", names(df_all_sentences), value = TRUE)

# Document means
dt_document_mean <- df_all_sentences[, lapply(.SD, mean, na.rm = TRUE), by = doc_id, .SDcols = dim_cols]

# Merge 
dt_emb_mean <- merge(df_all_sentences, dt_document_mean, by = "doc_id", 
                     all.x = TRUE, sort = FALSE, suffixes = c("", "_avg"))

avg_cols <- paste0(dim_cols, "_avg")

# Sentence to document cosine distance
mat <- as.matrix(dt_emb_mean[, ..dim_cols])
mat_avg <- as.matrix(dt_emb_mean[, ..avg_cols])
dot_products <- rowSums(mat * mat_avg)
norms_mat <- sqrt(rowSums(mat^2))
norms_avg <- sqrt(rowSums(mat_avg^2))
cosine_sim <- dot_products / (norms_mat * norms_avg)

dt[, (result_col) := cosine_sim]

# document average columns
docavg_cols <- paste0(dim_cols, "_docavg")  

# Sentence to doc cosine distance
compute_cosine_to_doc_mean(df_all_sentences)

# Step 3: Efficient cosine similarity using matrix operations
dot_products <- rowSums(mat * mat_avg)
norms_mat <- sqrt(rowSums(mat^2))
norms_avg <- sqrt(rowSums(mat_avg^2))
cosine_sim <- dot_products / (norms_mat * norms_avg)

# Step 4: Assign result
df_all_sentences[, cosine_doc_mean := cosine_sim]


# Compute cosine to next sentence per document
df_cosine_to_next <- df_all_sentences %>%
  group_by(doc_id) %>%
  mutate(
    cosine_to_next = map_dbl(row_number(), function(i) {
      if (i == n()) return(NA_real_)
      x <- as.numeric(across(all_of(dim_cols))[i, ])
      y <- as.numeric(across(all_of(dim_cols))[i + 1, ])
      cosine_distance(x, y)
    })
  ) %>%
  ungroup()
  # aggregate by document
df_mean_cosine_to_next <- df_cosine_to_next %>%
  group_by(doc_id) %>%
  summarise(
    mean_cosine_to_next = mean(cosine_to_next, na.rm = TRUE),
    .groups = "drop"
  )

docavg_cols <- grep("^Dim", names(df_sentence_avg), value = TRUE)

# Cosine distance between sentences and document mean vector
df_cosine_to_doc <- df_all_sentences %>%
  left_join(df_sentence_avg, by = "doc_id", suffix = c("", "_docavg")) %>% 
  rowwise() %>%
  mutate(
    cosine_to_docmean = cosine_distance(
      as.numeric(c_across(all_of(dim_cols))),
      as.numeric(c_across(all_of(docavg_cols)))
    )
  ) %>%
  ungroup()

  # ...by document
df_mean_docdev <- df_cosine_to_doc %>%
  group_by(doc_id) %>%
  summarise(
    avg_cosine_to_docmean = mean(cosine_to_docmean, na.rm = TRUE),
    .groups = "drop"
  )

# Time to gather the results
# We'll write df_all_sentences to disk as it might be too large to
# keep in memory
saveRDS(df_all_sentences, "out/df_all_sentence_embeddings_camembert-base.Rds")

# But we can reduce using PCA and keep only the first 3 dimensions
embedding_matrix <- df_sentence_avg %>%
  select(all_of(dim_cols)) %>%
  as.matrix()
pca_results <- prcomp(embedding_matrix, center = TRUE, scale. = TRUE)
pca_coords <- as.data.frame(pca_results$x[, 1:3])
names(pca_coords) <- c("PC1", "PC2", "PC3")
pca_coords$doc_id <- df_sentence_avg$doc_id

result <- df_mean_cosine_to_next %>% 
  left_join(df_mean_docdev, by = "doc_id") %>% 
  left_join(pca_coords, by = "doc_id") 

# if we want tsne reduction too
library(Rtsne)
tsne_result <- Rtsne(embedding_matrix, dims = 3, perplexity = 1 , verbose = TRUE)
df_tsne_result <- as.data.frame(tsne_result$Y) %>% 
  # rename to tsne_1, tsne_2, tsne_3
  setNames(paste0("tsne_", 1:3)) %>%
  mutate(doc_id = df_sentence_avg$doc_id)
# add to result
result <- left_join(result, df_tsne_result, by = "doc_id")

# and why not UMAP too

library(uwot)
#umap_coords <- umap(embedding_matrix, n_components = 3)
umap_coords <- uwot::umap(embedding_matrix, n_components = 3, ret_model = TRUE)
# save model
saveRDS(umap_coords$model, "out/umap_model.Rds")

df_umap_result <- as.data.frame(umap_coords$embedding) %>% 
  # rename to umap_1, umap_2, umap_3
  setNames(paste0("umap_", 1:3)) %>%
  mutate(doc_id = df_sentence_avg$doc_id)
# add to result
result <- left_join(result, df_umap_result, by = "doc_id")


result$grade <- as.numeric(as.character(str_extract(result$doc_id, "(?<=g)\\d+")))

# save result
saveRDS(result, "out/df_sentence_embeddings_result.Rds")

# Now we can plot the texts in 3D!
library(plotly)
fig <- plot_ly(
  result,
  x = ~umap_1, y = ~umap_2, z = ~umap_3,
  color = ~grade,
  colors = c("blue", "red"),
  marker = list(size = 8, opacity = 0.6)  
) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')
  ))
fig
fig <- plot_ly(
  result,
  x = ~tsne_1 , y = ~tsne_2 , z = ~tsne_3,
  color = ~grade,
  colors = c("blue", "red"),
  marker = list(size = 8, opacity = 0.6)  # 
) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')
  ))
fig
fig <- plot_ly(
  result,
  x = ~PC1 , y = ~PC2 , z = ~PC3,
  color = ~grade,
  colors = c("blue", "red"),
  marker = list(size = 8, opacity = 0.6)  #
) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')
  ))
fig

# check correlation between grade and cosine features
cor(result$grade, result$mean_cosine_to_next, use = "complete.obs")
