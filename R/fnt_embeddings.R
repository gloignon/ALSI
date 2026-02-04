
# Start and configure Python backend for embeddings
start_python_backend <- function(venv_name = "textenv",
                                 use_gpu = TRUE,
                                 force_recreate = FALSE,
                                 packages = c("torch", "transformers", "sentence-transformers",
                                              "scipy", "numpy", "nltk"),
                                 python = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }
  
  if (!is.null(python)) {
    reticulate::use_python(python, required = TRUE)
  }
  
  if (isTRUE(force_recreate) && reticulate::virtualenv_exists(venv_name)) {
    reticulate::virtualenv_remove(venv_name)
  }
  
  if (!reticulate::virtualenv_exists(venv_name)) {
    reticulate::virtualenv_create(venv_name)
    reticulate::virtualenv_install(venv_name, packages = packages)
  }
  
  reticulate::use_virtualenv(venv_name, required = TRUE)
  
  if (isTRUE(use_gpu)) {
    Sys.unsetenv("ALSIO_FORCE_CPU")
  } else {
    Sys.setenv(ALSIO_FORCE_CPU = "1")
  }
  
  invisible(reticulate::py_config())
}


# This will embed the sentences.
# mode can be "basic", "prompt", or "query"
# instruction is the prompt or query to use, it will be ignored in 'basic' mode
#
## Models to consider for French:
# HIT-TMG/KaLM-embedding-multilingual-mini-instruct-v1
# dangvantuan/french-document-embedding  very fast!
# dangvantuan/sentence-camembert-base
# intfloat/multilingual-e5-large-instruct
# paraphrase-multilingual-MiniLM-L12-v2
# Lajavaness/bilingual-embedding-large
encode_embeddings <- function(dt_corpus,
                              model_name = "Lajavaness/bilingual-embedding-large",
                              mode = "basic",
                              batch_size = 32,
                              instruction = "Identifiez le thème principal et secondaire dans le texte.") {
  
  dt <- copy(dt_corpus)
  source_python("py/embed_sentences_instruct.py")
  
  # Load the model
  load_embedding_model(model_name)
  
  # Prepare input: unique sentences by doc and sentence_id
  dt_sentences <- as.data.table(dt)[
    , .(sentence = first(sentence)), by = .(doc_id, sentence_id)
  ][order(doc_id, sentence_id)]
  
  results <- list()
  doc_ids <- unique(dt_sentences$doc_id)
  n_docs <- length(doc_ids)
  total_sentences <- nrow(dt_sentences)
  processed_sentences <- 0
  failed_docs <- character(0)
  
  start_time <- Sys.time()
  
  print_status <- function(processed, total, start_time) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    speed <- processed / elapsed * 60  # sentences per minute
    remaining <- (total - processed) / speed * 60  # seconds
    cat(sprintf("\rProcessed %d/%d sentences (%.1f sent/min) — ~%.1f min left     ",
                processed, total, speed, remaining / 60), sep = "")
    flush.console()
  }
  
  for (i in seq_along(doc_ids)) {
    doc <- doc_ids[i]
    doc_dt <- dt_sentences[doc_id == doc]
    n_sent_in_doc <- nrow(doc_dt)
    
    embeddings <- tryCatch({
      emb_matrix <- embed_sentences(doc_dt$sentence, mode = mode, 
                                    instruction = instruction, bs = batch_size)
      
      # Fix for 1D case (only one sentence)
      if (is.null(dim(emb_matrix))) {
        emb_matrix <- matrix(emb_matrix, nrow = 1)
      }
      
      cbind(doc_dt, as.data.table(emb_matrix))
    }, error = function(e) {
      warning(sprintf("Embedding failed for doc_id = %s: %s", doc, e$message))
      failed_docs <<- c(failed_docs, doc)
      NULL
    })
    
    if (!is.null(embeddings)) {
      results[[length(results) + 1]] <- embeddings
      processed_sentences <- processed_sentences + n_sent_in_doc
    }
    
    if (i %% 3 == 0 || i == n_docs) {
      print_status(processed_sentences, total_sentences, start_time)
    }
  }
  
  cat("\nDone!\n")
  
  dt_embed <- rbindlist(results, use.names = TRUE, fill = TRUE)
  
  # Rename embedding columns
  n_dim <- ncol(dt_embed) - 3
  emb_cols <- names(dt_embed)[(ncol(dt_embed) - n_dim + 1):ncol(dt_embed)]
  setnames(dt_embed, emb_cols, paste0("dim", seq_len(n_dim)))
  
  # Optional: message if anything failed
  if (length(failed_docs)) {
    message(sprintf("⚠️ Embedding failed for %d document(s): %s", length(failed_docs), 
                    paste(failed_docs, collapse = ", ")))
  }
  
  return(dt_embed)
}


# computes sentence embeddings for a whole corpus,
# returns sentence embeddings and document mean embeddings
corpus_embeddings <- function(dt_corpus,
                                model_name = "Lajavaness/bilingual-embedding-large",
                                mode = "basic",
                                batch_size = 32,
                                instruction = "Identifiez le thème principal et secondaire dans le texte."
                              ) {

  dt_sentences <- as.data.table(dt_corpus)[
    , .(sentence = first(sentence)), by = .(doc_id, sentence_id)
  ][order(doc_id, sentence_id)]
  
  dt_embeddings <- encode_embeddings(
    dt_sentences,
    model_name = model_name,
    mode = mode,
    batch_size = batch_size,
    instruction = instruction
  )
  
  # now we can do mean per doc
  dim_cols <- grep("^dim", names(dt_embeddings), value = TRUE)
  dt_doc_mean <- dt_embeddings[, lapply(.SD, mean, na.rm = TRUE), by = doc_id, .SDcols = dim_cols]
  
  result <- list(
    dt_sent_embeddings = dt_embeddings,
    dt_doc_embeddings = dt_doc_mean  
    )
  
  return(result)
}


# Example usage:
# 
# embeddings <- corpus_embeddings(
#   dt_corpus = dt_corpus
# )
# 

# PCA/UMAP ----
# Load the pretrained models
#umap_model <- uwot::load_uwot("models/wiki_umap_model_doc.uwot")
#pca_reference <- readRDS("models/wiki_pca_reference_doc.Rds")

# apply the pca model to our embeddings
#pca_corpus <- predict(pca_reference, dt_doc_mean[, .SD, .SDcols = dim_cols])

# 
# # apply the umap model
# library(uwot)
# umap_kalm <- umap_transform(model = umap_model, dt_doc_mean[, .SD, .SDcols = dim_cols])
# df_kalm <- data.frame(umap_kalm)
# df_kalm <- cbind(df_kalm, dt_doc_sent_id)
# 
# # Validation plots ----
# 
# # make a "grade" vector from doc_id
# dt_corpus[, class := as.numeric(str_extract(doc_id, "(?<=g)\\d+"))]
# 
# 
# # plot using ggplot2
# library(ggplot2)
# ggplot(df_kalm, aes(x = X1, y = X2, color = factor(class))) +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "PCA on KaLM embeddings")
# ggplot(df_kalm, aes(x = X1, y = X2, color = X3, shape = factor(class))) +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "PCA on KaLM embeddings")
# 
# # now plot in 3d with class as color
# library(plotly)
# fig <- plot_ly(
#   df_kalm,
#   x = ~X1, y = ~X2, z = ~X3,
#   color = ~factor(class),
#   colors = c("blue", "red"),
#   marker = list(size = 8, opacity = 0.6)  
# ) %>%
#   add_markers() %>%
#   layout(scene = list(
#     xaxis = list(title = 'PC1'),
#     yaxis = list(title = 'PC2'),
#     zaxis = list(title = 'PC3')
#   ))
# fig
