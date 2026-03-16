
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
                              model_name = "dangvantuan/french-document-embedding",
                              mode = "basic",
                              batch_size = 32,
                              instruction = "Identifiez le thème principal et les thèmes secondaires dans ce texte.") {
  
  dt <- copy(dt_corpus)
  source_python("py/embed_sentences_instruct.py")
  
  # Load the model (may download on first use — this can take a while)
  cat(sprintf("Loading model '%s' (downloading if not cached)...\n", model_name))
  flush.console()
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

      # Ensure we always have a proper R matrix (reticulate can
      # return 1D vectors for single sentences or numpy arrays
      # that R sees as arrays rather than matrices)
      if (is.null(dim(emb_matrix))) {
        emb_matrix <- matrix(emb_matrix, nrow = 1)
      } else {
        emb_matrix <- as.matrix(emb_matrix)
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


# Compute document-level coherence features from sentence embeddings.
# All metrics are O(n) per document.
# Input: dt_sent_embeddings from corpus_embeddings() (with doc_id, sentence_id, dim1..dimN)
# Returns a data.table with one row per doc_id:
#   - emb_thematic_dispersion: mean cosine distance from each sentence to the document centroid
#   - emb_centroid_distance_sd: SD of cosine distances to centroid
#   - emb_sequential_similarity: mean cosine similarity between consecutive sentences
#   - emb_mean_semantic_gap: mean cosine distance between consecutive sentences
#   - emb_max_semantic_gap: largest cosine distance between consecutive sentences
#   - emb_topic_drift: mean cosine distance between consecutive 3-sentence block centroids
#   - emb_mean_novelty: mean cosine distance of each sentence to the running centroid of all previous sentences
#   - emb_n_topics: optimal number of sentence clusters (silhouette on L2-normalized embeddings, k=1..5)
#   - emb_convexity: conceptual convexity (Gärdenfors) — mean NN cosine similarity of points
#     interpolated along all sentence-pair segments (at lambda 0.25/0.5/0.75).
#     1 = perfectly convex, lower = holes in the semantic space.
#     Uses all pairs for n<=30, random sample of 500 pairs above.
#   - emb_local_convexity: same as convexity but only on consecutive sentence pairs;
#     measures local semantic continuity rather than global convexity.
embedding_coherence <- function(dt_sent_embeddings) {

  dt <- as.data.table(dt_sent_embeddings)
  dim_cols <- grep("^dim", names(dt), value = TRUE)

  if (length(dim_cols) == 0) {
    stop("No embedding columns (dim1, dim2, ...) found in dt_sent_embeddings.")
  }

  doc_ids <- unique(dt$doc_id)
  results <- vector("list", length(doc_ids))

  na_row <- function(doc) {
    data.table(
      doc_id = doc,
      emb_thematic_dispersion = NA_real_,
      emb_centroid_distance_sd = NA_real_,
      emb_sequential_similarity = NA_real_,
      emb_mean_semantic_gap = NA_real_,
      emb_max_semantic_gap = NA_real_,
      emb_topic_drift = NA_real_,
      emb_mean_novelty = NA_real_,
      emb_n_topics = NA_integer_,
      emb_convexity = NA_real_,
      emb_local_convexity = NA_real_
    )
  }

  for (i in seq_along(doc_ids)) {
    doc <- doc_ids[i]
    mat <- as.matrix(dt[doc_id == doc, ..dim_cols])
    mat <- mat[complete.cases(mat), , drop = FALSE]
    n <- nrow(mat)

    if (n < 2) {
      results[[i]] <- na_row(doc)
      next
    }

    # L2-normalize rows for cosine similarity via dot product
    norms <- sqrt(rowSums(mat^2))
    norms[norms == 0] <- 1
    mat_norm <- mat / norms

    # Centroid and per-sentence cosine distances
    centroid <- colMeans(mat_norm)
    centroid_norm <- centroid / sqrt(sum(centroid^2))
    dist_to_centroid <- 1 - as.numeric(mat_norm %*% centroid_norm)

    # 1) Thematic dispersion: mean cosine distance to centroid
    mean_centroid_distance <- mean(dist_to_centroid)

    # 2) Centroid distance SD
    centroid_dist_sd <- sd(dist_to_centroid)

    # 3) Sequential similarity: mean cosine sim between consecutive sentences
    consecutive_sims <- rowSums(mat_norm[-n, , drop = FALSE] * mat_norm[-1, , drop = FALSE])
    mean_sequential_sim <- mean(consecutive_sims)

    # 4) Semantic gaps: mean and max cosine distance between consecutive sentences
    gaps <- 1 - consecutive_sims
    mean_gap <- mean(gaps)
    max_gap <- max(gaps)

    # 5) Topic drift: mean cosine distance between consecutive blocks of 3 sentences
    block_size <- 3L
    if (n >= 2 * block_size) {
      n_blocks <- n %/% block_size
      block_centroids <- matrix(0, nrow = n_blocks, ncol = ncol(mat_norm))
      for (b in seq_len(n_blocks)) {
        rows <- ((b - 1) * block_size + 1):min(b * block_size, n)
        block_centroids[b, ] <- colMeans(mat_norm[rows, , drop = FALSE])
      }
      bc_norms <- sqrt(rowSums(block_centroids^2))
      bc_norms[bc_norms == 0] <- 1
      bc_norm <- block_centroids / bc_norms
      block_sims <- rowSums(bc_norm[-n_blocks, , drop = FALSE] * bc_norm[-1, , drop = FALSE])
      topic_drift <- mean(1 - block_sims)
    } else {
      topic_drift <- NA_real_
    }

    # 6) Mean novelty: mean cosine distance to running centroid of previous sentences
    # cumulative sum for running centroid, O(n)
    cum_mat <- apply(mat_norm, 2, cumsum)
    novelty_scores <- numeric(n - 1)
    for (j in 2:n) {
      running_centroid <- cum_mat[j - 1, ] / (j - 1)
      rc_norm <- sqrt(sum(running_centroid^2))
      if (rc_norm > 0) {
        novelty_scores[j - 1] <- 1 - sum(mat_norm[j, ] * running_centroid) / rc_norm
      } else {
        novelty_scores[j - 1] <- NA_real_
      }
    }
    mean_novelty <- mean(novelty_scores, na.rm = TRUE)

    # 7) Number of topics: optimal k via silhouette on L2-normalized embeddings
    #    (euclidean distance on unit vectors ~ cosine distance)
    #    Need at least 3 sentences per cluster for stability
    k_max <- min(5L, n %/% 3L)
    if (k_max >= 2) {
      d_mat <- dist(mat_norm)
      best_k <- 1L
      best_sil <- 0  # silhouette of k=1 is 0 by convention
      for (k in 2:k_max) {
        km <- tryCatch(kmeans(mat_norm, centers = k, nstart = 3, iter.max = 20),
                       warning = function(w) kmeans(mat_norm, centers = k, nstart = 3, iter.max = 20))
        sil <- mean(cluster::silhouette(km$cluster, d_mat)[, "sil_width"])
        if (sil > best_sil) { best_sil <- sil; best_k <- k }
      }
      n_topics <- best_k
    } else {
      n_topics <- 1L
    }

    # 8) Conceptual convexity (Gärdenfors): for sentence pairs,
    #    interpolate at lambda = 0.25, 0.5, 0.75 along the segment and
    #    measure cosine similarity to the nearest actual sentence.
    #    Mean NN similarity across all interpolated points: 1 = perfectly
    #    convex, lower = holes in the semantic space.
    #    Uses all pairs when n <= 30 (up to 435 pairs); above that,
    #    randomly samples 500 pairs to keep computation manageable.
    if (n >= 3) {
      n_all_pairs <- n * (n - 1) / 2
      if (n <= 30) {
        # exact: all unique pairs
        pair_idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
      } else {
        # sample 500 random pairs
        n_sample <- min(500L, n_all_pairs)
        idx_a_samp <- sample.int(n, n_sample, replace = TRUE)
        idx_b_samp <- sample.int(n, n_sample, replace = TRUE)
        # ensure a != b
        same <- idx_a_samp == idx_b_samp
        idx_b_samp[same] <- (idx_b_samp[same] %% n) + 1L
        pair_idx <- cbind(idx_a_samp, idx_b_samp)
      }
      idx_a <- pair_idx[, 1]
      idx_b <- pair_idx[, 2]

      lambdas <- c(0.25, 0.5, 0.75)
      nn_sims <- numeric(0)
      for (lam in lambdas) {
        interp <- (1 - lam) * mat_norm[idx_a, , drop = FALSE] +
                       lam  * mat_norm[idx_b, , drop = FALSE]
        i_norms <- sqrt(rowSums(interp^2))
        i_norms[i_norms == 0] <- 1
        interp <- interp / i_norms
        sim_interp <- interp %*% t(mat_norm)
        nn_sims <- c(nn_sims, apply(sim_interp, 1, max))
      }
      convexity <- mean(nn_sims)
    } else {
      convexity <- NA_real_
    }

    # 9) Local convexity: same as above but only on consecutive sentence pairs.
    #    Tests whether the text maintains smooth local semantic transitions.
    if (n >= 3) {
      idx_a_loc <- seq_len(n - 1)
      idx_b_loc <- seq(2L, n)

      lambdas <- c(0.25, 0.5, 0.75)
      nn_sims_loc <- numeric(0)
      for (lam in lambdas) {
        interp <- (1 - lam) * mat_norm[idx_a_loc, , drop = FALSE] +
                       lam  * mat_norm[idx_b_loc, , drop = FALSE]
        i_norms <- sqrt(rowSums(interp^2))
        i_norms[i_norms == 0] <- 1
        interp <- interp / i_norms
        sim_interp <- interp %*% t(mat_norm)
        nn_sims_loc <- c(nn_sims_loc, apply(sim_interp, 1, max))
      }
      local_convexity <- mean(nn_sims_loc)
    } else {
      local_convexity <- NA_real_
    }

    results[[i]] <- data.table(
      doc_id = doc,
      emb_thematic_dispersion = mean_centroid_distance,
      emb_centroid_distance_sd = centroid_dist_sd,
      emb_sequential_similarity = mean_sequential_sim,
      emb_mean_semantic_gap = mean_gap,
      emb_max_semantic_gap = max_gap,
      emb_topic_drift = topic_drift,
      emb_mean_novelty = mean_novelty,
      emb_n_topics = n_topics,
      emb_convexity = convexity,
      emb_local_convexity = local_convexity
    )
  }

  rbindlist(results)
}
