# Lexical cohesion
library(data.table)

compute_next_sentence_match <- function(dt) {

  # Order by document, sentence, and token position
  setorder(dt, doc_id, sentence_id, token_id)
  
  # Self-join to match tokens in next sentence
  dt <- dt[
    dt[, .(next_sentence_id = sentence_id + 1, token, doc_id)],
    on = c("doc_id", "sentence_id" = "next_sentence_id", "token"),
    match_next_sentence := TRUE
  ]
  
  # Replace NA with FALSE for unmatched cases
  dt[is.na(match_next_sentence), match_next_sentence := FALSE]
  
  return(dt)
}

compute_document_match <- function(dt) {
  # counts at doc and sentence granularity
  dt[, total_tokens_doc   := .N, by = .(doc_id)]
  dt[, total_tokens_sent  := .N, by = .(doc_id, sentence_id)]
  dt[, token_count_doc    := .N, by = .(doc_id, token)]
  dt[, token_count_sent   := .N, by = .(doc_id, sentence_id, token)]
  
  # counts excluding the current sentence
  dt[, other_tokens_doc   := pmax(total_tokens_doc - total_tokens_sent, 0L)]
  dt[, other_token_count  := pmax(token_count_doc - token_count_sent, 0L)]
  
  # normalized proportion of this token in the rest of the doc
  dt[, normalized_match := fifelse(
    other_tokens_doc > 0L,
    other_token_count / other_tokens_doc,
    NA_real_
  )]
  
  return(dt)
}

compute_previous_sentence_match <- function(dt, value_col = "token", n_prev = 1, new_col = NULL) {
  if (!value_col %in% names(dt)) {
    stop(paste0("compute_previous_sentence_match | column not found: ", value_col))
  }
  if (!all(c("doc_id", "sentence_id") %in% names(dt))) {
    stop("compute_previous_sentence_match | dt must contain columns: doc_id, sentence_id")
  }
  if (is.null(new_col)) {
    new_col <- paste0(value_col, "_prev", n_prev, "_prop")
  }
  
  order_cols <- c("doc_id", "sentence_id")
  if ("token_id" %in% names(dt)) {
    order_cols <- c(order_cols, "token_id")
  }
  setorderv(dt, order_cols)
  n_prev <- max(1L, as.integer(n_prev))
  
  # Unique values per sentence
  sentence_values <- unique(dt[, .(doc_id, sentence_id, value = get(value_col))])
  
  # Build previous sentence map for each sentence/value
  prev_map <- sentence_values[
    ,
    .(sentence_id_prev = (sentence_id - n_prev):(sentence_id - 1L)),
    by = .(doc_id, sentence_id, value)
  ]
  prev_map <- prev_map[sentence_id_prev >= 1]
  
  # Keep only previous sentences that contain the same value
  prev_hits <- sentence_values[
    prev_map,
    on = .(doc_id, sentence_id = sentence_id_prev, value),
    nomatch = 0L,
    .(doc_id, sentence_id = i.sentence_id, value, sentence_id_prev)
  ]
  
  # Count matches per sentence/value
  matches <- prev_hits[
    ,
    .(n_prev_match = uniqueN(sentence_id_prev)),
    by = .(doc_id, sentence_id, value)
  ]
  
  # How many previous sentences are available for this sentence
  prev_counts <- sentence_values[
    ,
    .(n_prev_available = pmin(n_prev, sentence_id - 1L)),
    by = .(doc_id, sentence_id)
  ]
  
  # Combine and compute proportions
  proportions <- merge(sentence_values, prev_counts, by = c("doc_id", "sentence_id"), all.x = TRUE)
  proportions <- merge(proportions, matches, by = c("doc_id", "sentence_id", "value"), all.x = TRUE)
  proportions[is.na(n_prev_match), n_prev_match := 0L]
  proportions[, (new_col) := fifelse(n_prev_available > 0, n_prev_match / n_prev_available, 0)]
  proportions <- proportions[, c("doc_id", "sentence_id", "value", new_col), with = FALSE]
  
  # TODO: Consider computing multiple n_prev windows in one pass to avoid repeated work.
  # Join back to original table
  dt[, value := get(value_col)]
  dt <- proportions[dt, on = .(doc_id, sentence_id, value)]
  dt[, value := NULL]
  
  return(dt)
}

compute_similarity <- function(vec1, vec2, method = "cosine") {
  # Create binary presence/absence vectors
  unique_tokens <- unique(c(vec1, vec2))
  binary_vec1 <- as.integer(unique_tokens %in% vec1)
  binary_vec2 <- as.integer(unique_tokens %in% vec2)
  
  if (method == "cosine") {
    # Cosine similarity
    similarity <- sum(binary_vec1 * binary_vec2) /
      (sqrt(sum(binary_vec1^2)) * sqrt(sum(binary_vec2^2)))
  } else if (method == "jaccard") {
    # Jaccard similarity
    intersection <- sum(binary_vec1 * binary_vec2)
    union <- sum(binary_vec1 | binary_vec2)
    similarity <- intersection / union
  } else {
    stop("Invalid method. Use 'cosine' or 'jaccard'.")
  }
  
  return(similarity)
}

compute_sentence_similarity <- function(dt, method = "cosine") {
  # Ensure data.table is ordered by document and sentence position
  setorder(dt, doc_id, sentence_id, token_id)
  
  # Group tokens by sentence (collapse tokens into a single string)
  sentence_tokens <- dt[, .(tokens = paste(token, collapse = " ")), by = .(doc_id, sentence_id)]
  
  # Use shift on the collapsed token strings
  sentence_tokens[, tokens_next := shift(tokens, type = "lead"), by = doc_id]
  
  # Drop last row of each document (since it won't have a next pair)
  sentence_tokens <- sentence_tokens[!is.na(tokens_next)]
  
  # Split strings back into token vectors
  sentence_tokens[, tokens := strsplit(tokens, " ")]
  sentence_tokens[, tokens_next := strsplit(tokens_next, " ")]
  
  # Compute similarity for each pair of adjacent sentences
  sentence_tokens[, similarity := mapply(
    compute_similarity,
    tokens,
    tokens_next,
    MoreArgs = list(method = method)
  )]
  
  # Return as a clean data table
  result <- sentence_tokens[, .(doc_id, sentence_id, similarity)]
  
  return(result)
}

# Aggregate the new features by doc_id
simple_lexical_cohesion <- function(dt_corpus, n_sent_context = c(1,5)) {

  dt <- copy(dt_corpus)
  dt <- dt[compte == TRUE]
  
  n_sent_context <- unique(as.integer(n_sent_context))
  n_sent_context <- n_sent_context[!is.na(n_sent_context) & n_sent_context >= 1L]
  if (length(n_sent_context) == 0) {
    stop("simple_lexical_cohesion | n_sent_context must contain integers >= 1")
  }
  
  # previous sentence overlap (token + lemma) for each context size
  for (n_prev in n_sent_context) {
    dt <- compute_previous_sentence_match(dt,
      value_col = "token",
      n_prev = n_prev,
      new_col = paste0("token_prev", n_prev, "_prop")
    )
    dt <- compute_previous_sentence_match(dt,
      value_col = "lemma",
      n_prev = n_prev,
      new_col = paste0("lemma_prev", n_prev, "_prop")
    )
  }
  
  # add the document match results
  dt <- compute_document_match(dt)
  
  # Now only content words, their lemmatized form
  dt_content <- dt[upos %in% c("NOUN", "VERB", "ADJ", "ADV")]
  for (n_prev in n_sent_context) {
    dt_content <- compute_previous_sentence_match(dt_content,
      value_col = "token",
      n_prev = n_prev,
      new_col = paste0("token_prev", n_prev, "_prop")
    )
    dt_content <- compute_previous_sentence_match(dt_content,
      value_col = "lemma",
      n_prev = n_prev,
      new_col = paste0("lemma_prev", n_prev, "_prop")
    )
  }
  dt_content <- compute_document_match(dt_content)
  
  # Cosine similarity
  dt_cosine <- compute_sentence_similarity(dt, method = "cosine")
  dt_cosine_content <- compute_sentence_similarity(dt_content, method = "cosine")
  # rename similarity column to avoid duplicate names
  setnames(dt_cosine_content, "similarity", "similarity_content")
  # combine dt_cosine with dt_cosine_content
  dt_cosine <- merge(dt_cosine, dt_cosine_content, by = c("doc_id", "sentence_id"), all = TRUE)
  
  # Aggregate by document
  token_cols <- paste0("token_prev", n_sent_context, "_prop")
  lemma_cols <- paste0("lemma_prev", n_sent_context, "_prop")
  token_out_cols <- paste0("token_sent_overlap_prev", n_sent_context)
  lemma_out_cols <- paste0("lemma_sent_overlap_prev", n_sent_context)
  
  dt_result <- dt[, c(
    setNames(lapply(token_cols, function(col) mean(get(col), na.rm = TRUE)), token_out_cols),
    setNames(lapply(lemma_cols, function(col) mean(get(col), na.rm = TRUE)), lemma_out_cols),
    list(token_doc_overlap = mean(normalized_match, na.rm = TRUE))
  ), by = doc_id]
  
  content_token_out_cols <- paste0("content_sent_overlap_prev", n_sent_context)
  content_lemma_out_cols <- paste0("content_lemma_sent_overlap_prev", n_sent_context)
  
  dt_result_content <- dt_content[, c(
    setNames(lapply(token_cols, function(col) mean(get(col), na.rm = TRUE)), content_token_out_cols),
    setNames(lapply(lemma_cols, function(col) mean(get(col), na.rm = TRUE)), content_lemma_out_cols),
    list(content_doc_overlap = mean(normalized_match, na.rm = TRUE))
  ), by = doc_id]
  dt_result_cosine <- dt_cosine[, .(
    cosine_sent = mean(similarity, na.rm = T),
    cosine_content = mean(similarity_content, na.rm = T)
  ), by = doc_id]
  
  # Merge the results
  dt_result <- merge(dt_result, dt_result_content, by = "doc_id", all = TRUE)
  #   # add cosine stuff too
  dt_result <- merge(dt_result, dt_result_cosine, by = "doc_id", all = TRUE)
  # 
  return(dt_result)
}

# Example use
# t <- features$parsed_corpus[doc_id %in% c("g01_pri_fs1", "g01_pri_ol01")]
# # result <- compute_next_sentence_match(t)
# #result <- compute_document_match(t)
# compute_similarity(vec1 = t[sentence_id == 1, token],
#                   vec2 = t[sentence_id == 2, token],
#                   method = "cosine")
# compute_sentence_similarity(t, method = "cosine")
# result <- simple_lexical_cohesion(t)
# result
