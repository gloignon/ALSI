# Lexical cohesion
library(data.table)

#' Compute document-level token match proportions
#'
#' For each token in each sentence, computes its normalised frequency in the
#' rest of the document (excluding the current sentence). Adds helper columns
#' directly to \code{dt} by reference.
#'
#' @param dt A \code{data.table} with columns \code{doc_id},
#'   \code{sentence_id}, and \code{token}.
#' @returns The input \code{data.table} augmented with columns
#'   \code{normalized_match}, \code{other_tokens_doc}, etc.
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

#' Compute proportion of values shared with previous sentences
#'
#' For each token, computes the proportion of the \code{n_prev} preceding
#' sentences that contain the same value (token or lemma).
#'
#' @param dt A \code{data.table} with \code{doc_id}, \code{sentence_id}, and
#'   the column named by \code{value_col}.
#' @param value_col Column to match on (default \code{"token"}).
#' @param n_prev Number of preceding sentences to consider (default 1).
#' @param new_col Name for the result column; auto-generated if NULL.
#' @returns The input \code{data.table} with the new proportion column added.
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

#' Compute Coh-Metrix style argument overlap
#'
#' For each pair of adjacent sentences, checks whether any noun or pronoun
#' lemma appears in both. Returns a binary overlap flag per pair.
#'
#' @param dt A \code{data.table} with \code{doc_id}, \code{sentence_id},
#'   \code{upos}, and \code{lemma}.
#' @returns A \code{data.table} with columns \code{doc_id},
#'   \code{sentence_id}, and \code{arg_overlap} (0 or 1).
compute_argument_overlap <- function(dt) {
  # Coh-Metrix style: for each adjacent sentence pair, does any noun/pronoun
  # lemma appear in both? Returns one row per pair with a binary flag.
  dt_np <- dt[upos %in% c("NOUN", "PRON")]
  # unique lemmas per sentence
  sent_lemmas <- dt_np[, .(lemmas = list(unique(lemma))), by = .(doc_id, sentence_id)]
  setorder(sent_lemmas, doc_id, sentence_id)

  # pair with next sentence
  next_lemmas <- copy(sent_lemmas)
  setnames(next_lemmas, "lemmas", "lemmas_next")
  next_lemmas[, sentence_id := sentence_id - 1L]
  pairs <- next_lemmas[sent_lemmas, on = .(doc_id, sentence_id), nomatch = 0L]

  if (nrow(pairs) == 0L) {
    return(data.table(doc_id = character(), sentence_id = integer(), arg_overlap = integer()))
  }
  pairs[, arg_overlap := as.integer(mapply(function(a, b) length(intersect(a, b)) > 0L,
                                           lemmas, lemmas_next))]
  pairs[, .(doc_id, sentence_id, arg_overlap)]
}

#' Compute similarity between two token vectors
#'
#' Builds binary presence/absence vectors from two token sets and computes
#' cosine or Jaccard similarity.
#'
#' @param vec1 Character vector of tokens from the first set.
#' @param vec2 Character vector of tokens from the second set.
#' @param method Either \code{"cosine"} or \code{"jaccard"}.
#' @returns A numeric scalar: the similarity score.
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

#' Compute adjacent-sentence similarity
#'
#' Pairs each sentence with its successor and computes token-level similarity
#' (cosine or Jaccard) between them.
#'
#' @param dt A \code{data.table} with \code{doc_id}, \code{sentence_id},
#'   \code{token_id}, and \code{token}.
#' @param method Either \code{"cosine"} or \code{"jaccard"}.
#' @returns A \code{data.table} with \code{doc_id}, \code{sentence_id}, and
#'   \code{similarity}.
compute_sentence_similarity <- function(dt, method = "cosine") {
  # Ensure data.table is ordered by document and sentence position
  setorder(dt, doc_id, sentence_id, token_id)
  
  # Group tokens by sentence as lists (avoids paste/split roundtrip that breaks on spaces)
  sentence_tokens <- dt[, .(tokens = list(token)), by = .(doc_id, sentence_id)]

  # Self-join to pair each sentence with its next (GForce doesn't support shift on list columns)
  setorder(sentence_tokens, doc_id, sentence_id)
  next_tokens <- copy(sentence_tokens)
  setnames(next_tokens, "tokens", "tokens_next")
  next_tokens[, sentence_id := sentence_id - 1L]
  sentence_tokens <- next_tokens[sentence_tokens, on = .(doc_id, sentence_id), nomatch = 0L]
  
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

#' Compute document-level lexical cohesion features
#'
#' Aggregates multiple cohesion measures per document: token/lemma overlap
#' with previous sentences (at multiple context windows), document-level
#' overlap, cosine similarity, and Coh-Metrix argument overlap. Computes
#' both all-token and content-word-only variants.
#'
#' @param dt_corpus A parsed \code{data.table} with columns \code{doc_id},
#'   \code{sentence_id}, \code{token_id}, \code{token}, \code{lemma},
#'   \code{upos}, and \code{compte}.
#' @param n_sent_context Integer vector of context window sizes for
#'   previous-sentence overlap (default \code{c(1, 5)}).
#' @returns A \code{data.table} with one row per document and columns for
#'   each cohesion feature (overlap proportions, cosine similarities,
#'   argument overlap, global-local gap).
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
  
  # Argument overlap (Coh-Metrix style): binary noun/pronoun overlap per adjacent pair
  dt_arg <- compute_argument_overlap(dt)
  dt_result_arg <- dt_arg[, .(arg_overlap = mean(arg_overlap, na.rm = TRUE)), by = doc_id]

  # Also on content words only
  dt_arg_content <- compute_argument_overlap(dt_content)
  dt_result_arg_content <- dt_arg_content[, .(arg_overlap_content = mean(arg_overlap, na.rm = TRUE)), by = doc_id]

  # Merge the results
  dt_result <- merge(dt_result, dt_result_content, by = "doc_id", all = TRUE)
  dt_result <- merge(dt_result, dt_result_cosine, by = "doc_id", all = TRUE)
  dt_result <- merge(dt_result, dt_result_arg, by = "doc_id", all = TRUE)
  dt_result <- merge(dt_result, dt_result_arg_content, by = "doc_id", all = TRUE)

  # Global-local cohesion gap: difference between document-level and adjacent-sentence overlap.
  # Positive = more global than local cohesion (topic coherence without local repetition).
  if ("token_sent_overlap_prev1" %in% names(dt_result)) {
    dt_result[, global_local_gap := token_doc_overlap - token_sent_overlap_prev1]
  }
  if ("content_sent_overlap_prev1" %in% names(dt_result)) {
    dt_result[, content_global_local_gap := content_doc_overlap - content_sent_overlap_prev1]
  }

  return(dt_result)
}
