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

  # Order by document, sentence, and token position
  setorder(dt, doc_id, sentence_id, token_id)
  
  # Self-join to find matches in a different sentence within the same document
  dt[, match_in_doc := FALSE]
  
  dt[dt, on = .(doc_id, token), 
       match_in_doc := sentence_id != i.sentence_id]
  
  return(dt)
}


# Aggregate the new features by doc_id
simple_lexical_cohesion <- function(dt_corpus) {

  dt <- copy(dt_corpus)
  dt <- dt[compte == TRUE]
  
  # get the next sentence overlap results
  dt <- compute_next_sentence_match(dt)
  
  # add the document match results
  dt <- compute_document_match(dt)
  
  # Now only content words, their lemmatized form
  dt_content <- dt[upos %in% c("NOUN", "VERB", "ADJ", "ADV")]
  dt_content <- compute_next_sentence_match(dt_content)
  dt_content <- compute_document_match(dt_content)
  
  # Aggregate by document
  dt_result <- dt[, .(
    token_sent_overlap = sum(match_next_sentence) / .N,
    token_doc_overlap = sum(match_in_doc) / .N
  ), by = doc_id]
  dt_result_content <-  dt_content[, .(
    content_sent_overlap = sum(match_next_sentence) / .N,
    content_doc_overlap = sum(match_in_doc) / .N
  ), by = doc_id]
  
  # Merge the results
  dt_result <- merge(dt_result, dt_result_content, by = "doc_id", all = TRUE)
  
  return(dt_result)
}

# Example use
# t <- features$parsed_corpus[doc_id %in% c("g01_pri_fs1", "g01_pri_ol01")]
# result <- compute_next_sentence_match(t)
# result <- compute_document_match(t)
# result <- simple_token_overlap(t)
