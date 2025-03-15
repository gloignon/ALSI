# This file contains functions for simple calculations of counts and proportions
# Includes counts for part-of-speech tags

library(data.table)

# Main function to calculate feature counts per doc_id
simple_count_features <- function(parsed_data, content_word_upos = c("NOUN", "VERB", "ADJ", "ADV", "PRON")) {
  # Ensure the input is a data.table
  if (!is.data.table(parsed_data)) {
    stop("Input must be a data.table.")
  }
  
  # Group by doc_id and calculate features
  results <- parsed_data[, .(
    
    # Total word count: Count all tokens that are not empty or NA
    word_count = sum(!is.na(compte) & compte == TRUE & !is.na(upos) & !upos == "PUNCT"),
    
    # unique word count
    unique_word_count = length(unique(token[!is.na(compte) & compte == TRUE & !is.na(upos) & !upos == "PUNCT"])),
    
    # Content word count: Count tokens with UPOS in the specified list
    content_word_count = sum(!compte == FALSE & upos %in% content_word_upos),
    
    # unique content word count
    unique_content_word_count = length(unique(token[!compte == FALSE & upos %in% content_word_upos])),
    
    # Sentence count: Count unique sentence IDs
    sentence_count = length(unique(sentence_id)),
    
    # Paragraph count: Count unique paragraph IDs (if available)
    paragraph_count = if ("paragraph_id" %in% names(parsed_data)) {
      length(unique(paragraph_id))
    } else {
      NA_real_
    },
    
    avg_word_length = mean(nchar(token[upos %in% content_word_upos]), na.rm = TRUE),
    
    # number of characters in the text
    char_count = sum(nchar(token[!compte == FALSE & !upos == "PUNCT" & !upos == "PART" & !is.na(token)]), na.rm = TRUE),
    char_count_content = sum(nchar(token[!compte == FALSE & upos %in% content_word_upos]), na.rm = TRUE)
    
  ), by = doc_id]
  
  # add average word and sentence length
  results[, avg_word_length := char_count / word_count]
  results[, avg_sentence_length := word_count / sentence_count]
  
  # average content word length
  results[, avg_content_word_length := char_count / content_word_count]
  
  # UPOS distribution: Add UPOS counts as a separate table grouped by doc_id
  upos_counts <- parsed_data[, .N, by = .(doc_id, upos)]
  setnames(upos_counts, "N", "count")
  
  # add a cnt_ prefix to the upos column values
  # upos_counts[, upos := paste0("cnt_", upos)]
  
  # add a column with proportions of each UPOS
  upos_counts[, prop := count / sum(count), by = doc_id]
  
  # pivot the count and proportion table to wide, fill missing values with 0
  upos_counts <- dcast(upos_counts, doc_id ~ upos, value.var = c("count", "prop"), fill = 0)

  # Add the UPOS distribution as part of the results
  results <- list(
    doc_level_counts = results,
    upos_counts = upos_counts
  )
  
  return(results)
}

# Function to calculate verb tense features
verb_tense_features <- function(parsed_corpus, counts) {
  if (!is.data.table(parsed_corpus)) {
    stop("parsed_corpus must be a data.table.")
  }
  dt <- copy(parsed_corpus)
  
  # make sure counts is also a dt
  if (!is.data.table(counts)) {
    stop("Counts must be a data.table.")
  }
 
  results_n <- dt[, .(
    present_count = sum(feats == "Tense=Pres", na.rm = T),  # number of present tense verbs
    past_count = sum(feats == "Tense=Past", na.rm = T),      # number of past tense verbs
    future_count = sum(feats == "Tense=Fut", na.rm = T),     # number of future tense verbs
    conditional_count = sum(feats == "Tense=Cond", na.rm = T),  # number of conditional tense verbs
    subjunctive_count = sum(feats == "Mood=Sub", na.rm = T),    # number of subjunctive mood verbs
    indicative_count = sum(feats == "Mood=Ind", na.rm = T),    # number of indicative mood verbs
    imperative_count = sum(feats == "Mood=Imp", na.rm = T),    # number of imperative mood verbs
    infinitive_count = sum(feats == "VerbForm=Inf", na.rm = T), # number of infinitive verbs
    # past participles
    past_participle_count = sum(feats == "VerbForm=Part" & feats == "Tense=Past", na.rm = T),
    # present participles
    present_participle_count = sum(feats == "VerbForm=Part" & feats == "Tense=Pres", na.rm = T),
    # passé simple
    past_simple_count = sum(feats == "Tense=Past" & feats == "Mood=Ind" & feats == "VerbForm=Fin", na.rm = T),
    # passé composé
    past_compose_count = sum(feats == "Tense=Past" & feats == "Mood=Ind" & feats == "VerbForm=Part", na.rm = T)
  ), by = doc_id]
  

  # to long
  results_p <- melt(results_n, id.vars = "doc_id", variable.name = "feature", value.name = "count")
  
  # Merge total counts into results_n for consistent alignment
  results_p <- merge(results_p, counts[, .(doc_id, word_count)], by = "doc_id", all.x = TRUE)
  
  # calculate proportions
  results_p[, proportion := count / word_count]
  
  # back to wide
  results_p <- dcast(results_p, doc_id + word_count ~ feature, value.var = "proportion")

  return(list(counts = results_n, proportions = results_p))
  
}
