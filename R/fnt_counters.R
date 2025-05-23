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
# Function to calculate verb tense features
verb_tense_features <- function(parsed_corpus, counts) {
  if (!is.data.table(parsed_corpus)) {
    stop("parsed_corpus must be a data.table.")
  }
  dt <- copy(parsed_corpus)
  
  if (!is.data.table(counts)) {
    stop("Counts must be a data.table.")
  }
  
  results_n <- dt[, .(
    present_count = sum(grepl("Tense=Pres", feats), na.rm = TRUE),
    past_count = sum(grepl("Tense=Past", feats), na.rm = TRUE),
    future_count = sum(grepl("Tense=Fut", feats), na.rm = TRUE),
    conditional_count = sum(grepl("Mood=Cnd", feats), na.rm = TRUE),
    subjunctive_count = sum(grepl("Mood=Sub", feats), na.rm = TRUE),
    indicative_count = sum(grepl("Mood=Ind", feats), na.rm = TRUE),
    imperative_count = sum(grepl("Mood=Imp", feats), na.rm = TRUE),
    infinitive_count = sum(grepl("VerbForm=Inf", feats), na.rm = TRUE),
    past_participle_count = sum(grepl("VerbForm=Part", feats) & grepl("Tense=Past", feats), na.rm = TRUE),
    present_participle_count = sum(grepl("VerbForm=Part", feats) & grepl("Tense=Pres", feats), na.rm = TRUE),
    past_simple_count = sum(grepl("Tense=Past", feats) & grepl("Mood=Ind", feats) & grepl("VerbForm=Fin", feats), na.rm = TRUE)
  ), by = doc_id]
  
  # Long format
  results_p <- melt(results_n, id.vars = "doc_id", variable.name = "feature", value.name = "count")
  
  # Merge word counts
  results_p <- merge(results_p, counts[, .(doc_id, word_count)], by = "doc_id", all.x = TRUE)
  
  # Compute proportions
  results_p[, proportion := count / word_count]
  
  # Wide format
  results_p <- dcast(results_p, doc_id + word_count ~ feature, value.var = "proportion")
  
  # Rename columns from *_count → *_prop
  setnames(
    results_p,
    old = setdiff(names(results_p), c("doc_id", "word_count")),
    new = gsub("_count$", "_prop", setdiff(names(results_p), c("doc_id", "word_count")))
  )
  
  # we no longer need the word_count column in results_p
  results_p[, word_count := NULL]
  
  return(list(counts = results_n, proportions = results_p))
}