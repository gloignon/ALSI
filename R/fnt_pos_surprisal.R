# Functions to compute part-of-speech surprisal (also known as POS-tag 
# sequence entropy, see Roark et al. 2007).
# Uses a simple probability model based on the frequency of each POS tag
# in a reference corpus of roughly a million tokens (50% French wikipedia, 
# 50% vikidia, the simple wikipedia equivalent).
#

#pos entropy
# will compute the entropy of the POS tags
# based on the trigram frequency table
pos_entropy_table <- function(trigram_freq) {
  dt <- as.data.table(copy(trigram_freq))
  
  dt_entropy <- dt[, .(
    entropy = -sum(proportion * log2(proportion))
  ), by = .(upos1, upos2)]
  
  return(dt_entropy)
  
}

compute_pos_surprisal <- function(dt,
                                  trigram_freq,
                                  backoff_coef = 0.4) {
  
  # Ensure data is ordered by sentence and position
  setorder(dt, doc_id, sentence_id, position)
  
  # ---- Ensure Required Columns Exist
  if (!("upos" %in% names(dt)))
    stop("Error: 'upos' column missing in corpus data")
  if (!("sentence_id" %in% names(dt)))
    stop("Error: 'sentence_id' column missing in corpus data")
  
  dt[, surprisal := NA_real_]
  
  # ---- BIGRAM & TRIGRAM SURPRISAL
  dt[, `:=`(
    upos1 = shift(as.character(upos), n = 2, type = "lag"),
    upos2 = shift(as.character(upos), n = 1, type = "lag")
  ), by = sentence_id]
  
  # Ensure no NA values for context at sentence boundaries
  dt[is.na(upos1), upos1 := "<START>"]
  dt[is.na(upos2), upos2 := "<START>"]
  
  # ---- Compute bigram frequencies from trigram table
  bigram_freq <- trigram_freq[, .(freq = sum(freq)), by = .(upos1, upos2)]
  bigram_freq[, proportion := freq / sum(freq)]
  
  # ---- Merge with trigram probabilities
  dt <- merge(
    dt,
    trigram_freq,
    by.x = c("upos1", "upos2", "upos"),
    by.y = c("upos1", "upos2", "upos3"),
    all.x = TRUE
  )
  
  # Use trigram if available
  dt[, surprisal := fifelse(is.na(proportion), surprisal, proportion)][, proportion := NULL]
  
  # ---- Backoff to bigram if trigram is missing
  missing_trigrams <- is.na(dt$surprisal)
  bigram_data <- merge(
    dt[missing_trigrams, .(upos1, upos2)],
    bigram_freq,
    by = c("upos1", "upos2"),
    all.x = TRUE
  )
  
  dt[missing_trigrams, surprisal := ifelse(
    !is.na(bigram_data$proportion),
    backoff_coef * bigram_data$proportion,
    NA_real_
  )]
  
  # ---- Backoff to unigram if bigram is missing
  unigram_freq <- bigram_freq[, .(freq = sum(freq)), by = upos1]
  unigram_freq[, proportion := freq / sum(freq)]
  
  missing_bigrams <- is.na(dt$surprisal)
  unigram_data <- merge(
    dt[missing_bigrams, .(upos)],
    unigram_freq,
    by.x = "upos",
    by.y = "upos1",
    all.x = TRUE
  )
  
  dt[missing_bigrams, surprisal := ifelse(
    !is.na(unigram_data$proportion),
    backoff_coef * unigram_data$proportion,
    1e-6
  )]
  
  # ---- Convert to log surprisal
  dt[, log_surprisal := -log2(surprisal)]
  
  # ---- Clean up intermediate columns
  dt[, c("surprisal") := NULL]
  
  
  # Optionally clean up if you want
  # dt[is.na(entropy), entropy := 0]  # Fill NA if you want a default value
  
  # Final clean-up of upos1 and upos2 (optional)
  # dt[, c("upos1", "upos2") := NULL]
  
  return(dt)
}


# Wrapper function to apply POS surprisal to a corpus
pos_surprisal <- function(dt_corpus, trigram_freq = NA, backoff_coef = 0.4)  {
  
  # make a copy to avoid modifying the original data
  dt_corpus <- setDT(copy(dt_corpus))
  
  # Load frequency table
  if (missing(trigram_freq)) {
    trigram_freq <- readRDS("models/trigram_freq_1860.Rds")  
  }
  
  setkey(trigram_freq, upos1, upos2, upos3)
  
  # Ensure corpus has a "position" column
  dt_corpus[, position := vrai_token_id]
  
  # ---- Compute token-level surprisal and entropy
  dt_corpus <- compute_pos_surprisal(
    dt_corpus, 
    trigram_freq = trigram_freq,
    backoff_coef = backoff_coef
  )
  
  # Add POS Entropy by 2-gram Context
  dt_pos_entropy <- pos_entropy_table(trigram_freq)  # call your entropy function
  dt_corpus <- merge(
    dt_corpus,
    dt_pos_entropy,
    by = c("upos1", "upos2"),
    all.x = TRUE
  )
  
  # Add entropy reduction (current token entropy minus previous token entropy)
  dt_corpus[, entropy_reduction := entropy - shift(entropy, type = "lag"), by = c("doc_id", "sentence_id")]
  
  # ---- Keep necessary columns
  dt_pos_surprisal <- dt_corpus[, .(
    doc_id,
    sentence_id,
    token_id,
    vrai_token_id,
    upos,
    pos_surprisal = log_surprisal,
    pos_entropy = entropy,
    pos_entropy_reduction = entropy_reduction
  )]
  
  
  # ---- Summarise by doc_id
  df_doc <- dt_pos_surprisal %>%
    filter(!is.na(upos) & !upos %in% c("X", "INTJ", "NUM", "SYM", "PUNCT")) %>%
    group_by(doc_id) %>%
    summarise(
      mean_pos_surprisal = mean(pos_surprisal, na.rm = TRUE),
      sd_pos_surprisal = sd(pos_surprisal, na.rm = TRUE),
      mean_pos_entropy = mean(pos_entropy, na.rm = TRUE),
      sd_pos_entropy = sd(pos_entropy, na.rm = TRUE),
      mean_pos_entropy_reduction = mean(pos_entropy_reduction, na.rm = TRUE),
      sd_pos_entropy_reduction = sd(pos_entropy_reduction, na.rm = TRUE)
    )
  
  # ---- Summarise by doc_id and sentence_id
  df_doc_sent <- dt_pos_surprisal %>%
    filter(!is.na(upos) & !upos %in% c("X", "INTJ", "NUM", "SYM", "PUNCT")) %>%
    group_by(doc_id, sentence_id) %>%
    summarise(
      mean_pos_surprisal = mean(pos_surprisal, na.rm = TRUE),
      sd_pos_surprisal = sd(pos_surprisal, na.rm = TRUE),
      mean_pos_entropy = mean(pos_entropy, na.rm = TRUE),
      sd_pos_entropy = sd(pos_entropy, na.rm = TRUE),
      mean_pos_entropy_reduction = mean(pos_entropy_reduction, na.rm = TRUE),
      sd_pos_entropy_reduction = sd(pos_entropy_reduction, na.rm = TRUE)
    )
  
  return(list(
    doc_surprisal = df_doc,
    sent_surprisal = df_doc_sent,
    token_surprisal = dt_pos_surprisal
  ))
}


