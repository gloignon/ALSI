# Functions to compute part-of-speech surprisal (also known as POS-tag 
# sequence entropy, see Roark et al. 2007).
# Uses a simple probability model based on the frequency of each POS tag
# in a reference corpus of roughly a million tokens (50% French wikipedia, 
# 50% vikidia, the simple wikipedia equivalent).
#


# Function to compute POS surprisal for a given corpus
# back_off_coef: Backoff coefficient for bigram probabilities
# i.e. for unseen trigrams it will use the bigram probability penalized by this coefficient
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
  
  # Apply bigram backoff coefficient
  dt[missing_trigrams, surprisal := ifelse(
    !is.na(bigram_data$proportion),
    backoff_coef * bigram_data$proportion,
    NA_real_ # Keep NA for now to trigger the unigram backoff step
  )]
  
  # ---- Backoff to unigram if bigram is missing
  # Create a unigram table from the bigram table 
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
  
  # Apply unigram backoff coefficient
  dt[missing_bigrams, surprisal := ifelse(
    !is.na(unigram_data$proportion),
    backoff_coef * unigram_data$proportion,
    1e-6                                           # Final fallback probability
  )]
  
  # ---- Convert to log surprisal at the end
  dt[, log_surprisal := -log2(surprisal)]
  
  # ---- Clean up intermediate columns
  dt[, c("surprisal", "upos1", "upos2") := NULL]
  
  return(dt)
}

# Wrapper function to apply POS surprisal to a corpus
pos_surprisal <- function(dt_corpus, trigram_freq = NA, backoff_coef = 0.4)  {
  
  # ---- APPLY FUNCTION TO CORPUS ----
  # Load frequency table
  # If the parameter is missing, load the default
  if (missing(trigram_freq)) {
    trigram_freq <- readRDS("models/trigram_freq_500.Rds")  # Columns: upos1, upos2, upos3, freq, proportion
  }

  setkey(trigram_freq, upos1, upos2, upos3)

  # Ensure corpus has a "position" column
  dt_corpus[, position := vrai_token_id]
  
  k_pos <- length(unique(trigram_freq$upos1))
  dt_corpus <- compute_pos_surprisal(dt_corpus, trigram_freq = trigram_freq, 
                                     backoff_coef = .7)
  
  
  
  dt_pos_surprisal <- dt_corpus %>% select(doc_id,
                                           sentence_id,
                                           token_id,
                                           vrai_token_id,
                                           upos,
                                           pos_surprisal = log_surprisal)
  
  # By doc_id
  df_doc <- dt_pos_surprisal %>%
    filter(!is.na(upos) & !upos %in% c("X", "INTJ", "NUM", "SYM", "PUNCT")) %>%
    group_by(doc_id) %>%
    summarise(mean_pos_surprisal = mean(pos_surprisal, na.rm = TRUE),
              sd_pos_surprisal = sd(pos_surprisal, na.rm = TRUE)
    )
  
  # By doc_id and sentence_id
  df_doc_sent <- dt_pos_surprisal %>%
    filter(!is.na(upos) & !upos %in% c("X", "INTJ", "NUM", "SYM", "PUNCT")) %>%
    group_by(doc_id, sentence_id) %>%
    summarise(mean_pos_surprisal = mean(pos_surprisal, na.rm = TRUE),
              sd_pos_surprisal = sd(pos_surprisal, na.rm = TRUE)
    )
  
  return(list(
    doc_surprisal = df_doc,
    sent_surprisal = df_doc_sent,
    token_surprisal = dt_pos_surprisal
  ))
  
}
