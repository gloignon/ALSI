# This script contains functions for a "fuzzy" lexical frequency merge (using a database of
# your choice) that will first try a perfect match and then fall back to a less ideal
# matching when the data is not available.
# Also contains functions for classic lexical diversity indexes and moving-average TTR.
#
library(data.table, quietly = TRUE)

#' Fuzzy Match Between Corpus and Lexical Database
#'
#' @param dt_corpus A data.table containing the corpus to be annotated.
#' @param dt_lexical A data.table containing the lexical database.
#' @param freq_col A string specifying the name of the frequency column.
#' @param prefix A string specifying the prefix to add to lexical database columns.
#' @param token_col Name of the token column in the corpus and database.
#' @param lemma_col Name of the lemma column in the corpus.
#' @param upos_col Name of the part-of-speech column in the corpus and database.
#' @param id_col Name of the unique identifier column for tokens in the corpus.
#' @param verbose Logical; if TRUE, print progress information. Defaults to FALSE.
#'
#' @return A data.table containing the merged corpus and lexical database.
fuzzy_match_lexical_db <- function(dt_corpus, dt_lexical,
                                   freq_col = "freq_u",
                                   prefix = "lexicaldb",
                                   token_col = "token",
                                   lemma_col = "lemma",
                                   upos_col = "upos",
                                   id_col = "vrai_token_id",
                                   verbose = FALSE) {

  
  # Helper function for deduplication
  deduplicate_matches <- function(data, id_col, sorting_col) {
    data <- data[order(get(id_col), -get(sorting_col))]
    data <- data[!duplicated(get(id_col))]
    return(data)
  }
  
  # Copy data for safety
  dt_corpus <- setDT(copy(dt_corpus))
  dt_lexical <- setDT(copy(dt_lexical))
  
  # Validation checks
  if (!(token_col %in% colnames(dt_lexical))) stop("The token column does not exist in the lexical db.")
  if (!(freq_col %in% colnames(dt_lexical))) stop("The frequency column does not exist in the lexical db.")
  if (!all(c(token_col, lemma_col, upos_col, id_col) %in% colnames(dt_corpus))) {
    stop("The corpus db does not have all required columns.")
  }
  
  # Sorting column for prioritization
  dt_lexical[, .sorting_col := get(freq_col)]
  
  # Initialize result
  dt_merged <- NULL
  
  # Exact match (token + upos)
  if (upos_col %in% colnames(dt_lexical)) {
    dt_merged_exact <- merge(dt_corpus, dt_lexical,
                             by.x = c(token_col, upos_col),
                             by.y = c(token_col, upos_col),
                             all.x = FALSE, sort = FALSE)
    dt_merged_exact <- deduplicate_matches(dt_merged_exact, id_col, ".sorting_col")
    if (verbose) cat("Exact matches: ", nrow(dt_merged_exact), "\n")
  } else {
    dt_merged_exact <- NULL
  }
  
  # Token-only match
  dt_merged_token <- merge(
    dt_corpus[!get(id_col) %in% dt_merged_exact[[id_col]]],
    dt_lexical[, .SD, .SDcols = setdiff(names(dt_lexical), c(upos_col, lemma_col))],
    by.x = token_col, by.y = token_col, all.x = FALSE, sort = FALSE
  )
  dt_merged_token <- deduplicate_matches(dt_merged_token, id_col, ".sorting_col")
  if (verbose) cat("Token-only matches: ", nrow(dt_merged_token), "\n")
  
  # Lemma-to-token match
  dt_merged_crossed <- merge(
    dt_corpus[!get(id_col) %in% c(dt_merged_exact[[id_col]], dt_merged_token[[id_col]])],
    dt_lexical[, .SD, .SDcols = setdiff(names(dt_lexical), c(upos_col, lemma_col))],
    by.x = lemma_col, by.y = token_col, all.x = FALSE, sort = FALSE
  )
  dt_merged_crossed <- deduplicate_matches(dt_merged_crossed, id_col, ".sorting_col")
  if (verbose) cat("Lemma-to-token matches: ", nrow(dt_merged_crossed), "\n")
  
  # Orphans (unmatched tokens)
  dt_orphans <- dt_corpus[!get(id_col) %in% c(dt_merged_exact[[id_col]], dt_merged_token[[id_col]], dt_merged_crossed[[id_col]])]
  if (verbose) cat("Unmatched tokens: ", nrow(dt_orphans), "\n")
  
  # Combine results
  dt_merged <- rbindlist(list(dt_merged_exact, dt_merged_token, dt_merged_crossed, dt_orphans), fill = TRUE)
  dt_merged <- dt_merged[order(get(id_col))]
  
  # Remove sorting column
  dt_merged[, .sorting_col := NULL]
  
  # Rename new columns
  cols_to_rename <- setdiff(names(dt_lexical), c(token_col, upos_col, ".sorting_col"))
  setnames(dt_merged, cols_to_rename, paste0(prefix, "_", cols_to_rename))
  
  return(dt_merged)
}

# Lexical diversity -----
# TTR function can accept a different vector for types,
# adding some flexibility, e.g. for using lemmas as types
calculate_TTR <- function(tokens, types) {
  n_token <- length(tokens)
  n_type <- length(unique(types))
  ttr <- n_type / n_token
  return(ttr)
}

calculate_moving_TTR <- function(tokens, window_size = 50) {
  library(zoo)
  
  # if text length is > window_size, change window_size to text length
  if (length(tokens) < window_size) {
    window_size <- length(tokens)
  }
  
  # Compute moving TTR using rollapply
  ttr_values <- zoo::rollapply(
    tokens, 
    width = window_size, 
    FUN = function(x) length(unique(x)) / length(x), 
    fill = NA, 
    align = "right"
  )
  
  # Remove NA values (incomplete windows at the beginning)
  ttr_values <- na.omit(ttr_values)
  
  # Average out all TTR values across segments
  mean_ttr <- mean(ttr_values)
  
  return(mean_ttr)
}


# will calculate maas index from provided vectors of tokens and types 
calculate_maas <- function(tokens, types) {
  n_token <- length(tokens)
  n_type <- length(unique(types))
  maas <- (log(n_token) - log(n_type)) / log(n_token^2)
  return(maas)
}

# Function to compute D-measure from a vector of tokens
D_measure_from_tokens <- function(tokens) {
  N <- length(tokens)  # Total number of tokens
  freq_table <- table(tokens)  # Frequency of each type
  V <- length(freq_table)  # Number of unique types
  
  # Create a table for fv(i, N): number of types occurring exactly i times
  fv <- table(freq_table)
  
  # Compute the D-measure using the formula
  D <- sum(as.numeric(names(fv)) * fv * 
             (as.numeric(names(fv)) - 1) / (N * (N - 1)))
  
  return(D)
}


# General function for lexical diversity indexes, will call the specific functions
lexical_diversity_general <- function(df,
                                      upos = NA,
                                      content_upos = c("NOUN", "VERB", "ADJ", "ADV"),
                                      window_size = 50) {

  # check that a tokens vector is provided
  if (missing(df)) {
    stop("No corpus provided.")
  }

  # check that I have the required columns
  if (!all(c("token", "doc_id") %in% colnames(df))) {
    stop("The corpus db does not have all the required columns (doc_id, token).")
  }
  
  # Filter
  # if we have a start column, we can apply this rule to remove UDPipe-generated tokens
  if ("start" %in% colnames(df)) {
    df <- df %>% filter(!is.na(start))
  }
  
  # and if we have our "compte" custom column we can use that
  if ("compte" %in% colnames(df)) {
    df <- df %>% filter(compte)
  } else {
    # otherwise, we can at least remove punctuation as it does not count for a word
    df <- df %>% filter(!upos %in% c("PUNCT"))
  }
  
  df_result <- df %>%
    group_by(doc_id) %>%
    summarise(
      TTR = calculate_TTR(token, token),
      maas = calculate_maas(token, token),
      MATTR = calculate_moving_TTR(token, window_size),
      simpsons_D = D_measure_from_tokens(token)
    )
  
  # if we have a upos column in df, we can also do content words only
  # check if df has a upos column
  if ("upos" %in% colnames(df)) {
    df_result_extra <- df %>%
      filter(!is.na(upos) & upos %in% content_upos) %>%
      group_by(doc_id) %>%
      summarise(
        TTR_content = calculate_TTR(token, token),
        maas_content = calculate_maas(token, token),
        MATTR_content = calculate_moving_TTR(token, window_size),
        simpsons_D_content = D_measure_from_tokens(token)
      )
    df_result <- left_join(df_result, df_result_extra, by = "doc_id")
  }
  
  return(df_result)
}
