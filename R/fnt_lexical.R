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



# Good-Turing frequency estimation ----

# This function will accept a lexical db (dt_lexical) and
# either a column name for the raw frequency or a column name for the frequency
# per million.
# It will return a named vector with N and N1
lexical_db_stats <- function(dt_lexical, freq = "freq", freq_u = "freq_u") {
  # Check if the provided column names exist in the data
  freq_col_exists <- freq %in% names(dt_lexical)
  freq_u_col_exists <- freq_u %in% names(dt_lexical)
  
  if (!freq_col_exists && !freq_u_col_exists) {
    stop("Neither the 'freq' nor the 'freq_u' column was found in dt_lexical.")
  }
  
  # Determine which column to use
  if (freq_col_exists) {
    v_freq <- dt_lexical[[freq]]
  } else {
    v_freq <- dt_lexical[[freq_u]] * 1e6  # Convert freq per million to raw counts
  }
  
  # Calculate total tokens (N) and hapax legomena (N1)
  N_token <- sum(v_freq, na.rm = TRUE)
  min_freq <- min(v_freq, na.rm = TRUE)
  N1_token <- sum(v_freq == min_freq, na.rm = TRUE)
  
  # Return a named numeric vector
  return(data.frame(N = N_token, N1 = N1_token))
}

# Putting it all together: will merge lexical frequencies of the specified
# database, and then apply Good-Turing smoothing to the result.
# Number of tokens (N_token) and hapax legomena (N1_token) can be fed explicitely
# to the function, otherwise will be computed from the lexical database.
add_lexical_freq_with_imputation <- function(parsed_corpus,
                                             lexical_db,
                                             prefix = "",
                                             freq_col = "freq",
                                             mode = "raw",  # or "u"
                                             N_token = NULL,
                                             N1_token = NULL) {
  stopifnot("doc_id" %in% names(parsed_corpus))
  stopifnot("token" %in% names(parsed_corpus))
  stopifnot(freq_col %in% names(lexical_db))
  stopifnot(mode %in% c("raw", "u"))
  
  # Step 1: Merge lexical database using our "fuzzy" algorithm
  matched <- fuzzy_match_lexical_db(parsed_corpus, lexical_db, prefix = prefix)
  
  # Step 2: Compute N and N1 if not provided
  if (is.null(N_token) || is.null(N1_token)) {
    # Get vector of usable frequencies
    freq_vec <- lexical_db[[freq_col]]
    if (mode == "u") {
      freq_vec <- freq_vec * 1e6
    }
    
    # Clean and calculate stats
    N_token <- if (is.null(N_token)) sum(freq_vec, na.rm = TRUE) else N_token
    min_freq <- min(freq_vec, na.rm = TRUE)
    N1_token <- if (is.null(N1_token)) sum(freq_vec == min_freq, na.rm = TRUE) else N1_token
  }
  
  # Step 3: Apply Good-Turing smoothing
  count_col <- paste0(prefix, "_", freq_col)
  matched <- good_turing_smoothing(
    dt = matched,
    count_col = count_col,
    N_token = N_token,
    N1_token = N1_token,
    output_name = paste0(prefix, "_", freq_col, "_imputed")
  )
  
  return(matched)
}


# This is used to impute missing frequencies from lexical databases.
# N_token is number of tokens that were parsed to create the frequency database.
# N1_token is the number of hapax legomena (tokens that occur only once).
# mode indicates if we are sending raw counts ("raw") or frequencies per million ("u").
good_turing_smoothing <- function(dt, count_col, mode = "raw", N_token, N1_token, output_name = NULL) {
  stopifnot("doc_id" %in% names(dt))
  stopifnot(count_col %in% names(dt))
  stopifnot(mode %in% c("raw", "u"))  # "u" = freq per million
  
  dt <- copy(dt)  # work on a copy
  
  # Step 1: Normalize user-provided frequency if needed
  dt[, freq_impute := if (mode == "u") .SD[[1]] * N_token / 1e6 else .SD[[1]], .SDcols = count_col]
  
  # Step 2: Count missing values per document
  doc_info <- dt[is.na(freq_impute), .N, by = doc_id]
  setnames(doc_info, "N", "n_missing")
  
  # Step 3: Compute imputed value
  doc_info[, imputed_value := ifelse(n_missing > 0, N1_token / n_missing, 0)]
  
  # Step 4: Merge and impute
  dt <- merge(dt, doc_info[, .(doc_id, imputed_value)], by = "doc_id", all.x = TRUE, sort = FALSE)
  dt[is.na(freq_impute), freq_impute := imputed_value]
  
  # Step 5: Convert back to user scale if needed
  if (mode == "u") {
    dt[, freq_impute := freq_impute * 1e6 / N_token]
  }
  
  # Step 6: Rename result column
  if (!is.null(output_name)) {
    setnames(dt, "freq_impute", output_name)
  }
  
  # Clean up helper
  dt[, imputed_value := NULL]
  
  return(dt)
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
        simpsons_D_content = D_measure_from_tokens(token),
      )
    df_result <- left_join(df_result, df_result_extra, by = "doc_id")
    
    df_result_verb <- df %>%
      filter(!is.na(upos) & upos %in% "VERB") %>%
      group_by(doc_id) %>%
      summarise(
        maas_verb = calculate_maas(token, token),
        simpsons_D_verb = D_measure_from_tokens(token),
      )
    
    df_result <- left_join(df_result, df_result_verb, by = "doc_id")
  }

  
  return(df_result)
}
