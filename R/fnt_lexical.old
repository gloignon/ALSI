fuzzy_match_lexical_db <- function(dt_corpus, dt_lexical,
                                   freq_col = "freq_u",
                                   prefix = "lexicaldb") {
  
  # we will work on copies to avoid problems outside the function
  dt_corpus <- setDT(copy(dt_corpus))
  dt_lexical <- setDT(copy(dt_lexical))
  
  # dt_corpus <- dt_parsed_lexical
  # dt_lexical <- dt_flelex

  # token_col = "eqol_token"
  # upos_col = "upos"
  # lemma_col = "eqol_lemma"

  # make sure the token column name exists in dt_lexical
  if (!"token" %in% colnames(dt_lexical)) {
    stop("The token column does not exist in the lexical db.")
  }
  
  # make sure the freq_col column exists in dt_lexical
  if (!(freq_col %in% colnames(dt_lexical))) {
    stop("The specified frequency column does not exist in the lexical db.")
  }
  # copy that column that to .sorting_col
  dt_lexical[, .sorting_col := get(freq_col)]
  
  # check if dt_corpus has a token, upos and lemma columns, plus the 
  # vrai_token_id column created earlier
  if (!all(c("token", "lemma", "upos", "vrai_token_id") %in% colnames(dt_corpus))) {
    stop("The corpus db does not have all the required columns.")
  }
  
  # make a vector of the columns this function will add
  new_cols <- paste0(prefix, "_", setdiff(names(dt_lexical), c("token", "upos", "lemma", ".sorting_col")))
  # if the new_cols column already exist in dt_corpus, delete with warning
  if (any(new_cols %in% colnames(dt_corpus))) {
    warning("Some columns already exist in the corpus db. They will be overwritten.")
    dt_corpus[, (new_cols) := NULL]
  }
  
  if ("upos" %in% colnames(dt_lexical)) {
    upos_present <- TRUE
  } else {
    upos_present <- FALSE
  }

  # merging...
  if (upos_present) {
    dt_merged_exact <-
      merge(dt_corpus,
            dt_lexical,
            by.x = c("lower_token", "upos"),
            by.y = c("token", "upos"),
            all.x = FALSE, 
            all.y = FALSE,
            sort = FALSE)
    # if there is a duplicated vrai_token_id, keep the token with the largest freq_u
    dt_merged_exact <- dt_merged_exact[order(vrai_token_id, -.sorting_col)]
    dt_merged_exact <- dt_merged_exact[!duplicated(vrai_token_id)]
  } else {
    dt_merged_exact <- NULL
  }
  
  # then we try matching the remaining tokens by token only
  dt_merged_token <-
    merge(dt_corpus[!vrai_token_id %in% dt_merged_exact$vrai_token_id],
          dt_lexical[, .SD, .SDcols = setdiff(names(dt_lexical), c("upos", "lemma"))],
          by.x = c("lower_token"),
          by.y = c("token"),
          all.x = FALSE, 
          all.y = FALSE,
          sort = FALSE)
  # if there is a duplicated vrai_token_id, keep the token with the largest freq_u
  dt_merged_token <- dt_merged_token[order(vrai_token_id, -.sorting_col)]
  dt_merged_token <- dt_merged_token[!duplicated(vrai_token_id)]
  
  # check if there are unmatched tokens
  # if there are still unmatched tokens
  # we will try matching the corpus lemmas with the tokens in the lexical db
  dt_merged_crossed <-
    merge(dt_corpus[!vrai_token_id %in% dt_merged_exact$vrai_token_id &
                            !vrai_token_id %in% dt_merged_token$vrai_token_id],
          dt_lexical[, .SD, .SDcols = setdiff(names(dt_lexical), c("upos", "lemma"))],
          by.x = "lemma",
          by.y = "token",
          all.x = FALSE, 
          all.y = FALSE, 
          sort = FALSE)
  # again in case of duplicates we keep the largest freq_u
  dt_merged_crossed <- dt_merged_crossed[order(vrai_token_id, -.sorting_col)]
  dt_merged_crossed <- dt_merged_crossed[!duplicated(vrai_token_id)]
  
  # the "orphans" are the tokens that are still unmatched
  dt_orphans <-
    dt_corpus[!vrai_token_id %in% dt_merged_exact$vrai_token_id &
                !vrai_token_id %in% dt_merged_token$vrai_token_id &
                !vrai_token_id %in% dt_merged_crossed$vrai_token_id]
  
  # we merge the results
  dt_merged <-
    bind_rows(dt_merged_exact, dt_merged_token, dt_merged_crossed, dt_orphans)
  
  # sort the result in the original order
  dt_merged <- dt_merged[order(vrai_token_id)]
  
  # we can now remove the sorting column
  dt_merged[, .sorting_col := NULL]
  
  # rename all new columns that come from dt_lexical to have specified prefix
  cols_to_rename <- setdiff(names(dt_lexical), c("token", "upos", ".sorting_col"))
  new_names <- paste0(prefix, "_", cols_to_rename)
  setnames(dt_merged, cols_to_rename, new_names)
  
  return(dt_merged)
}
