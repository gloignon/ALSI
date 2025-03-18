extra_syntactic_features <- function(dt) {
  # Extra dependency tree based features
  dt_corpus <- setDT(copy(dt))
  
  # Dependency distance
  dt_corpus[!is.na(feats) & !upos %in% c("PUNCT"), 
            dep_dist := abs(as.numeric(token_id) - as.numeric(head_token_id)), 
            by = c("doc_id", "paragraph_id", "sentence_id")]
  
  # Number of dependents per token
  dt_dep_stats <- dt_corpus[, .(dep_count = .N), by = c("doc_id", "paragraph_id", "sentence_id", "head_token_id")]
  
  # if there is already a dep_count column, remove it
  if ("dep_count" %in% names(dt_corpus)) {
    dt_corpus[, dep_count := NULL]
  }
   
   # merge into dt_corpus
  dt_corpus <- merge(dt_corpus, dt_dep_stats, 
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"), 
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"), 
                     all.x = TRUE)
  
  
  dt_corpus[, is_clause_indicator := dep_rel %in% c("ccomp",
                                                    "acl",
                                                    "advcl",
                                                    "xcomp",
                                                    "csubj",
                                                    "csubj:pass",
                                                    "acl:relcl")]
  
  
  # Complex nominals, verbs, plus clause indicators.
  dt_corpus <- dt_corpus[!upos %in% c("PUNCT", "DET", "ADP"), `:=`(
    is_complex_nominal = fifelse(upos == "NOUN" &
                                   token_id %in% head_token_id, TRUE, FALSE),
    is_complex_verb = fifelse(upos == "VERB" &
                                token_id %in% head_token_id, TRUE, FALSE)
  ), by = .(doc_id, sentence_id)]

  
  sum_extra_syn_features <- dt_corpus %>% 
    filter(compte == TRUE) %>%
    group_by(doc_id) %>% 
    summarise(
      n_complex_nominal = sum(is_complex_nominal, na.rm = T),
      n_complex_verb = sum(is_complex_verb, na.rm = T),
      n_clause = sum(is_clause_indicator, na.rm = T) + 1,
      n_clause_per_sent = (sum(is_clause_indicator, na.rm = T) + 1) / n_distinct(sentence_id),
      avg_clause_length = n() / (sum(is_clause_indicator, na.rm = T) + 1),
      complex_nom_per_sent = sum(is_complex_nominal, na.rm = T) / n_distinct(sentence_id),
      complex_verb_per_sent = sum(is_complex_verb, na.rm = T) / n_distinct(sentence_id),
      avg_dep_dist = mean(dep_dist, na.rm = T),
      avg_dep_count = mean(dep_count, na.rm = T)
    )

  return(sum_extra_syn_features)
}
