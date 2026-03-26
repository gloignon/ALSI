#' Compute Extra Syntactic Complexity Features
#'
#' Derives dependency-tree-based syntactic complexity measures from a parsed
#' corpus. Computes dependency distance, dependency count per token, clause
#' indicators, complex nominals, and complex verbs at the token level, then
#' aggregates per sentence and per document.
#'
#' @param dt A parsed data.table (UDPipe output after post-processing) containing
#'   at minimum: \code{doc_id}, \code{paragraph_id}, \code{sentence_id},
#'   \code{token_id}, \code{head_token_id}, \code{dep_rel}, \code{upos},
#'   \code{feats}, and \code{compte}.
#'
#' @returns A data.frame (grouped tibble) with one row per \code{doc_id} and columns:
#'   \describe{
#'     \item{avg_clause_length}{Mean tokens per clause.}
#'     \item{complex_nom_per_sent}{Mean complex nominals per sentence.}
#'     \item{complex_verb_per_sent}{Mean complex verbs per sentence.}
#'     \item{avg_dep_dist}{Mean dependency distance (token-to-head gap).}
#'     \item{avg_dep_count}{Mean number of dependents per token.}
#'     \item{clausal_density}{Total clauses divided by number of sentences.}
#'   }
#'
#' @details Clause boundaries are identified by dependency relations: ccomp,
#'   acl, advcl, xcomp, csubj, csubj:pass, and acl:relcl. Complex nominals
#'   are nouns that serve as heads; complex verbs are verbs that serve as heads.
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
    group_by(doc_id, sentence_id) %>%
    summarise(
      n_clause = sum(is_clause_indicator, na.rm = T) + 1,
      n_tokens = n(),
      n_complex_nominal = sum(is_complex_nominal, na.rm = T),
      n_complex_verb = sum(is_complex_verb, na.rm = T),
      dep_dist = mean(dep_dist, na.rm = T),
      dep_count = mean(dep_count, na.rm = T)
    ) %>%
    group_by(doc_id) %>% 
    summarise(
      avg_clause_length = mean(n_tokens / n_clause, na.rm = TRUE),
      complex_nom_per_sent = mean(n_complex_nominal, na.rm = T),
      complex_verb_per_sent = mean(n_complex_verb, na.rm = T),
      avg_dep_dist = mean(dep_dist, na.rm = T),
      avg_dep_count = mean(dep_count, na.rm = T),
      clausal_density = sum(n_clause, na.rm = T) / n(),
    )
  
  return(sum_extra_syn_features)
}
