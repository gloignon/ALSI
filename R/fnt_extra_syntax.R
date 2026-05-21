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
#'     \item{avg_clause_length}{Mean tokens per clause (Lu MLC).}
#'     \item{complex_nom_per_sent}{Mean complex nominals per sentence.}
#'     \item{complex_verb_per_sent}{Mean complex verbs per sentence.}
#'     \item{avg_dep_dist}{Mean dependency distance (token-to-head gap, Liu MDD).}
#'     \item{avg_dep_count}{Mean number of dependents per token.}
#'     \item{clausal_density}{Total clauses divided by number of sentences (Lu C/S).}
#'     \item{dc_per_clause}{Mean proportion of clauses that are dependent clauses (Lu DC/C).}
#'     \item{cn_per_clause}{Mean complex nominals per clause (Lu CN/C).}
#'     \item{cv_per_clause}{Mean complex verbs per clause.}
#'   }
#'
#' @details Operationalizations follow Lu (2010, \emph{International Journal of
#'   Corpus Linguistics}, 15(4), 474–496):
#'
#'   \strong{Clause boundaries} are identified by the dependent's UD relation:
#'   \code{ccomp}, \code{acl}, \code{advcl}, \code{xcomp}, \code{csubj},
#'   \code{csubj:pass}, \code{acl:relcl}. Each sentence counts as at least one
#'   clause; each clause-indicator token adds one more.
#'
#'   \strong{Complex nominals} (CN): a NOUN token that has at least one child
#'   with a substantive modifier relation — \code{amod}, \code{nmod},
#'   \code{nmod:poss}, \code{acl}, \code{acl:relcl}, \code{nummod},
#'   \code{appos}, \code{compound}, \code{det:nummod}. Bare nouns with only
#'   \code{det}/\code{case}/\code{punct} dependents are excluded.
#'
#'   \strong{Complex verbs} (CV): a VERB token that has at least one \code{aux}
#'   or \code{aux:pass} child. This captures modals, passives, and periphrastic
#'   tenses (perfect, progressive).
#'
#'   \strong{Dependency distance}: mean \eqn{|\text{pos(head)} -
#'   \text{pos(dependent)}|} over non-PUNCT tokens with non-NA features,
#'   following Liu (2008, \emph{Journal of Cognitive Science}, 9(2), 159–191).
#'
#' @note The clause-boundary relation set is language-agnostic. Some relations
#'   (e.g. \code{acl:relcl} for relative clauses, \code{xcomp} for non-finite
#'   complements) behave differently across UD treebanks and may warrant
#'   language-specific tuning.
#'   TODO: add a \code{clause_rels} parameter so callers can supply a
#'   language-specific profile (e.g. drop \code{xcomp} for English, keep
#'   \code{acl:relcl} for French).
extra_syntactic_features <- function(dt) {
  dt_corpus <- setDT(copy(dt))

  # --- Dependency distance (Liu 2008: mean |pos(head) - pos(dependent)|) ----
  dt_corpus[!is.na(feats) & !upos %in% c("PUNCT"),
            dep_dist := abs(as.numeric(token_id) - as.numeric(head_token_id)),
            by = c("doc_id", "paragraph_id", "sentence_id")]

  # --- Dependent count per token -------------------------------------------
  dt_dep_stats <- dt_corpus[, .(dep_count = .N),
                             by = c("doc_id", "paragraph_id", "sentence_id", "head_token_id")]
  if ("dep_count" %in% names(dt_corpus)) dt_corpus[, dep_count := NULL]
  dt_corpus <- merge(dt_corpus, dt_dep_stats,
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     all.x = TRUE)

  # --- Clause indicators ---------------------------------------------------
  dt_corpus[, is_clause_indicator := dep_rel %in% c("ccomp", "acl", "advcl",
                                                     "xcomp", "csubj",
                                                     "csubj:pass", "acl:relcl")]

  # --- Complex nominals and verbs (Lu 2010) --------------------------------
  # For each token, inspect the dep_rels of its *children* to decide whether
  # it qualifies as a CN or CV — rather than checking merely whether it has
  # any dependent at all.

  # Relations that make a NOUN head "complex" (pre- and post-nominal modifiers).
  # Coverage: relations marked (EN) fire mainly on English parses; relations
  # marked (FR) fire mainly on French parses; unmarked fire on both.
  #   amod        — adjectival modifier         (both)
  #   nmod        — nominal post-modifier / PP  (both)
  #   nmod:poss   — possessive "X's N"          (EN; French uses det "son/ma")
  #   acl         — adnominal clause            (both)
  #   acl:relcl   — relative clause             (both; French GSD treebank uses this)
  #   nummod      — numeral modifier            (both)
  #   appos       — apposition                  (both)
  #   compound    — noun compound "guard dog"   (EN; rare in French UD)
  #   det:nummod  — numeral determiner          (EN/FR depending on model)
  # Language-specific relations that don't appear in a given treebank are
  # simply never matched and cause no harm.
  cn_child_rels <- c("amod", "nmod", "nmod:poss", "acl", "acl:relcl",
                     "nummod", "appos", "compound", "det:nummod")
  # Relations that make a VERB head "complex" (auxiliaries).
  # aux and aux:pass are used consistently across English and French UD models.
  cv_child_rels <- c("aux", "aux:pass")

  dt_child_flags <- dt_corpus[, .(
    has_cn_child = any(dep_rel %in% cn_child_rels),
    has_cv_child = any(dep_rel %in% cv_child_rels)
  ), by = .(doc_id, paragraph_id, sentence_id, head_token_id)]

  for (col in c("has_cn_child", "has_cv_child")) {
    if (col %in% names(dt_corpus)) dt_corpus[, (col) := NULL]
  }
  dt_corpus <- merge(dt_corpus, dt_child_flags,
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     all.x = TRUE)
  dt_corpus[is.na(has_cn_child), has_cn_child := FALSE]
  dt_corpus[is.na(has_cv_child), has_cv_child := FALSE]

  dt_corpus[, is_complex_nominal := upos == "NOUN" & has_cn_child]
  dt_corpus[, is_complex_verb    := upos == "VERB" & has_cv_child]
  
  
  sum_extra_syn_features <- dt_corpus %>%
    filter(compte == TRUE) %>%
    group_by(doc_id, paragraph_id, sentence_id) %>%
    summarise(
      n_clause          = sum(is_clause_indicator, na.rm = TRUE) + 1L,
      n_tokens          = n(),
      n_complex_nominal = sum(is_complex_nominal,  na.rm = TRUE),
      n_complex_verb    = sum(is_complex_verb,     na.rm = TRUE),
      dep_dist          = mean(dep_dist,  na.rm = TRUE),
      dep_count         = mean(dep_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(doc_id) %>%
    summarise(
      avg_clause_length     = mean(n_tokens / n_clause,          na.rm = TRUE),
      complex_nom_per_sent  = mean(n_complex_nominal,            na.rm = TRUE),
      complex_verb_per_sent = mean(n_complex_verb,               na.rm = TRUE),
      avg_dep_dist          = mean(dep_dist,                     na.rm = TRUE),
      avg_dep_count         = mean(dep_count,                    na.rm = TRUE),
      clausal_density       = sum(n_clause,  na.rm = TRUE) / n(),
      dc_per_clause         = mean((n_clause - 1L) / n_clause,   na.rm = TRUE),
      cn_per_clause         = mean(n_complex_nominal / n_clause, na.rm = TRUE),
      cv_per_clause         = mean(n_complex_verb    / n_clause, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(sum_extra_syn_features)
}
