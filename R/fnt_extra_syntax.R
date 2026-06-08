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
#' @param dc_rels Character vector of UD relations that mark a finite
#'   dependent-clause head. Defaults to Lu's (2010) finite-clause set
#'   (\code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl}). Supply a
#'   different set to use an alternative clause-boundary definition (e.g. the
#'   broader, pre-2026-06 ALSI list that also included \code{xcomp},
#'   \code{csubj}, and \code{csubj:pass}).
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
#'   \strong{Clause boundaries}: total clauses per sentence = independent
#'   clauses (T-units, counted via the same conj-BFS heuristic as
#'   \code{tunit_features()} in \code{fnt_tunits.R}) plus finite dependent-
#'   clause heads, identified by the \code{dc_rels} relation set (defaults to
#'   \code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl} — Lu's 2010
#'   finite-clause definition; non-finite \code{xcomp} is excluded).
#'
#'   \strong{Complex nominals} (CN): a NOUN token that has at least one child
#'   with a substantive modifier relation — \code{amod}, \code{nmod},
#'   \code{nmod:poss}, \code{acl}, \code{acl:relcl}, \code{nummod},
#'   \code{appos}, \code{compound}, \code{det:nummod}. Bare nouns with only
#'   \code{det}/\code{case}/\code{punct} dependents are excluded.
#'
#'   \strong{Complex verbs} (CV): a VERB token that has at least one
#'   \code{aux}, \code{aux:pass}, \code{aux:tense}, or \code{aux:caus} child.
#'   This captures passives, periphrastic compound tenses (e.g. French passé
#'   composé "a mangé"), and causative constructions ("faire chanter"). The
#'   French GSD model never emits a bare \code{aux}; its periphrastic
#'   auxiliaries surface via the \code{aux:tense}/\code{aux:caus}/\code{aux:pass}
#'   subtypes, so all three must be checked alongside \code{aux} for the flag
#'   to fire on French compound tenses.
#'
#'   \strong{Dependency distance}: mean \eqn{|\text{pos(head)} -
#'   \text{pos(dependent)}|} over non-PUNCT tokens with non-NA features,
#'   following Liu (2008, \emph{Journal of Cognitive Science}, 9(2), 159–191).
#'
#' @note Earlier versions of this function used a broader dependent-clause
#'   relation set that also included \code{xcomp}, \code{csubj}, and
#'   \code{csubj:pass}. Inspection of parsed French corpora showed
#'   \code{xcomp} heads are consistently non-finite (infinitives or
#'   predicative-adjective complements) and \code{csubj}/\code{csubj:pass}
#'   essentially do not occur, so the default was narrowed to Lu's (2010)
#'   finite-clause set. Pass a custom \code{dc_rels} to reproduce the older
#'   behaviour if needed for comparison.

# UD relations that make a NOUN head a "complex nominal" (Lu 2010).
# Marked (EN) fire mainly on English parses; (FR) on French; unmarked on both.
#   amod       — adjectival modifier               (both)
#   nmod       — nominal post-modifier / PP        (both)
#   nmod:poss  — possessive "X's N"                (EN; French uses det "son/ma")
#   acl        — adnominal clause                  (both)
#   acl:relcl  — relative clause                   (both)
#   nummod     — numeral modifier                  (both)
#   appos      — apposition                        (both)
#   compound   — noun compound "guard dog"         (EN; rare in French UD)
#   det:nummod — numeral determiner                (EN/FR depending on model)
.cn_child_rels <- c("amod", "nmod", "nmod:poss", "acl", "acl:relcl",
                    "nummod", "appos", "compound", "det:nummod")

# Add is_complex_nominal column to a parsed data.table (modifies in place).
# A NOUN token qualifies when at least one of its children has a relation in
# .cn_child_rels. Bare nouns with only det/case/punct dependents are excluded.
add_complex_nominal_flag <- function(dt_corpus) {
  dt_child_flags <- dt_corpus[, .(
    has_cn_child = any(dep_rel %in% .cn_child_rels)
  ), by = .(doc_id, paragraph_id, sentence_id, head_token_id)]

  if ("has_cn_child" %in% names(dt_corpus)) dt_corpus[, has_cn_child := NULL]
  dt_corpus <- merge(dt_corpus, dt_child_flags,
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     all.x = TRUE)
  dt_corpus[is.na(has_cn_child), has_cn_child := FALSE]
  dt_corpus[, is_complex_nominal := upos == "NOUN" & has_cn_child]
  return(dt_corpus)
}

# Add is_predicate column: TRUE for VERB/AUX tokens, and for ADJ tokens with
# a copula (cop) child (modifies in place). Used to detect independent-clause
# (T-unit) heads among `conj`-coordinated tokens; shared with tunit_features()
# in fnt_tunits.R.
add_predicate_flag <- function(dt_corpus) {
  dt_cop <- dt_corpus[dep_rel == "cop",
                      .(has_cop = TRUE),
                      by = .(doc_id, paragraph_id, sentence_id, head_token_id)]

  if ("has_cop" %in% names(dt_corpus)) dt_corpus[, has_cop := NULL]
  dt_corpus <- merge(
    dt_corpus, dt_cop,
    by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
    by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
    all.x = TRUE
  )
  dt_corpus[is.na(has_cop), has_cop := FALSE]
  dt_corpus[, is_predicate := upos %in% c("VERB", "AUX") | (upos == "ADJ" & has_cop)]
  return(dt_corpus)
}

# Count independent clauses (Hunt 1965 T-units) in a single sentence: the
# root clause plus every predicate reachable from it via a chain of `conj`
# arcs (BFS). Shared with tunit_features() in fnt_tunits.R, which documents
# the full rationale (shared-subject coordination, gapping limitations, etc.).
count_tunits_in_sent <- function(token_ids, head_token_ids, dep_rels, is_pred) {
  root_tok <- token_ids[dep_rels == "root"]
  if (length(root_tok) == 0L) return(1L)
  root_tok <- root_tok[[1L]]

  visited  <- root_tok
  frontier <- root_tok
  while (length(frontier) > 0L) {
    conj_mask <- dep_rels == "conj" & head_token_ids %in% frontier
    children  <- setdiff(token_ids[conj_mask], visited)
    visited   <- c(visited, children)
    frontier  <- children
  }

  non_root_reachable <- visited[visited != root_tok]
  if (length(non_root_reachable) == 0L) return(1L)

  pred_flags <- is_pred[match(non_root_reachable, token_ids)]
  return(1L + sum(pred_flags, na.rm = TRUE))
}

extra_syntactic_features <- function(dt,
                                     dc_rels = c("ccomp", "advcl", "acl", "acl:relcl")) {
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

  # --- Clause counts (Lu 2010): independent clauses (T-units) plus finite --
  # dependent clauses. A sentence's clause total is NOT always "indicators +
  # 1" — sentences can contain more than one independent clause (e.g. "Marie
  # chante et Pierre danse"), so independent clauses are counted via the same
  # conj-BFS T-unit logic as tunit_features() in fnt_tunits.R. The dependent-
  # clause relation set defaults to Lu's (2010) finite-clause definition
  # (xcomp is non-finite and excluded — see fnt_tunits.R for the rationale),
  # but callers may supply their own `dc_rels` to use a different relation set.
  dt_corpus <- add_predicate_flag(dt_corpus)

  dt_corpus[, is_dependent_clause := dep_rel %in% dc_rels]

  sent_keys <- c("doc_id", "paragraph_id", "sentence_id")
  dt_sent_tu <- dt_corpus[, .(
    n_tunits = count_tunits_in_sent(token_id, head_token_id, dep_rel, is_predicate)
  ), by = sent_keys]

  # --- Complex nominals and verbs (Lu 2010) --------------------------------
  dt_corpus <- add_complex_nominal_flag(dt_corpus)

  # The French GSD model never emits a bare "aux" — periphrastic auxiliaries
  # surface with language-specific subtypes: aux:tense (compound past tenses,
  # e.g. "a mangé", "est allé" — the bulk of periphrastic forms), aux:pass
  # (passives), and aux:caus (causative "faire" constructions). Restricting
  # to c("aux", "aux:pass") silently misses aux:tense/aux:caus entirely.
  cv_child_rels <- c("aux", "aux:pass", "aux:tense", "aux:caus")
  dt_cv_flags <- dt_corpus[, .(has_cv_child = any(dep_rel %in% cv_child_rels)),
                             by = .(doc_id, paragraph_id, sentence_id, head_token_id)]
  if ("has_cv_child" %in% names(dt_corpus)) dt_corpus[, has_cv_child := NULL]
  dt_corpus <- merge(dt_corpus, dt_cv_flags,
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     all.x = TRUE)
  dt_corpus[is.na(has_cv_child), has_cv_child := FALSE]
  dt_corpus[, is_complex_verb := upos == "VERB" & has_cv_child]
  
  
  sum_extra_syn_features <- dt_corpus |>
    filter(compte == TRUE) |>
    group_by(doc_id, paragraph_id, sentence_id) |>
    summarise(
      n_dc              = sum(is_dependent_clause, na.rm = TRUE),
      n_tokens          = n(),
      n_complex_nominal = sum(is_complex_nominal,  na.rm = TRUE),
      n_complex_verb    = sum(is_complex_verb,     na.rm = TRUE),
      dep_dist          = mean(dep_dist,  na.rm = TRUE),
      dep_count         = mean(dep_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(dt_sent_tu, by = sent_keys) |>
    mutate(n_clause = n_tunits + n_dc) |>
    group_by(doc_id) |>
    summarise(
      # Lu (2010) MLC: total tokens / total clauses (a per-document ratio of
      # sums, not a mean of per-sentence ratios — matches clausal_density below).
      avg_clause_length     = sum(n_tokens) / sum(n_clause),
      complex_nom_per_sent  = mean(n_complex_nominal,            na.rm = TRUE),
      complex_verb_per_sent = mean(n_complex_verb,               na.rm = TRUE),
      avg_dep_dist          = mean(dep_dist,                     na.rm = TRUE),
      avg_dep_count         = mean(dep_count,                    na.rm = TRUE),
      clausal_density       = sum(n_clause,  na.rm = TRUE) / n(),
      dc_per_clause         = mean(n_dc / n_clause,              na.rm = TRUE),
      cn_per_clause         = mean(n_complex_nominal / n_clause, na.rm = TRUE),
      cv_per_clause         = mean(n_complex_verb    / n_clause, na.rm = TRUE),
      .groups = "drop"
    )

  return(sum_extra_syn_features)
}
