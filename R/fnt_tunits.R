#' Compute T-unit Complexity Features
#'
#' Identifies T-unit boundaries in a parsed corpus and returns all 14 measures
#' from Lu's (2010) L2SCA battery, plus Hunt's (1965) MLT and a coordination
#' proportion index, aggregated per document.
#'
#' A T-unit (minimal terminable unit) is an independent clause together with
#' all subordinate clauses and non-clausal phrases attached to it. In UD
#' terms, each sentence contains one T-unit rooted at the sentence root; each
#' coordinated predicate reachable from that root via a chain of \code{conj}
#' arcs and whose head token is a predicate (VERB, AUX, or ADJ-with-cop)
#' starts an additional T-unit. Shared-subject coordination ("Marie chante et
#' danse") counts as two T-units, following Hunt's standard scoring.
#'
#' @param dt A parsed data.table (UDPipe output after post-processing)
#'   containing at minimum: \code{doc_id}, \code{paragraph_id},
#'   \code{sentence_id}, \code{token_id}, \code{head_token_id},
#'   \code{dep_rel}, \code{upos}, and \code{compte}.
#'
#' @returns A data.frame with one row per \code{doc_id} and columns:
#'   \describe{
#'     \item{n_tunits}{Total number of T-units in the document.}
#'     \item{n_sentences}{Total number of orthographic sentences.}
#'     \item{mls}{Mean length of sentence in tokens (Lu 2010 MLS). PUNCT
#'       excluded.}
#'     \item{mlt}{Mean length of T-unit in tokens (Hunt 1965 / Lu 2010 MLT).
#'       PUNCT excluded.}
#'     \item{mlc}{Mean length of clause in tokens (Lu 2010 MLC). Total tokens
#'       divided by total clauses (T-units + dependent clauses). PUNCT
#'       excluded.}
#'     \item{c_s}{Clauses per sentence (Lu 2010 C/S).}
#'     \item{t_s}{T-units per sentence (Lu 2010 T/S; coordination index,
#'       Bardovi-Harlig 1992). Values above 1 indicate sentences with multiple
#'       coordinate main clauses.}
#'     \item{c_t}{Clauses per T-unit (Hunt 1965 / Lu 2010 C/T): always >= 1;
#'       increases with subordination depth within each T-unit.}
#'     \item{dc_c}{Dependent clauses per clause (Lu 2010 DC/C). Finite
#'       subordinate clauses (\code{ccomp}, \code{advcl}, \code{acl},
#'       \code{acl:relcl}) divided by total clauses.}
#'     \item{dc_t}{Dependent clauses per T-unit (Lu 2010 DC/T). Same DC
#'       definition; denominator is T-units rather than clauses.}
#'     \item{ct_t}{Complex T-unit ratio (Lu 2010 CT/T). Proportion of T-units
#'       that contain at least one dependent clause. Ranges from 0 to 1.}
#'     \item{vp_t}{Verb phrases per T-unit (Lu 2010 VP/T). Total VERB and AUX
#'       tokens divided by total T-units.}
#'     \item{cp_t}{Coordinate phrases per T-unit (Lu 2010 CP/T). \code{conj}
#'       arcs whose head is NOUN, PROPN, ADJ, or ADV (phrasal coordination,
#'       not clausal).}
#'     \item{cp_c}{Coordinate phrases per clause (Lu 2010 CP/C). Same CP
#'       definition; denominator is total clauses.}
#'     \item{cn_t}{Complex nominals per T-unit (Lu 2010 CN/T). NOUN tokens
#'       with at least one substantive modifier child. Uses the same relation
#'       set as \code{add_complex_nominal_flag()} in \code{fnt_extra_syntax.R}.}
#'     \item{cn_c}{Complex nominals per clause (Lu 2010 CN/C). Same CN
#'       definition; denominator is total clauses.}
#'     \item{prop_coord_sent}{Proportion of sentences containing more than one
#'       T-unit. Not part of Lu's battery; added by ALSI.}
#'   }
#'
#' @details
#'   \strong{Clause count}: total clauses = T-units + dependent clauses.
#'   Each T-unit contributes one root (independent) clause; each DC adds one
#'   subordinate clause. This gives \code{c_t = 1 + dc_t} always.
#'
#'   \strong{T-unit boundary rule}: starting from the sentence root, follow
#'   \code{conj} arcs transitively (BFS). Every node in this conj-reachable
#'   set whose UPOS is \code{VERB}, \code{AUX}, or \code{ADJ} with a
#'   \code{cop} child marks the head of a new T-unit. The root itself always
#'   anchors the first T-unit, so \code{n_tunits >= n_sentences}.
#'
#'   \strong{Dependent clause (DC)}: finite subordinate clause heads with
#'   dep_rel in \code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl}.
#'   Non-finite \code{xcomp} is excluded following Lu's (2010) definition.
#'
#'   \strong{Limitations}: the heuristic cannot distinguish shared-subject
#'   coordination from non-shared-subject coordination based on UD structure
#'   alone — both are counted as separate T-units per Hunt's definition.
#'   Gapping is not detected. Non-verbal coordination on nouns or adjectives
#'   is excluded because the conj head is not a predicate.
#'
#'   \strong{Token length}: only tokens where \code{compte == TRUE} are
#'   counted toward MLS, MLT, and MLC, which excludes punctuation.
#'
#' @references
#'   Hunt, K. W. (1965). \emph{Grammatical structures written at three grade
#'   levels} (NCTE Research Report No. 3). National Council of Teachers of
#'   English.
#'
#'   Lu, X. (2010). Automatic analysis of syntactic complexity in second
#'   language writing. \emph{International Journal of Corpus Linguistics,
#'   15}(4), 474--496. \doi{10.1075/ijcl.15.4.02lu}
#'
#'   Bardovi-Harlig, K. (1992). A second look at T-unit analysis.
#'   \emph{TESOL Quarterly, 26}(2), 390--395.
tunit_features <- function(dt) {
  dt_corpus <- setDT(copy(dt))

  # --- Mark predicates: VERB, AUX, or ADJ with a cop child -----------------
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

  # --- BFS over conj arcs from root to find T-unit starters ----------------
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

  sent_keys <- c("doc_id", "paragraph_id", "sentence_id")

  # T-unit count per sentence (uses full token set for tree traversal)
  dt_sent_tu <- dt_corpus[, .(
    n_tunits = count_tunits_in_sent(token_id, head_token_id, dep_rel, is_predicate)
  ), by = sent_keys]

  # Token count per sentence (compte == TRUE excludes PUNCT)
  dt_sent_len <- dt_corpus[compte == TRUE, .(n_tokens = .N), by = sent_keys]

  # Finite dependent clause heads (Lu DC): ccomp, advcl, acl, acl:relcl.
  # xcomp is non-finite in UD and excluded per Lu (2010).
  dc_rels <- c("ccomp", "advcl", "acl", "acl:relcl")

  # Coordinate phrase heads: conj arcs whose head UPOS is nominal/adjectival
  # (phrasal coordination within a T-unit, not clausal T-unit boundaries).
  cp_head_upos <- c("NOUN", "PROPN", "ADJ", "ADV")

  dt_sent_vocab <- dt_corpus[compte == TRUE, .(
    n_vp = sum(upos %in% c("VERB", "AUX")),
    n_dc = sum(dep_rel %in% dc_rels),
    n_cp = sum(dep_rel == "conj" & upos[match(head_token_id, token_id)] %in% cp_head_upos,
               na.rm = TRUE)
  ), by = sent_keys]

  # Complex nominals: delegate to the shared helper in fnt_extra_syntax.R
  dt_corpus <- add_complex_nominal_flag(dt_corpus)
  dt_sent_cn <- dt_corpus[compte == TRUE,
                           .(n_cn = sum(is_complex_nominal, na.rm = TRUE)),
                           by = sent_keys]

  dt_sent <- merge(dt_sent_tu,  dt_sent_len,   by = sent_keys, all.x = TRUE)
  dt_sent <- merge(dt_sent,     dt_sent_vocab, by = sent_keys, all.x = TRUE)
  dt_sent <- merge(dt_sent,     dt_sent_cn,    by = sent_keys, all.x = TRUE)

  for (col in c("n_tokens", "n_vp", "n_dc", "n_cp", "n_cn")) {
    dt_sent[is.na(get(col)), (col) := 0L]
  }

  result <- dt_sent[, {
    tot_tu      <- sum(n_tunits)
    tot_dc      <- sum(n_dc)
    tot_clauses <- tot_tu + tot_dc   # clauses = T-units + dependent clauses
    tot_tokens  <- sum(n_tokens)
    n_sents     <- .N
    .(
      n_tunits        = tot_tu,
      n_sentences     = n_sents,
      # Lu (2010) 14 measures
      mls             = tot_tokens  / n_sents,
      mlt             = tot_tokens  / tot_tu,
      mlc             = tot_tokens  / tot_clauses,
      c_s             = tot_clauses / n_sents,
      t_s             = tot_tu      / n_sents,
      c_t             = tot_clauses / tot_tu,
      dc_c            = tot_dc      / tot_clauses,
      dc_t            = tot_dc      / tot_tu,
      ct_t            = sum(n_dc > 0L) / tot_tu,
      vp_t            = sum(n_vp)  / tot_tu,
      cp_t            = sum(n_cp)  / tot_tu,
      cp_c            = sum(n_cp)  / tot_clauses,
      cn_t            = sum(n_cn)  / tot_tu,
      cn_c            = sum(n_cn)  / tot_clauses,
      # ALSI addition
      prop_coord_sent = mean(n_tunits > 1L)
    )
  }, by = "doc_id"]

  return(as.data.frame(result))
}
