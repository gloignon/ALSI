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
#'   and \code{compte}.
#' @param dc_rels Character vector of UD relations used as a dependent-clause
#'   head. Defaults to a UD approximation of Lu's (2010) finite dependent-clause
#'   definition (\code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl}).
#'   This is an approximation, not an exact match: Lu counts only \emph{finite}
#'   dependent clauses, whereas these UD relations are not conditioned on
#'   finiteness, so a non-finite \code{acl}/\code{advcl} head (e.g. a
#'   participial or infinitival adjunct) is still counted. The set is chosen
#'   because, in parsed French corpora, the excluded non-finite \code{xcomp} is
#'   the dominant non-finite complement and \code{csubj} is essentially absent
#'   (see the note below). Supply a different set to use an alternative
#'   clause-boundary definition (e.g. the broader, pre-2026-06 ALSI list that
#'   also included \code{xcomp}, \code{csubj}, and \code{csubj:pass}).
#'
#' @returns A data.frame (grouped tibble) with one row per \code{doc_id} and columns:
#'   \describe{
#'     \item{avg_clause_length}{Mean tokens per clause (Lu MLC).}
#'     \item{complex_nom_per_sent}{Mean complex nominals per sentence.}
#'     \item{complex_verb_per_sent}{Mean complex verbs per sentence.}
#'     \item{avg_dep_dist}{Pooled mean dependency distance (Liu 2008 MDD,
#'       Formula 2: total |token-to-head gap| / total real dependencies).}
#'     \item{avg_dep_count}{Mean out-degree per token, pooled across the
#'       document (total out-degree / total countable tokens; leaves count
#'       as 0).}
#'     \item{clausal_density}{Total clauses divided by number of sentences (Lu C/S).}
#'     \item{dc_per_clause}{Dependent clauses per clause (Lu DC/C): total
#'       dependent clauses / total clauses.}
#'     \item{cn_per_clause}{Complex nominals per clause (Lu CN/C): total complex
#'       nominals / total clauses.}
#'     \item{cv_per_clause}{Complex verbs per clause (ALSI analogue of CN/C):
#'       total complex verbs / total clauses.}
#'     \item{avg_words_before_root}{Mean number of countable tokens preceding
#'       the sentence root (main verb), averaged over sentences with a root
#'       (Vajjala & Meurers 2012, adapted — pre-verbal working-memory load).}
#'   }
#'
#' @citation_type \code{clausal_density}, \code{avg_clause_length},
#'   \code{dc_per_clause}, \code{complex_nom_per_sent}, \code{cn_per_clause}:
#'   "adapted" (Lu 2010, Penn-Treebank tregex translated to UD dependencies).
#'   \code{avg_dep_dist}: "computationally_identical" (Liu 2008, Formula 2).
#'   \code{complex_verb_per_sent}, \code{cv_per_clause}: ALSI-derived (no Lu
#'   index; "inspired" by UD auxiliary-chain annotation, cf. Duran et al. 2021).
#'   \code{avg_dep_count}: derived graph statistic (no Lu/Liu source).
#'   \code{avg_words_before_root}: "adapted" (Vajjala & Meurers 2012; their
#'   main-clause main-verb anchor approximated by the UD root).
#'   See \code{docs/FEATURES_CITATIONS.yaml} for per-feature backing.
#'
#' @details Operationalizations follow Lu (2010, \emph{International Journal of
#'   Corpus Linguistics}, 15(4), 474–496):
#'
#'   \strong{Clause boundaries}: total clauses per sentence = independent
#'   clauses (T-units, counted via the same conj-BFS heuristic as
#'   \code{tunit_features()} in \code{fnt_tunits.R}) plus dependent-clause
#'   heads identified by the \code{dc_rels} relation set. The default
#'   (\code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl}) is a UD
#'   approximation of Lu's finite dependent-clause definition; it excludes
#'   non-finite \code{xcomp} by default but does not test finiteness on
#'   \code{acl}/\code{advcl} heads.
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
#'   \strong{Dependency distance}: Liu's (2008, \emph{Journal of Cognitive
#'   Science}, 9(2), 159–191) sample MDD (his Formula 2): the document distance
#'   is \eqn{\sum |\text{pos(head)} - \text{pos(dependent)}|} over every real
#'   dependency, divided by the total number of real dependencies. Root links
#'   (\code{head_token_id == 0}, DD defined as 0 by Liu) and PUNCT tokens are
#'   excluded; the morphological-features column is not used as a filter (Liu's
#'   formula is not conditioned on morphology). This is the same pooled
#'   estimator as \code{avg_head_distance} in \code{fnt_heights.R}.
#'
#' @note Earlier versions of this function used a broader dependent-clause
#'   relation set that also included \code{xcomp}, \code{csubj}, and
#'   \code{csubj:pass}. Inspection of parsed French corpora showed
#'   \code{xcomp} heads are consistently non-finite (infinitives or
#'   predicative-adjective complements) and \code{csubj}/\code{csubj:pass}
#'   essentially do not occur, so the default was narrowed toward Lu's (2010)
#'   finite dependent-clause definition. Pass a custom \code{dc_rels} to
#'   reproduce the older behaviour if needed for comparison.

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

# Add is_predicate column: TRUE for VERB/AUX tokens, and for any token with a
# copula (cop) child, regardless of its own upos (modifies in place). Per UD
# guidelines, cop always attaches to the head of a non-verbal predicate, which
# can be ADJ ("he is tall"), NOUN/PROPN/PRON ("she was a singer"), NUM, etc. —
# not only ADJ. Used to detect independent-clause (T-unit) heads among
# `conj`-coordinated tokens; shared with tunit_features() in fnt_tunits.R.
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
  dt_corpus[, is_predicate := upos %in% c("VERB", "AUX") | has_cop]
  return(dt_corpus)
}

# UD relations that mark a token as having its own subject (as opposed to a
# subject shared by ellipsis under coordination, e.g. "danse" in "Marie
# chante et danse"). Used by count_tunits_in_sent() to decide whether a
# conj-reachable predicate starts a new T-unit.
.subj_rels <- c("nsubj", "nsubj:pass", "csubj", "csubj:pass", "expl")

# Count independent clauses (Hunt 1965 T-units) in a single sentence: the
# root clause plus every predicate reachable from it via a chain of `conj`
# arcs (BFS) that also has its own subject. Shared with tunit_features() in
# fnt_tunits.R, which documents the full rationale.
#
# Per Hunt (1965, p.20-21): "each [unit] will contain only one main clause."
# His own worked example slices "They tried and tried." into a single
# T-unit despite the "and", because both verbs share one subject ("they") —
# it is a compound predicate within one main clause, not two main clauses.
# A conj-reachable predicate with no nsubj/csubj/expl child of its own is
# therefore folded into the same T-unit as its head; only conjuncts that
# introduce their own subject (e.g. "she was a singer" in Bardovi-Harlig &
# Bofman's "There was a woman... and she was a singer" = 2 T-units) start a
# new one.
#
# `count_parataxis` (default FALSE) optionally extends the traversal to follow
# `parataxis` (and `parataxis:*` subtypes) arcs as well as `conj`. This catches
# asyndetic coordination ("Le soleil brille, les oiseaux chantent."), which the
# French GSD model annotates with parataxis rather than conj and which the
# conj-only BFS therefore undercounts. It is OFF by default because UD parataxis
# is a grab-bag: it also covers comment/epistemic clauses ("Marie est partie,
# je crois.") and quotative framing ("..., dit-il."), which Hunt/Lu would keep
# inside a single T-unit. The predicate + own-subject gate suppresses the
# non-clausal cases but NOT comment/quotative clauses (which carry their own
# subject), so enabling this trades a rare true-positive for a more common
# false-positive on running French text. Enable only when the register is known
# to use asyndetic main-clause coordination and few comment clauses.
count_tunits_in_sent <- function(token_ids, head_token_ids, dep_rels, is_pred,
                                 count_parataxis = FALSE) {
  root_tok <- token_ids[dep_rels == "root"]
  if (length(root_tok) == 0L) return(1L)
  root_tok <- root_tok[[1L]]

  # Strip subtypes (e.g. "parataxis:insert" -> "parataxis") so subtype variants
  # are matched uniformly; conj has no subtypes in the French GSD model.
  base_rels   <- sub(":.*$", "", dep_rels)
  follow_rels <- if (count_parataxis) c("conj", "parataxis") else "conj"

  visited  <- root_tok
  frontier <- root_tok
  while (length(frontier) > 0L) {
    edge_mask <- base_rels %in% follow_rels & head_token_ids %in% frontier
    children  <- setdiff(token_ids[edge_mask], visited)
    visited   <- c(visited, children)
    frontier  <- children
  }

  non_root_reachable <- visited[visited != root_tok]
  if (length(non_root_reachable) == 0L) return(1L)

  pred_flags <- is_pred[match(non_root_reachable, token_ids)]
  has_own_subj <- vapply(
    non_root_reachable,
    \(tok) any(head_token_ids == tok & dep_rels %in% .subj_rels),
    logical(1L)
  )
  return(1L + sum(pred_flags & has_own_subj, na.rm = TRUE))
}

extra_syntactic_features <- function(dt,
                                     dc_rels = c("ccomp", "advcl", "acl", "acl:relcl"),
                                     count_parataxis = FALSE) {
  dt_corpus <- setDT(copy(dt))

  # --- Dependency distance (Liu 2008, Formula 2: pooled sample MDD) ---------
  # DD is the absolute linear gap between a token and its governor. Liu defines
  # the root (head_token_id == 0) as DD = 0 and excludes it from the sum (his
  # denominator is n - s, real dependencies only); punctuation is not a
  # syntactic dependency and is likewise excluded. The morphological-features
  # column plays no part in Liu's formula, so dependents are NOT filtered on
  # `feats` here (an earlier version did, dropping valid feats == NA tokens and
  # — for root tokens with non-NA feats — counting a spurious |token_id - 0|
  # distance to the pseudo-root). Document-level MDD is then computed as a
  # pooled ratio of sums (sum|DD| / sum(real deps)), matching avg_head_distance
  # in fnt_heights.R rather than averaging per-sentence means.
  dt_corpus[head_token_id != "0" & !upos %in% c("PUNCT"),
            dep_dist := abs(as.numeric(token_id) - as.numeric(head_token_id)),
            by = c("doc_id", "paragraph_id", "sentence_id")]

  # --- Dependent count per token (mean out-degree) -------------------------
  # Out-degree counts only countable children (compte == TRUE) so excluded
  # tokens (e.g. punctuation) do not inflate a head's degree, mirroring the
  # compte filter applied to every other count below. Tokens that head no
  # countable child are genuine leaves with out-degree 0, not missing data, so
  # their dep_count is set to 0 (not NA) — otherwise leaves would be dropped
  # from the document mean, turning "mean out-degree per token" into "mean
  # child count among non-leaf heads".
  dt_dep_stats <- dt_corpus[compte == TRUE,
                            .(dep_count = .N),
                            by = c("doc_id", "paragraph_id", "sentence_id", "head_token_id")]
  if ("dep_count" %in% names(dt_corpus)) dt_corpus[, dep_count := NULL]
  dt_corpus <- merge(dt_corpus, dt_dep_stats,
                     by.x = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
                     by.y = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
                     all.x = TRUE)
  dt_corpus[is.na(dep_count), dep_count := 0L]

  # --- Clause counts (Lu 2010): independent clauses (T-units) plus finite --
  # dependent clauses. A sentence's clause total is NOT always "indicators +
  # 1" — sentences can contain more than one independent clause (e.g. "Marie
  # chante et Pierre danse"), so independent clauses are counted via the same
  # conj-BFS T-unit logic as tunit_features() in fnt_tunits.R. The dependent-
  # clause relation set defaults to a UD approximation of Lu's (2010) finite
  # dependent-clause definition (xcomp is non-finite and excluded — see
  # fnt_tunits.R for the rationale), but callers may supply their own `dc_rels`
  # to use a different relation set.
  dt_corpus <- add_predicate_flag(dt_corpus)

  dt_corpus[, is_dependent_clause := dep_rel %in% dc_rels]

  sent_keys <- c("doc_id", "paragraph_id", "sentence_id")
  dt_sent_tu <- dt_corpus[, .(
    n_tunits = count_tunits_in_sent(token_id, head_token_id, dep_rel, is_predicate,
                                    count_parataxis = count_parataxis)
  ), by = sent_keys]

  # --- Words before the main verb (Vajjala & Meurers 2012, adapted) ---------
  # Pre-verbal working-memory load: count the countable tokens whose surface
  # position precedes the sentence root (the main predicate). Copular être
  # sentences are reparsed upstream (reparse_copulas) so the copula heads the
  # clause and the root is the verb, matching Vajjala & Meurers' main-clause
  # main-verb anchor. Sentences with no root contribute NA and are dropped from
  # the document mean. Only compte == TRUE tokens are counted, consistent with
  # the other counts in this function (punctuation does not inflate the load).
  dt_sent_bv <- dt_corpus[, {
    root_pos <- as.numeric(token_id[dep_rel == "root"])
    if (length(root_pos) == 0L) {
      .(n_before_root = NA_integer_)
    } else {
      .(n_before_root = sum(compte == TRUE &
                              as.numeric(token_id) < root_pos[[1L]], na.rm = TRUE))
    }
  }, by = sent_keys]

  # --- Complex nominals (Lu 2010 CN1, adapted to UD) and complex verbs -----
  # (ALSI-derived; NOT a Lu index — Lu has VP/T, not a complex-verb count).
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
      # Pooled DD bookkeeping: keep the sentence's DD sum and its count of real
      # (non-root, non-PUNCT) dependencies so the document MDD can be formed as
      # sum|DD| / sum(deps) — Liu's Formula (2), not a mean of sentence MDDs.
      dep_dist_sum      = sum(dep_dist, na.rm = TRUE),
      n_dep             = sum(!is.na(dep_dist)),
      # Pooled out-degree bookkeeping: total out-degree and the number of
      # countable tokens, so the document mean pools per token (sum / count)
      # rather than averaging per-sentence means (which equal-weights short and
      # long sentences). n_tokens above already counts countable tokens.
      dep_count_sum     = sum(dep_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(dt_sent_tu, by = sent_keys) |>
    left_join(dt_sent_bv, by = sent_keys) |>
    mutate(n_clause = n_tunits + n_dc) |>
    group_by(doc_id) |>
    summarise(
      # Lu (2010) MLC: total tokens / total clauses (a per-document ratio of
      # sums, not a mean of per-sentence ratios — matches clausal_density below).
      avg_clause_length     = sum(n_tokens) / sum(n_clause),
      complex_nom_per_sent  = mean(n_complex_nominal,            na.rm = TRUE),
      complex_verb_per_sent = mean(n_complex_verb,               na.rm = TRUE),
      # Liu (2008) Formula (2): pooled sample MDD = sum|DD| / sum(real deps).
      # A document with no real dependencies (e.g. a single root-only token)
      # has denominator 0; report NA rather than NaN.
      avg_dep_dist          = if (sum(n_dep) == 0) NA_real_
                              else sum(dep_dist_sum) / sum(n_dep),
      # Mean out-degree per token, pooled across the document: total out-degree
      # / total countable tokens (matches the ratio-of-sums style used for the
      # Lu measures), not a mean of per-sentence means.
      avg_dep_count         = sum(dep_count_sum) / sum(n_tokens),
      # Vajjala & Meurers (2012): mean tokens before the main verb (root),
      # averaged over sentences that have a root.
      avg_words_before_root = mean(n_before_root, na.rm = TRUE),
      clausal_density       = sum(n_clause,  na.rm = TRUE) / n(),
      # Lu (2010) DC/C and CN/C are sample-level ratios of sums (total
      # dependent clauses / total clauses; total complex nominals / total
      # clauses), consistent with avg_clause_length and clausal_density above —
      # NOT means of per-sentence ratios, which weight short and long sentences
      # equally and do not reproduce Lu's document ratio. cv_per_clause is the
      # ALSI complex-verb analogue, formed the same way.
      dc_per_clause         = sum(n_dc)              / sum(n_clause),
      cn_per_clause         = sum(n_complex_nominal) / sum(n_clause),
      cv_per_clause         = sum(n_complex_verb)    / sum(n_clause),
      .groups = "drop"
    )

  return(sum_extra_syn_features)
}
