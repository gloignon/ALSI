#' Compute T-unit Complexity Features
#'
#' Identifies T-unit boundaries in a parsed corpus and returns all 14 measures
#' from Lu's (2010) L2SCA battery, plus a coordination proportion index,
#' aggregated per document. The T-unit construct itself originates with Hunt
#' (1965); Lu is the source for all 14 ratios, including MLT.
#'
#' A T-unit (minimal terminable unit) is an independent clause together with
#' all subordinate clauses and non-clausal phrases attached to it. In UD
#' terms, each sentence contains one T-unit rooted at the sentence root; each
#' coordinated predicate reachable from that root via a chain of \code{conj}
#' arcs, whose head token is a predicate (VERB, AUX, or ADJ/NOUN-with-cop)
#' AND has its own subject (\code{nsubj}, \code{nsubj:pass}, \code{csubj},
#' \code{csubj:pass}, or \code{expl}), starts an additional T-unit.
#' Shared-subject coordination ("Marie chante et danse") counts as one
#' T-unit, since the second verb has no subject of its own — per Hunt
#' (1965, p.20-21), whose own worked example slices "They tried and tried."
#' into a single T-unit on exactly this basis ("each [unit] will contain
#' only one main clause").
#'
#' @param dt A parsed data.table (parser output after
#'   \code{post_process_lexicon}) containing at minimum: \code{doc_id},
#'   \code{paragraph_id},
#'   \code{sentence_id}, \code{token_id}, \code{head_token_id},
#'   \code{dep_rel}, \code{upos}, and \code{compte}.
#' @param count_parataxis Logical, default \code{FALSE}. When \code{TRUE}, the
#'   T-unit BFS also follows \code{parataxis} (and \code{parataxis:*}) arcs in
#'   addition to \code{conj}, so asyndetic main-clause coordination ("Le soleil
#'   brille, les oiseaux chantent.") opens a new T-unit. OFF by default because
#'   UD \code{parataxis} also covers comment clauses ("Marie est partie, je
#'   crois.") and quotative framing, which would then be over-counted. See the
#'   note in \code{count_tunits_in_sent()} in \code{fnt_syntactic_complexity.R}.
#'
#' @returns A data.frame with one row per \code{doc_id} and columns:
#'   \describe{
#'     \item{n_tunits}{Total number of T-units in the document.}
#'     \item{n_sentences}{Total number of orthographic sentences.}
#'     \item{mls}{Mean length of sentence in tokens (Lu 2010 MLS). PUNCT
#'       excluded.}
#'     \item{mlt}{Mean length of T-unit in tokens (Lu 2010 MLT). PUNCT
#'       excluded.}
#'     \item{mlc}{Mean length of clause in tokens (Lu 2010 MLC). Total tokens
#'       divided by total clauses (T-units + dependent clauses). PUNCT
#'       excluded.}
#'     \item{c_s}{Clauses per sentence (Lu 2010 C/S).}
#'     \item{t_s}{T-units per sentence (Lu 2010 T/S), equivalent to Hunt's
#'       (1966) main clause coordination index. Values above 1 indicate
#'       sentences with multiple coordinate main clauses.}
#'     \item{c_t}{Clauses per T-unit (Lu 2010 C/T): always >= 1; increases
#'       with subordination depth within each T-unit.}
#'     \item{dc_c}{Dependent clauses per clause (Lu 2010 DC/C). Finite
#'       subordinate clauses (\code{ccomp}, \code{advcl}, \code{acl},
#'       \code{acl:relcl}) divided by total clauses.}
#'     \item{dc_t}{Dependent clauses per T-unit (Lu 2010 DC/T). Same DC
#'       definition; denominator is T-units rather than clauses.}
#'     \item{ct_t}{Complex T-unit ratio (Lu 2010 CT/T). Proportion of T-units
#'       that contain at least one dependent clause. Ranges from 0 to 1.
#'       Computed as a sentence-level proxy: \code{n_dc} is counted per
#'       sentence, not per T-unit, so when a sentence holds multiple
#'       coordinated T-units this under-resolves which specific T-unit
#'       contains the dependent clause.}
#'     \item{vp_t}{Verb phrases per T-unit (Lu 2010 VP/T). One verb phrase per
#'       predicate head — VERB tokens, plus AUX tokens that are not themselves
#'       \code{aux}/\code{aux:pass}/\code{aux:tense}/\code{aux:caus} dependents
#'       (e.g. copulas, modal heads) — divided by total T-units. Auxiliary
#'       chains attached via those four relations are folded into their
#'       governing verb's count, matching Lu's VP1 pattern (a periphrastic
#'       form like "has been running" is one VP, not three).}
#'     \item{cp_t}{Coordinate phrases per T-unit (Lu 2010 CP/T). \code{conj}
#'       arcs whose head is NOUN, PROPN, ADJ, or ADV (phrasal coordination,
#'       not clausal). Lu's CP also covers coordinated VPs; ALSI routes
#'       verbal \code{conj} coordination into \code{t_s}/\code{prop_coord_sent}
#'       (additional T-units) instead, so this is a UD adaptation rather than
#'       a literal CP/T.}
#'     \item{cp_c}{Coordinate phrases per clause (Lu 2010 CP/C). Same CP
#'       definition and UD-adaptation caveat as \code{cp_t}; denominator is
#'       total clauses.}
#'     \item{cn_t}{Complex nominals per T-unit (Lu 2010 CN/T). NOUN tokens
#'       with at least one substantive modifier child. Uses the same relation
#'       set as \code{add_complex_nominal_flag()} in \code{fnt_syntactic_complexity.R}.}
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
#'   set whose UPOS is \code{VERB}, \code{AUX}, or \code{ADJ}/\code{NOUN} with
#'   a \code{cop} child, AND which has its own subject dependent
#'   (\code{nsubj}, \code{nsubj:pass}, \code{csubj}, \code{csubj:pass}, or
#'   \code{expl}), marks the head of a new T-unit. A conj-reachable predicate
#'   with no subject of its own (subject shared by ellipsis under
#'   coordination) is folded into the same T-unit as its head instead. The
#'   root itself always anchors the first T-unit regardless of whether it has
#'   a subject (e.g. imperatives), so \code{n_tunits >= n_sentences}.
#'
#'   \strong{Dependent clause (DC)}: finite subordinate clause heads with
#'   dep_rel in \code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl}.
#'   Non-finite \code{xcomp} is excluded following Lu's (2010) definition.
#'
#'   \strong{Limitations}: gapping (a fully elided verb in the second
#'   conjunct, e.g. "Mary likes coffee, John tea") is not detected, since
#'   there is no token to anchor the T-unit boundary on. Sentences joined by
#'   bare juxtaposition that the parser annotates with \code{parataxis} rather
#'   than \code{conj} (asyndetic coordination, some run-ons) are undercounted
#'   by default, since the BFS has no \code{conj} edge to follow between them —
#'   see Hunt's (1965) 68-word run-on example in
#'   \code{validation/l2sca_reference_texts/}. Pass
#'   \code{count_parataxis = TRUE} to also follow \code{parataxis} arcs and
#'   recover these (at the cost of over-counting comment/quotative clauses).
#'   Comma splices are NOT automatically undercounted: the French GSD model
#'   often parses "Marie chante, Pierre danse." with a \code{conj} arc across
#'   the comma, in which case the second T-unit IS detected. Whether
#'   juxtaposition is undercounted depends on the parser's
#'   \code{conj}-vs-\code{parataxis} choice, not on the punctuation. Non-verbal
#'   coordination on
#'   nouns or adjectives without a \code{cop} child is excluded because the
#'   conj head is not a predicate.
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
#'   Hunt, K. W. (1966). Recent measures in syntactic development.
#'   \emph{Elementary English, 43}(7), 732--739.
tunit_features <- function(dt, count_parataxis = FALSE) {
  dt_corpus <- setDT(copy(dt))

  # --- Mark predicates and find T-unit starters (shared with fnt_syntactic_complexity.R) ---
  dt_corpus <- add_predicate_flag(dt_corpus)

  sent_keys <- c("doc_id", "paragraph_id", "sentence_id")

  # T-unit count per sentence (uses full token set for tree traversal)
  dt_sent_tu <- dt_corpus[, .(
    n_tunits = count_tunits_in_sent(token_id, head_token_id, dep_rel, is_predicate,
                                    count_parataxis = count_parataxis)
  ), by = sent_keys]

  # Token count per sentence (compte == TRUE excludes PUNCT)
  dt_sent_len <- dt_corpus[compte == TRUE, .(n_tokens = .N), by = sent_keys]

  # Finite dependent clause heads (Lu DC): ccomp, advcl, acl, acl:relcl.
  # xcomp is non-finite in UD and excluded per Lu (2010).
  dc_rels <- c("ccomp", "advcl", "acl", "acl:relcl")

  # Coordinate phrase heads: conj arcs whose head UPOS is nominal/adjectival
  # (phrasal coordination within a T-unit, not clausal T-unit boundaries).
  cp_head_upos <- c("NOUN", "PROPN", "ADJ", "ADV")

  # Verb phrases (Lu VP1: "a VP directly dominated by a clause node"). In a
  # constituency tree, periphrastic forms ("has been running") nest VPs inside
  # VPs, so VP1 fires once for the whole auxiliary chain — not once per
  # auxiliary. The dependency equivalent is one count per predicate head, with
  # its auxiliary dependents folded into that same head (mirrors the
  # has_cv_child grouping used for is_complex_verb in fnt_syntactic_complexity.R).
  # The French GSD model never emits a bare "aux" — periphrastic auxiliaries
  # surface via aux:tense (compound past tenses, e.g. "a mangé"), aux:pass
  # (passives), and aux:caus (causatives). Restricting to c("aux", "aux:pass")
  # would silently miss aux:tense/aux:caus and inflate vp_t for the bulk of
  # French periphrastic tenses.
  aux_dep_rels <- c("aux", "aux:pass", "aux:tense", "aux:caus")

  dt_sent_vocab <- dt_corpus[compte == TRUE, .(
    n_vp = sum(upos == "VERB" | (upos == "AUX" & !dep_rel %in% aux_dep_rels)),
    n_dc = sum(dep_rel %in% dc_rels),
    n_cp = sum(dep_rel == "conj" & upos[match(head_token_id, token_id)] %in% cp_head_upos,
               na.rm = TRUE)
  ), by = sent_keys]

  # Complex nominals: delegate to the shared helper in fnt_syntactic_complexity.R
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
