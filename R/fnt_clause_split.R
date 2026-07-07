#' Assign Tokens to Clauses
#'
#' Splits each sentence into clauses and labels every token with the clause
#' it belongs to. A clause head is either an independent-clause (T-unit)
#' predicate — the sentence root, or a \code{conj}-reachable predicate with
#' its own subject, per the Hunt (1965) T-unit definition already used by
#' \code{count_tunits_in_sent()} in \code{fnt_syntactic_complexity.R} — or a
#' finite dependent-clause head matched by \code{dc_rels}, per Lu's (2010)
#' definition used by the same file and by \code{tunit_features()} in
#' \code{fnt_tunits.R}. This function does not introduce a new clause
#' construct; it materialises the token-level membership that those two
#' files only count.
#'
#' @param dt A parsed data.table (parser output after
#'   \code{post_process_lexicon}) containing at minimum: \code{doc_id},
#'   \code{paragraph_id}, \code{sentence_id}, \code{token_id},
#'   \code{head_token_id}, \code{dep_rel}, and \code{upos}.
#' @param dc_rels Character vector of UD relations used as a dependent-clause
#'   head. Defaults to the same set as \code{extra_syntactic_features()} in
#'   \code{fnt_syntactic_complexity.R} (\code{ccomp}, \code{advcl}, \code{acl},
#'   \code{acl:relcl}), so clause counts from that function and clause
#'   membership from this one stay consistent under the same call.
#' @param count_parataxis Logical, default \code{FALSE}. Passed to the
#'   T-unit-head detection: when \code{TRUE}, a \code{parataxis}-reachable
#'   predicate with its own subject also starts a new clause. See
#'   \code{count_tunits_in_sent()} in \code{fnt_syntactic_complexity.R} for
#'   the rationale for keeping this off by default.
#' @param require_finite Logical, default \code{FALSE}. When \code{FALSE},
#'   a \code{dc_rels} head starts a dependent clause purely on its UD
#'   relation, matching Lu (2010)'s relation-based approximation used by the
#'   clause/T-unit \emph{count} features (so those stay stable). When
#'   \code{TRUE}, a \code{dc_rels} head only starts a clause if it introduces
#'   a \emph{finite} clause, i.e. its head is a finite predicate: either a
#'   \code{VERB}/\code{AUX} that is not clearly non-finite (its \code{feats}
#'   must not carry \code{VerbForm=Inf|Part|Ger|Conv}), or a non-verbal token
#'   carrying a finite copula/auxiliary child. A verbal head with no
#'   \code{VerbForm} at all is treated as finite ("finite unless clearly
#'   non-finite"), so a genuine finite clause is not dropped when the parser
#'   omits the feature. This gates out (i) non-finite adnominal participles
#'   (\code{acl}, e.g. "la pomme cueillie") and open complements
#'   (\code{xcomp}, e.g. "va manger"), which modify but do not introduce a new
#'   finite clause, and (ii) an ordinary noun/adjective phrase the parser
#'   happens to attach with a clausal relation (e.g. an object \code{NOUN}
#'   mis-tagged \code{xcomp}) --- a bare \code{NOUN}/\code{ADJ} with no copula
#'   is not a clause. Requires \code{upos}; a \code{feats} column is used if
#'   present and treated as all-\code{NA} (every verbal head finite) if absent.
#'   T-unit heads (root and \code{conj}-coordinated predicates) are unaffected
#'   by this flag.
#'
#' @returns A copy of \code{dt} with two additional columns:
#'   \describe{
#'     \item{clause_id}{Integer, 1-based, unique per clause within each
#'       sentence (\code{doc_id}, \code{paragraph_id}, \code{sentence_id}).
#'       Ids follow token order of the clause heads, so clause 1 is not
#'       necessarily the root's clause (a clause head preceding the root ---
#'       e.g. a center-embedded relative clause --- takes id 1). To recover
#'       the main clause, look up the \code{clause_id} of the
#'       \code{dep_rel == "root"} token rather than assuming 1. \code{NA} for
#'       UD multiword-token range rows and empty nodes (see Details).}
#'     \item{is_clause_head}{Logical; \code{TRUE} for the token that anchors
#'       the clause (the T-unit predicate or dependent-clause head). \code{NA}
#'       for UD multiword-token range rows and empty nodes.}
#'   }
#'
#' @details
#' UD multiword-token range rows (\code{token_id} like \code{"30-31"}, e.g.
#' French "au" = "à" + "le") and empty nodes (\code{token_id} like
#' \code{"8.1"}, for elided predicates) carry no dependency edge of their own
#' (\code{head_token_id}/\code{dep_rel} are \code{NA}) and are excluded from
#' clause assignment, matching the same \code{token_id} filter used in
#' \code{fnt_corpus.R}'s \code{post_process_lexicon()}. The syntactic tokens
#' they expand into (e.g. "à" and "le" for the "au" row above) are ordinary
#' rows and are assigned to a clause normally.
#'
#' Clause heads are identified first (root, T-unit predicates via the
#' existing conj/parataxis BFS, and \code{dc_rels} heads). Every token is
#' then assigned to a clause by walking up the \code{head_token_id} chain
#' until a clause head (or the sentence root) is reached — the first clause
#' head encountered going upward. Because the walk stops at the nearest
#' enclosing clause head, tokens inside a nested clause (e.g. a relative
#' clause) are assigned to that nested clause rather than to the outer
#' clause that contains it, so clauses do not swallow their embedded
#' sub-clauses. A clause head's own governing relation (e.g. the
#' \code{acl:relcl} arc itself) is attributed to the clause it heads, not
#' to the outer clause — matching Lu's (2010) clause-counting convention
#' that a dependent clause's own head word is part of that clause.
#'
#' @references
#'   Hunt, K. W. (1965). \emph{Grammatical structures written at three grade
#'   levels}. National Council of Teachers of English.
#'
#'   Lu, X. (2010). Automatic analysis of syntactic complexity in second
#'   language writing. \emph{International Journal of Corpus Linguistics},
#'   15(4), 474--496. \doi{10.1075/ijcl.15.4.02lu}
#'
#' @citation_type "adapted" (Hunt 1965 T-unit boundaries, Lu 2010
#'   dependent-clause relation set; both already used for clause counting in
#'   \code{fnt_syntactic_complexity.R} and \code{fnt_tunits.R}, here extended
#'   to per-token clause membership)
#'
#' @export
split_into_clauses <- function(dt,
                               dc_rels = c("ccomp", "advcl", "acl",
                                          "acl:relcl"),
                               count_parataxis = FALSE,
                               require_finite = FALSE) {
  dt_corpus <- setDT(copy(dt))
  dt_corpus <- add_predicate_flag(dt_corpus)

  sent_keys <- c("doc_id", "paragraph_id", "sentence_id")

  # UD multiword-token range rows (token_id "30-31") and empty nodes (token_id
  # "8.1", for elided predicates) carry no dependency edge of their own
  # (head_token_id/dep_rel are NA) and are not part of the tree — they only
  # co-occur with the real syntactic tokens they expand into or annotate.
  # Excluding them from the BFS/upward-walk avoids dereferencing an NA
  # head_token_id; see the same filter in fnt_corpus.R (post_process_lexicon).
  is_syntactic <- !grepl("[-.]", dt_corpus$token_id)

  dt_corpus[, is_clause_head := NA]
  dt_corpus[, clause_id := NA_integer_]

  dt_corpus[is_syntactic, is_clause_head := dep_rel %in% dc_rels]

  # require_finite: a dc_rels head only starts a dependent clause if it
  # introduces a FINITE clause. A constituent opens no clause — and so must
  # not split off — unless its head is a finite predicate:
  #   * a VERB/AUX whose feats are not clearly non-finite, OR
  #   * a non-verbal predicate (ADJ/NOUN/…) carrying a finite cop/aux child.
  # This rejects two things the relation set alone would wrongly split:
  #   * a non-finite adnominal participle / open complement (acl "la pomme
  #     cueillie", xcomp "va manger") — a non-finite VERB with no finite aux;
  #   * an ordinary noun/adjective/PP that the parser happens to attach with a
  #     clausal relation (e.g. Trankit tagging the object noun "pomme" as
  #     xcomp of "mange") — a bare NOUN/ADJ with no copula is not a clause.
  # Finiteness of a verb/aux/cop token is judged "finite unless clearly
  # non-finite": a token with no VerbForm at all counts as finite, so a
  # genuine finite clause is not dropped when the parser omits the feature.
  # Only the dc_rels heads are re-tested; T-unit heads are untouched.
  if (require_finite) {
    if (!"feats" %in% names(dt_corpus)) dt_corpus[, feats := NA_character_]
    dt_corpus[is_syntactic, is_clause_head := is_clause_head &
                .is_finite_clause_head(token_id, head_token_id, upos, feats),
              by = sent_keys]
  }

  dt_corpus[is_syntactic, is_clause_head := is_clause_head |
              .is_tunit_head(token_id, head_token_id, dep_rel, is_predicate,
                             count_parataxis = count_parataxis),
            by = sent_keys]

  dt_corpus[is_syntactic, clause_id := .assign_clause_ids(
    token_id, head_token_id, is_clause_head), by = sent_keys]

  return(dt_corpus)
}

# Per-sentence helper: flags every token that heads a clause under the Hunt
# (1965) T-unit definition — the sentence root, plus every predicate
# reachable from it via a conj (and, optionally, parataxis) chain that has
# its own subject. Mirrors count_tunits_in_sent() in
# fnt_syntactic_complexity.R but returns a membership mask instead of a
# count, so the two stay behaviourally identical under the same inputs.
.is_tunit_head <- function(token_ids, head_token_ids, dep_rels, is_pred,
                           count_parataxis = FALSE) {
  head_flag <- logical(length(token_ids))

  root_tok <- token_ids[dep_rels == "root"]
  if (length(root_tok) == 0L) return(head_flag)
  root_tok <- root_tok[[1L]]
  head_flag[token_ids == root_tok] <- TRUE

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
  if (length(non_root_reachable) == 0L) return(head_flag)

  pred_flags <- is_pred[match(non_root_reachable, token_ids)]
  has_own_subj <- vapply(
    non_root_reachable,
    \(tok) any(head_token_ids == tok & dep_rels %in% .subj_rels),
    logical(1L)
  )
  new_heads <- non_root_reachable[pred_flags & has_own_subj]
  head_flag[token_ids %in% new_heads] <- TRUE
  return(head_flag)
}

# Per-sentence helper: for every token, is it (if the caller flagged it as a
# dc_rels head) the head of a FINITE clause? Used only when
# split_into_clauses(require_finite = TRUE). A token heads a finite clause
# only if it is a finite predicate:
#   (a) a VERB/AUX whose feats are not clearly non-finite
#       (VerbForm=Inf|Part|Ger|Conv), OR
#   (b) a non-verbal token with a finite cop/aux child (a periphrastic /
#       copular predicate — "est cueillie", "a mangé" — is finite via its
#       auxiliary even when the lexical head is a participle or a bare
#       ADJ/NOUN).
# Finiteness of a verb/aux/cop token is "finite unless clearly non-finite":
# a token with no VerbForm at all counts as finite, so a genuine finite
# clause is not dropped when the parser omits the feature. A bare NOUN/ADJ
# with no cop/aux child heads no clause (e.g. an object noun the parser
# mis-attached as xcomp), so it fails both branches. Returns a logical mask
# over token_ids; the caller ANDs it with the dc_rels flag, so non-dc tokens'
# values are irrelevant.
.non_finite_forms <- c("Inf", "Part", "Ger", "Conv")

.is_finite_clause_head <- function(token_ids, head_token_ids, upos, feats) {
  has_vf   <- !is.na(feats) & grepl("VerbForm=", feats)
  verbform <- rep(NA_character_, length(feats))
  verbform[has_vf] <- sub(".*VerbForm=([A-Za-z]+).*", "\\1", feats[has_vf])

  # A verb/aux/cop token is finite unless its own VerbForm is clearly
  # non-finite (a missing VerbForm counts as finite).
  is_finite_pred <- upos %in% c("VERB", "AUX") &
    !(has_vf & verbform %in% .non_finite_forms)

  # (a) the head is itself a finite verb/aux; (b) otherwise it must carry a
  # finite cop/aux CHILD (some token headed by it that is itself a finite
  # predicate). A head that is neither — a bare NOUN/ADJ/participle with no
  # finite verbal support — opens no clause.
  vapply(seq_along(token_ids), function(i) {
    if (is_finite_pred[i]) return(TRUE)
    kids <- head_token_ids == token_ids[i]
    any(kids & is_finite_pred)
  }, logical(1L))
}

# Per-sentence helper: assigns every token to a clause by walking up the
# head_token_id chain until a clause-head token (or the root, token whose
# head is "0") is reached. Clause ids are 1-based, assigned in the order
# clause heads appear in token_ids. The root is NOT guaranteed clause_id 1:
# ids follow token order, so a clause head that precedes the root in the
# sentence (e.g. the verb of a center-embedded relative clause, whose matrix
# verb/root comes later) takes clause_id 1 instead. Callers needing the
# root's clause must look it up via dep_rel == "root", not assume 1 (see
# layout_metataal_bricks() in fnt_metataal.R). The upward walk stops at the
# FIRST clause head encountered, so a token nested inside a dependent clause
# is attributed to that inner clause rather than to the clause that contains
# it.
.assign_clause_ids <- function(token_ids, head_token_ids, is_clause_head) {
  clause_heads <- token_ids[is_clause_head]
  clause_num   <- setNames(seq_along(clause_heads), clause_heads)

  find_clause <- function(tok) {
    seen <- character(0)
    repeat {
      if (tok %in% clause_heads) return(unname(clause_num[[tok]]))
      seen <- c(seen, tok)
      idx  <- match(tok, token_ids)
      if (is.na(idx)) return(NA_integer_)
      parent <- head_token_ids[[idx]]
      if (parent == "0" || parent %in% seen) return(NA_integer_)
      tok <- parent
    }
  }

  return(vapply(token_ids, find_clause, integer(1L)))
}
