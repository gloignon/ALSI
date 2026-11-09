#' Normalize spaCy English (ClearNLP) output to UD-style labels
#'
#' English spaCy pipelines (\code{en_core_web_sm/md/lg/trf}) emit dependency
#' relations from the ClearNLP/Stanford scheme, not Universal Dependencies
#' (UD). ALSI's clause/T-unit logic (\code{tunit_features()},
#' \code{extra_syntactic_features()}) is written against UD labels, because
#' the French backend (\code{spacy_fr_gsd_alsi_v1}) was trained on a UD
#' treebank and already emits UD-style \code{upos}/\code{dep_rel}.
#'
#' This function renames ClearNLP relations to their closest UD equivalent
#' and reconstructs a \code{cop} relation (UD attaches the copula FROM the
#' copula verb TO the predicate; ClearNLP attaches the predicate FROM itself
#' TO the copula verb via \code{attr}/\code{acomp}), so the existing UD-based
#' functions can run unchanged on English spaCy output.
#'
#' See \code{docs/spacy-en-tagset-mapping.md} for the full label-by-label
#' rationale, including which UD relations (\code{acl}, \code{nmod},
#' \code{aux:tense}, \code{aux:caus}, \code{det:nummod}) have no ClearNLP
#' equivalent and are therefore left absent rather than guessed at.
#'
#' @param dt A parsed data.table in ALSI's spaCy output schema (columns
#'   \code{doc_id}, \code{sentence_id}, \code{token_id}, \code{upos},
#'   \code{head_token_id}, \code{dep_rel}, ...) produced by an English
#'   spaCy pipeline (ClearNLP dependency scheme).
#'
#' @returns A new data.table with \code{dep_rel} renamed to UD equivalents,
#'   and \code{cop} relations reconstructed (one per copula verb whose
#'   \code{attr}/\code{acomp} dependent is reinterpreted as the predicate).
#'   \code{upos} is left untouched (\code{en_core_web_*} \code{pos_} already
#'   follows UD).
#'
#' @export
normalize_spacy_en_to_ud <- function(dt) {
  dt <- setDT(copy(dt))

  # Direct 1:1 relabeling: ClearNLP name -> UD name. Relations not in this
  # table (root/conj/nsubj/csubj/expl/ccomp/advcl/aux/amod/nummod/appos/
  # compound/...) are already spelled the same in both schemes.
  rename_map <- c(
    ROOT     = "root",
    nsubjpass = "nsubj:pass",
    csubjpass = "csubj:pass",
    auxpass   = "aux:pass",
    relcl     = "acl:relcl",
    poss      = "nmod:poss"
  )
  to_rename <- dt$dep_rel %in% names(rename_map)
  dt[to_rename, dep_rel := rename_map[dep_rel]]

  # Reconstruct `cop`: ClearNLP attaches the nominal/adjectival predicate TO
  # the copula verb via attr/acomp (predicate is the dependent). UD instead
  # makes the predicate the head, with the copula verb attached to it via
  # `cop`, and every OTHER dependent of the copula verb (subject, expl, aux,
  # conj siblings, ...) also re-attaches to the predicate, since the
  # predicate becomes the clause's structural anchor. Three things change:
  #   1. predicate's row: drop the attr/acomp edge, take over the copula
  #      verb's old (head_token_id, dep_rel) — it is now the clause anchor.
  #   2. copula verb's row: re-point to the predicate as its head, dep_rel
  #      becomes `cop`.
  #   3. every row that pointed at the copula verb as head_token_id, other
  #      than the predicate itself (already handled in step 1) and the
  #      copula verb's own row (handled in step 2): retarget to the
  #      predicate. Without this, a sibling `conj` (e.g. "she was a singer"
  #      coordinated onto "there was a woman") keeps pointing at the old
  #      copula-verb token id, which after step 2 no longer anchors anything,
  #      silently breaking count_tunits_in_sent()'s conj-BFS.
  cop_edges <- dt[dep_rel %in% c("attr", "acomp"),
                   .(doc_id, sentence_id,
                     verb_token_id = head_token_id,
                     pred_token_id = token_id)]
  if (nrow(cop_edges) > 0L) {
    verb_old <- dt[cop_edges,
                    on = c("doc_id", "sentence_id", "token_id" = "verb_token_id"),
                    .(doc_id, sentence_id,
                      verb_token_id = i.verb_token_id,
                      pred_token_id = i.pred_token_id,
                      verb_head     = head_token_id,
                      verb_dep_rel  = dep_rel)]

    # A verb's old head can itself be another copula verb that is also being
    # flipped in this same call (chained copular conjuncts, e.g. "there WAS
    # a woman and she WAS a singer" — the second `was`'s old head is the
    # first `was`, which is itself becoming `cop` of `woman`). Resolve
    # verb_head through the verb_token_id -> pred_token_id map until it
    # settles on a value that is not itself a flipped verb, so every
    # redirected edge ends up pointing at a real (surviving) anchor.
    resolve_map <- setNames(cop_edges$pred_token_id, cop_edges$verb_token_id)
    resolved_head <- verb_old$verb_head
    repeat {
      hit <- as.character(resolved_head) %in% names(resolve_map)
      if (!any(hit)) break
      resolved_head[hit] <- resolve_map[as.character(resolved_head[hit])]
    }
    verb_old[, verb_head := resolved_head]

    # Step 3 first (reads head_token_id before it is overwritten in step 2),
    # excluding the predicate row itself (step 1 handles that one).
    others <- dt[verb_old, on = c("doc_id", "sentence_id", "head_token_id" = "verb_token_id"),
                  .(doc_id, sentence_id, token_id, pred_token_id = i.pred_token_id)]
    others <- others[token_id != pred_token_id]
    if (nrow(others) > 0L) {
      dt[others,
         on = c("doc_id", "sentence_id", "token_id"),
         head_token_id := i.pred_token_id]
    }

    dt[verb_old,
       on = c("doc_id", "sentence_id", "token_id" = "pred_token_id"),
       `:=`(head_token_id = i.verb_head, dep_rel = i.verb_dep_rel)]

    dt[cop_edges,
       on = c("doc_id", "sentence_id", "token_id" = "verb_token_id"),
       `:=`(head_token_id = i.pred_token_id, dep_rel = "cop")]
  }

  return(dt)
}
