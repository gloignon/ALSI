# Rebuild the distributed dependency-triple models from the repo root. The
# same counts feed both conditionings (predict the dependent POS, or predict
# the relation):
#
#   source("R/artefact_builders/build_deprel_ngrams.R")
#   source("R/fnt_deprel_surprisal.R")
#   ng <- build_deprel_ngrams("corpora/fr_gsd-ud-train_alsi-verbpos.conllu",
#                             exclude_pos = c("PUNCT", "SYM"))
#   saveRDS(prepare_deprel_model(ng), "models/deprel_trigram_fr_gsd_alsi.Rds")
#   saveRDS(prepare_attach_model(ng), "models/attach_trigram_fr_gsd_alsi.Rds")

library(data.table)

#' Build a dependency-triple frequency table from a CoNLL-U file
#'
#' Each unit is one dependency arc encoded as the triple (head UPOS,
#' dependency relation, dependent UPOS) — a "bit of tree" rather than a
#' linear window. The companion scorer \code{deprel_surprisal()} treats
#' \code{head_pos} and \code{dep_rel} as the context and \code{dep_pos} as the
#' target, i.e. it models \eqn{p(\text{dep\_pos} \mid \text{head\_pos},
#' \text{dep\_rel})}.
#'
#' Root tokens (\code{head_token_id == "0"}) carry no incoming arc and are
#' never counted as dependents; they still serve as heads for their children.
#'
#' @param conllu_path Path to a CoNLL-U file.
#' @param exclude_pos Character vector of UPOS tags to treat as invisible
#'   (e.g. c("PUNCT", "SYM")). An arc is dropped when either its head or its
#'   dependent carries one of these tags, so punctuation arcs do not enter the
#'   model. Must match the value passed to \code{deprel_surprisal()}.
#' @returns A data.table with columns \code{head_pos}, \code{dep_rel},
#'   \code{dep_pos}, \code{N} (raw count), ordered by descending \code{N}.
build_deprel_ngrams <- function(conllu_path, exclude_pos = character(0)) {
  dt <- setDT(udpipe::udpipe_read_conllu(conllu_path))

  # Head UPOS lookup keyed on (sentence_id, token_id) — built before filtering
  # so root tokens remain available as heads.
  heads <- dt[, .(sentence_id, head_token_id = token_id, head_pos = upos)]

  arcs <- dt[head_token_id != "0"]
  arcs[heads, head_pos := i.head_pos, on = .(sentence_id, head_token_id)]
  arcs[, dep_pos := upos]

  if (length(exclude_pos) > 0L) {
    arcs <- arcs[!head_pos %chin% exclude_pos & !dep_pos %chin% exclude_pos]
  }

  # Drop multi-word-token rows and any arc whose head went missing.
  arcs <- arcs[!is.na(head_pos) & !is.na(dep_pos)]

  return(arcs[, .N, by = .(head_pos, dep_rel, dep_pos)][order(-N)])
}
