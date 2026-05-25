library(data.table)

#' Build a POS 3-gram frequency table from a CoNLL-U file
#'
#' @param conllu_path Path to a CoNLL-U file.
#' @param exclude_pos Character vector of UPOS tags to treat as invisible
#'   (e.g. c("PUNCT", "SYM")). Tokens with these tags are dropped before
#'   forming n-grams, so they do not interrupt sequences.
#' @param use_sentence_boundaries Logical. If \code{TRUE}, prepend two
#'   \code{<BOS>} tokens and append one \code{<EOS>} token to each sentence
#'   before forming trigrams. This lets the model score the first and last
#'   content tokens of every sentence (which would otherwise lack a full left
#'   context). Must match the value passed to \code{pos_surprisal()}.
#' @returns A data.table with columns \code{pos1}, \code{pos2}, \code{pos3},
#'   \code{N} (raw count).
build_pos_ngrams <- function(conllu_path, exclude_pos = character(0),
                             use_sentence_boundaries = FALSE) {
  dt <- setDT(udpipe::udpipe_read_conllu(conllu_path))

  if (length(exclude_pos) > 0L) {
    dt <- dt[!upos %chin% exclude_pos]
  }

  if (use_sentence_boundaries) {
    boundary_rows <- function(sid) {
      data.table(
        sentence_id = sid,
        upos        = c("<BOS>", "<BOS>", dt[sentence_id == sid, upos], "<EOS>")
      )
    }
    dt <- rbindlist(lapply(unique(dt$sentence_id), boundary_rows))
  }

  # pos1=t-2, pos2=t-1, pos3=t — matches scorer convention
  dt[, pos3 := upos]
  dt[, pos2 := shift(upos, 1L), by = sentence_id]
  dt[, pos1 := shift(upos, 2L), by = sentence_id]

  # Drop tokens without a full left context (first two per sentence)
  trigrams <- dt[!is.na(pos1) & !is.na(pos2), .(pos1, pos2, pos3)]

  trigrams[, .N, by = .(pos1, pos2, pos3)][order(-N)]
}
