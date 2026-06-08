
# Iterative LLM-assisted qualitative coding.
#
# Implements the hybrid workflow described in:
#   Dunivin (2025). Scaling hermeneutics: a guide to qualitative coding with
#   LLMs for reflexive content analysis. EPJ Data Science, 14:28.
#   doi:10.1140/epjds/s13688-025-00548-8
#
# Key design decisions grounded in the paper:
#   - One code per LLM call (beats full-codebook approach; Table 3 in Dunivin)
#   - Chain-of-thought (CoT): role → definition → justification → decision
#     (improves mean Cohen's κ from 0.59 to 0.68; Table 3 in Dunivin)
#   - Structured JSON output for machine-readable parsing
#   - llm_fn is a user-supplied backend function (character × character → character),
#     keeping this file independent of any specific LLM provider.
#
# Typical usage:
#   source("R/fnt_qualitative_coding.R")
#   # Define a backend (e.g. wrapping demo_ollama or an API call):
#   my_llm <- function(system_prompt, user_prompt) { ... }
#   codebook <- data.table(
#     code       = c("Activism", "Scholarship"),
#     definition = c("Refers to political or social activism.", "Describes academic work.")
#   )
#   results <- code_corpus(passages, codebook, my_llm)
#   kappas  <- compute_kappa(results, gold_standard)


#' Build a chain-of-thought prompt for one code and one passage
#'
#' Constructs the system and user prompts following the four-step CoT sequence
#' in Dunivin (2025, Fig. 2): role assignment, code definition, justification
#' request, and decision instruction.
#'
#' @param code_name Character. Short label for the category.
#' @param code_definition Character. Full description written for LLM consumption
#'   (see Dunivin 2025, Sect. 2 on adapting codebooks for machine readers).
#' @param passage Character. The text passage to be coded.
#' @returns A named list with elements \code{system} and \code{user}.
build_code_prompt <- function(code_name, code_definition, passage) {
  system_prompt <- paste0(
    "You are applying a category label to a passage of text. ",
    "Read the passage carefully, then decide whether the category applies."
  )

  user_prompt <- paste(
    paste0("Category: ", code_name),
    paste0("Definition: ", code_definition),
    "",
    "Passage:",
    passage,
    "",
    paste0(
      "First, write a brief justification (1-2 sentences) explaining whether ",
      "and why this category applies to the passage."
    ),
    paste0(
      "Then output your decision as JSON on a single line: ",
      '{"applies": true, "rationale": "..."} or {"applies": false, "rationale": "..."}'
    ),
    sep = "\n"
  )

  return(list(system = system_prompt, user = user_prompt))
}


#' Parse the JSON decision from an LLM response
#'
#' Extracts the first JSON object from the raw LLM output. Falls back
#' gracefully when JSON is missing or malformed.
#'
#' @param raw Character. Raw text returned by the LLM.
#' @returns A list with elements \code{applies} (logical or NA) and
#'   \code{rationale} (character).
parse_llm_code_response <- function(raw) {
  json_match <- regmatches(raw, regexpr("\\{[^\\}]+\\}", raw, perl = TRUE))
  if (length(json_match) == 0L) {
    return(list(applies = NA, rationale = trimws(raw)))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(json_match[[1L]]),
    error = function(e) NULL
  )
  if (is.null(parsed) || !("applies" %in% names(parsed))) {
    return(list(applies = NA, rationale = raw))
  }
  return(list(
    applies   = isTRUE(parsed[["applies"]]),
    rationale = as.character(parsed[["rationale"]] %||% "")
  ))
}


#' Apply one code to one passage via an LLM backend
#'
#' @param passage Character. Text to classify.
#' @param code_name Character. Category label.
#' @param code_definition Character. Category description (LLM-adapted).
#' @param llm_fn Function. Backend with signature
#'   \code{function(system_prompt, user_prompt) -> character}.
#' @returns A list with \code{applies} (logical or NA) and \code{rationale}
#'   (character).
apply_code <- function(passage, code_name, code_definition, llm_fn) {
  prompt <- build_code_prompt(code_name, code_definition, passage)
  raw    <- llm_fn(prompt[["system"]], prompt[["user"]])
  return(parse_llm_code_response(raw))
}


#' Apply a codebook to a corpus of passages using an LLM backend
#'
#' For each (passage, code) pair, calls \code{llm_fn} once, following the
#' per-code approach recommended by Dunivin (2025). Returns a long-format
#' \code{data.table} suitable for downstream analysis and validation.
#'
#' @param passages Character vector of text passages, or a \code{data.table}
#'   with columns \code{id} and \code{text}.
#' @param codebook A \code{data.table} (or \code{data.frame}) with columns
#'   \code{code} and \code{definition}. Definitions should be written for LLM
#'   interpretation per Dunivin (2025, Sect. 2).
#' @param llm_fn Function with signature
#'   \code{function(system_prompt, user_prompt) -> character}.
#' @param verbose Logical. If \code{TRUE}, emit progress messages to stderr.
#' @returns A \code{data.table} with columns \code{passage_id}, \code{code},
#'   \code{applies} (logical), and \code{rationale} (character).
code_corpus <- function(passages, codebook, llm_fn, verbose = TRUE) {
  codebook <- data.table::as.data.table(codebook)

  if (is.character(passages)) {
    ids   <- seq_along(passages)
    texts <- passages
  } else {
    dt <- data.table::as.data.table(passages)
    ids   <- dt[["id"]]
    texts <- dt[["text"]]
  }

  n_codes    <- nrow(codebook)
  n_passages <- length(texts)
  total      <- n_passages * n_codes
  k          <- 0L

  results <- vector("list", total)

  for (i in seq_along(texts)) {
    for (j in seq_len(n_codes)) {
      k <- k + 1L
      if (verbose) {
        message(sprintf("[%d/%d] passage_id=%s  code='%s'",
                        k, total, ids[[i]], codebook$code[[j]]))
      }
      res <- apply_code(
        passage         = texts[[i]],
        code_name       = codebook$code[[j]],
        code_definition = codebook$definition[[j]],
        llm_fn          = llm_fn
      )
      results[[k]] <- data.table::data.table(
        passage_id = ids[[i]],
        code       = codebook$code[[j]],
        applies    = res[["applies"]],
        rationale  = res[["rationale"]]
      )
    }
  }

  return(data.table::rbindlist(results))
}

# Null-coalescing helper (avoids dependency on rlang)
`%||%` <- function(a, b) if (!is.null(a)) a else b
