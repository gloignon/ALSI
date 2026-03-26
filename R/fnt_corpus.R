# This script contains functions for processing and analyzing text data.
library(data.table)
library(tidyverse)
library(utf8)
library(udpipe)

#' Build a Corpus from Text Files
#'
#' Reads text files from a directory or a single file path and returns a
#' two-column data.table. Optionally cleans French text (normalises
#' punctuation, guillemets, line breaks, inclusive writing dots, etc.).
#'
#' @param path Path to a directory of text files or to a single file.
#' @param verbose Logical; if TRUE, prints progress messages.
#' @param clean Logical; if TRUE (default), applies French-specific text
#'   cleaning via an internal \code{clean_text} helper.
#' @param encoding Character; file encoding passed to \code{readr::read_file}.
#'   Defaults to \code{"UTF-8"}. Use \code{"latin1"} or \code{"windows-1252"}
#'   for legacy French text files. Use \code{"auto"} to auto-detect encoding
#'   from the first file (all files in a directory are assumed to share the
#'   same encoding).
#' @returns A \code{data.table} with columns \code{doc_id} (file basename) and
#'   \code{text}.
build_corpus <- function(path, verbose = FALSE, clean = TRUE, encoding = "UTF-8") {
  if (dir.exists(path)) {
    file_paths <- list.files(path = path, full.names = TRUE)
    if (verbose) message("build_corpus | directory detected")
  } else if (file_test("-f", path)) {
    file_paths <- normalizePath(path)
  } else {
    stop("build_corpus | path does not exist")
  }

  # Auto-detect encoding (per-file when processing a directory)
  per_file_encoding <- NULL
  if (tolower(encoding) == "auto") {
    per_file_encoding <- vapply(file_paths, function(fp) {
      guess <- readr::guess_encoding(fp)
      if (nrow(guess) == 0 || guess$confidence[1] < 0.2) {
        stop("build_corpus | could not detect encoding for '", basename(fp),
             "'. Please specify encoding manually (e.g. encoding = 'latin1').")
      }
      guess$encoding[1]
    }, character(1), USE.NAMES = FALSE)

    # Treat ASCII as UTF-8 (ASCII is a strict subset)
    per_file_encoding[per_file_encoding == "ASCII"] <- "UTF-8"

    unique_enc <- unique(per_file_encoding)
    if (length(unique_enc) == 1) {
      message("build_corpus | auto-detected encoding: ", unique_enc)
    } else {
      enc_table <- table(per_file_encoding)
      enc_summary <- paste0(names(enc_table), " (", enc_table, " file",
                            ifelse(enc_table > 1, "s", ""), ")",
                            collapse = ", ")
      warning("build_corpus | mixed encodings detected: ", enc_summary,
              ". Each file will be read with its detected encoding.",
              call. = FALSE)
    }
    # Set encoding to the first file's for fallback/reporting
    encoding <- per_file_encoding[1]
  }

  get_locale <- function(file_path) {
    if (!is.null(per_file_encoding)) {
      idx <- match(file_path, file_paths)
      locale(encoding = per_file_encoding[idx])
    } else {
      locale(encoding = encoding)
    }
  }

  read_file_safe <- function(file_path) {
    file_locale <- get_locale(file_path)
    tryCatch(
      read_file(file_path, locale = file_locale),
      error = function(e) {
        # Try to guess the correct encoding for a helpful message
        guess <- readr::guess_encoding(file_path)
        suggestion <- if (nrow(guess) > 0) {
          paste0("Detected encoding may be '", guess$encoding[1],
                 "' (confidence: ", round(guess$confidence[1], 2),
                 "). Try: build_corpus(..., encoding = '", guess$encoding[1], "')")
        } else {
          "Try: encoding = 'latin1' or encoding = 'windows-1252'"
        }
        stop("build_corpus | failed to read '", basename(file_path),
             "' as ", encoding, ".\n", suggestion, call. = FALSE)
      }
    )
  }

  normalize_safe <- function(contenu, file_path) {
    tryCatch(
      utf8_normalize(contenu, map_quote = TRUE),
      error = function(e) {
        guess <- readr::guess_encoding(file_path)
        suggestion <- if (nrow(guess) > 0) {
          paste0("Detected encoding may be '", guess$encoding[1],
                 "' (confidence: ", round(guess$confidence[1], 2),
                 "). Try: build_corpus(..., encoding = '", guess$encoding[1], "')")
        } else {
          "Try: encoding = 'latin1' or encoding = 'windows-1252'"
        }
        stop("build_corpus | '", basename(file_path),
             "' is not valid UTF-8.\n", suggestion, call. = FALSE)
      }
    )
  }

  clean_text <- function(file_path) {
    if (verbose) message("working on ", file_path)
    contenu <- read_file_safe(file_path)
    contenu <- normalize_safe(contenu, file_path)
    
    # Remove backslashes
    contenu <- gsub("\\\\", "", contenu)
    
    # Preserve true paragraph breaks (double line breaks), only remove single ones
    contenu <- gsub("\r?\n(?=[^\r\n])", " ", contenu, perl = TRUE)  # single line breaks → space
    contenu <- gsub("(\\r?\\n)\\s+", "\\1", contenu)                # remove space at beginning of line
    contenu <- str_replace_all(contenu, "\r", "")                   # normalize returns
    contenu <- str_replace_all(contenu, "\n{3,}", "\n\n")           # max two line breaks
    
    # Clean punctuation
    # << should be replaced with the right French guillemet
    contenu <- str_replace_all(contenu, "<<", "«")
    # and >> will also become a guillemet
    contenu <- str_replace_all(contenu, ">>", "»")
    contenu <- str_replace_all(contenu, "’", "'")
    contenu <- str_replace_all(contenu, "''", "'")
    contenu <- str_replace_all(contenu, "«", " « ")
    contenu <- str_replace_all(contenu, "»", " » ")
    contenu <- str_replace_all(contenu, "!", "! ")
    contenu <- str_replace_all(contenu, ";", " ; ")
    contenu <- str_replace_all(contenu, ":", " : ")
    # Protect dot(s) between lowercase letters (covers étudiant.e and étudiant.e.s)
    contenu <- gsub("([a-zà-öù-ÿ])\\.([a-zà-öù-ÿ])", "\\1§DOT§\\2", contenu, perl = TRUE)
    # Repeat to catch cases like e.s (two successive substitutions)
    while (grepl("([a-zà-öù-ÿ])\\.([a-zà-öù-ÿ])", contenu, perl = TRUE)) {
      contenu <- gsub("([a-zà-öù-ÿ])\\.([a-zà-öù-ÿ])", "\\1§DOT§\\2", contenu, perl = TRUE)
    }
    # Add a space after true sentence-ending dots (before uppercase/digit)
    contenu <- gsub("\\.(?=\\s*[A-Z0-9ÀÉÈÙ])", ". ", contenu, perl = TRUE)
    # Restore protected dots
    contenu <- gsub("§DOT§", ".", contenu, fixed = TRUE)
    
    contenu <- gsub("\\,(?=[A-Za-zÀÉÈÙ0-9])", ", ", contenu, perl = TRUE)
    contenu <- gsub("\\)(?=[A-Za-zÀÉÈÙ0-9])", ") ", contenu, perl = TRUE)
    
    # Clean extra spaces
    contenu <- str_replace_all(contenu, " {3,}", "  ")
    contenu <- str_replace_all(contenu, " {2}", " ")
    
    return(contenu)
  }
  
  # Create corpus
  dt_corpus <- data.table(
    doc_id = basename(file_paths),
    text = if (clean) unlist(lapply(file_paths, clean_text)) else unlist(lapply(file_paths, function(p) {
      normalize_safe(read_file_safe(p), p)
    }))
  )

  # Warn if replacement characters appear (sign of encoding mismatch)
  has_replacement <- grepl("\uFFFD", dt_corpus$text)
  if (any(has_replacement)) {
    bad_files <- dt_corpus$doc_id[has_replacement]
    warning(
      "build_corpus | replacement characters (\uFFFD) detected in: ",
      paste(bad_files, collapse = ", "),
      ". Check the 'encoding' parameter (current: '", encoding, "').",
      call. = FALSE
    )
  }

  return(dt_corpus)
}

#' Parse Text with UDPipe
#'
#' Tokenises, tags, and dependency-parses text using a UDPipe model. Supports
#' parallel processing via the \pkg{future} / \pkg{future.apply} framework.
#'
#' @param txt Either a data.frame with columns \code{doc_id} and \code{text},
#'   or a character vector of texts.
#' @param ud_model Path to a UDPipe \code{.udpipe} model file.
#' @param n_cores Number of parallel workers (1 = sequential).
#' @param chunk_size Number of documents per chunk when parallelising.
#' @param show_progress Logical; if TRUE, shows a progress bar or messages.
#' @param future_plan Optional \pkg{future} plan. If NULL, uses the current
#'   plan or falls back to sequential.
#' @returns A \code{data.table} in CoNLL-U format (one row per token).
parse_text <- function(txt, ud_model = "models/french_gsd-remix_2.udpipe", n_cores = 1, chunk_size = 10, show_progress = TRUE, future_plan = NULL) {
  
  # Check if the model file exists
  if (!file.exists(ud_model)) {
    stop(paste("UDPipe model not found at:", ud_model))
  }
  
  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x))) {
        stop("parse_text | data.frame must contain columns: doc_id, text")
      }
      return(as.data.table(x[, c("doc_id", "text")]))
    }
    if (is.character(x)) {
      return(data.table(doc_id = paste0("doc_", seq_along(x)), text = x))
    }
    stop("parse_text | txt must be a data.frame with doc_id/text or a character vector")
  }
  
  txt_dt <- normalize_input(txt)
  n_docs <- nrow(txt_dt)
  if (show_progress) {
    message("parse_text | ", n_docs, " text(s) to process")
  }
  if (n_docs == 0) {
    return(data.table())
  }
  
  chunk_size <- max(1, as.integer(chunk_size))
  idx <- split(seq_len(n_docs), ceiling(seq_len(n_docs) / chunk_size))
  chunks <- lapply(idx, function(i) txt_dt[i, , drop = FALSE])
  
  worker_parse <- function(chunk, model_path) {
    if (!exists(".udpipe_model", envir = .GlobalEnv)) {
      assign(".udpipe_model", udpipe::udpipe_load_model(file = model_path), envir = .GlobalEnv)
    }
    parsed <- udpipe::udpipe(x = chunk, object = get(".udpipe_model", envir = .GlobalEnv), trace = FALSE)
    data.table::as.data.table(parsed)
  }
  
  parse_sequential <- function() {
    if (show_progress) {
      pb <- utils::txtProgressBar(min = 0, max = length(chunks), style = 3)
      on.exit(close(pb), add = TRUE)
    }
    res <- vector("list", length(chunks))
    for (i in seq_along(chunks)) {
      res[[i]] <- worker_parse(chunks[[i]], ud_model)
      if (show_progress) utils::setTxtProgressBar(pb, i)
    }
    data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
  }
  
  if (n_cores <= 1 || length(chunks) == 1) {
    return(parse_sequential())
  }
  
  n_cores <- min(n_cores, length(chunks))
  
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    if (show_progress) {
      message("parse_text | future/future.apply not available, falling back to sequential")
    }
    return(parse_sequential())
  }
  
  safe_worker <- function(chunk, model_path) {
    tryCatch(
      worker_parse(chunk, model_path),
      error = function(e) {
        structure(list(error = conditionMessage(e)), class = "parse_text_error")
      }
    )
  }
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  if (!is.null(future_plan)) {
    if (is.function(future_plan)) {
      future::plan(future_plan, workers = n_cores)
    } else {
      future::plan(future_plan)
    }
  } else {
    if (inherits(old_plan, "sequential") || inherits(old_plan, "SequentialFuture")) {
      return(parse_sequential())
    }
    if (future::supportsMulticore()) {
      future::plan(future::multicore, workers = n_cores)
    } else {
      future::plan(future::multisession, workers = n_cores)
    }
  }
  
  run_future <- function() {
    if (show_progress) {
      message("parse_text | running in parallel with future.apply")
    }
    future.apply::future_lapply(
      chunks,
      function(chunk) safe_worker(chunk, ud_model),
      future.seed = TRUE
    )
  }
  
  res <- tryCatch(
    run_future(),
    error = function(e) {
      if (inherits(e, "FutureInterruptError") || inherits(e, "FutureError")) {
        if (show_progress) {
          message("parse_text | parallel interrupted, falling back to sequential")
        }
        return(parse_sequential())
      }
      stop(e)
    }
  )
  
  err_idx <- vapply(res, inherits, logical(1), "parse_text_error")
  if (any(err_idx)) {
    msg <- paste(vapply(res[err_idx], `[[`, "", "error"), collapse = "\n- ")
    stop(paste0("parse_text | worker error(s):\n- ", msg))
  }
  
  data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
}

#' Post-Process UDPipe Output
#'
#' Cleans and enriches raw UDPipe output: adds unique token IDs, strips file
#' extensions from doc IDs, handles double tokens introduced by the parser,
#' removes punctuation/particles from countable tokens, reclassifies copulas
#' as VERB, normalises case and ligatures, and sorts for consistent ordering.
#'
#' @param dt A data.table or data.frame of raw UDPipe output.
#' @returns A cleaned \code{data.table} with additional columns
#'   \code{vrai_token_id}, \code{compte} (logical flag for countable tokens),
#'   and \code{lower_token}.
post_process_lexicon <- function(dt) {
  dt_out <- setDT(copy(dt))

  
  # Add a unique token ID
  dt_out[, vrai_token_id := 1:.N]
  
  # Clean document IDs
  dt_out[, doc_id := str_remove_all(doc_id, "\\.txt$")]
  
  # Remove rows with missing tokens
  dt_out <- dt_out[!is.na(token)]
  
  # Handle double tokens introduced by parsing
  dt_out[, is_double_token := is.na(head_token_id) & is.na(upos)]
  dt_intrus <- dt_out[is_double_token == TRUE, .(
    doc_id, term_id = c(term_id + 1, term_id + 2)
  )][, is_intruder := TRUE]

  dt_out$compte <- TRUE
  dt_out <- merge(dt_out, dt_intrus, all = TRUE)
  dt_out[is_intruder == TRUE, compte := FALSE]
  dt_out[, c("is_intruder", "is_double_token") := NULL]
  
  # Remove punctuation and particles from counts
  dt_out[upos %in% c("PUNCT", "PART"), compte := FALSE]
  
  # Correct copula to VERB
  dt_out[dep_rel == "cop", upos := "VERB"]
  
  # Remove duplicates
  dt_out <- dt_out[!duplicated(dt_out[, .(doc_id, term_id)])]
  
  # Clean columns
  cols_to_remove <- c("start", "end", "xpos", "deps")
  dt_out <- dt_out[, .SD, .SDcols = setdiff(names(dt_out), cols_to_remove)]
  
  # Reclassify relative pronouns as PRON
  dt_out[upos == "ADV" & feats == "PronType=Rel", upos := "PRON"]
  
  # Normalize tokens and lemmas
  dt_out[upos != "PROPN", `:=`(
    token = str_to_lower(token),
    lemma = str_to_lower(lemma)
  )]
  dt_out[, `:=`(
    token = str_replace_all(token, "œ", "oe"),
    lemma = str_replace_all(lemma, "œ", "oe")
  )]
  dt_out[!is.na(token) & str_starts(token, "['-]"), 
              token := str_sub(token, 2)]
  
  # Sort for consistent ordering
  dt_out <- dt_out[order(doc_id, paragraph_id, sentence_id, term_id)]
  
  # Add lowercase token column
  dt_out[, lower_token := tolower(token)]
  
  return(dt_out)
}
