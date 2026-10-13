# This script contains functions for processing and analyzing text data.
library(data.table)
library(tidyverse)
library(utf8)
library(udpipe)


#' Build a Corpus from Text Files
#'
#' Reads text files from a directory or a single file path and returns a
#' two-column data.table. Optionally cleans French text (normalises
#' punctuation, guillemets, line breaks, etc.).
#'
#' Files are read once as raw bytes, decoded with \pkg{stringi}, and cleaned
#' with vectorised regular expressions, so large corpora are handled quickly.
#' A UTF-8 / UTF-16 byte-order mark is stripped, line endings (CRLF, CR, LF)
#' are unified, and control characters (e.g. NUL) are removed.
#'
#' @param path Path to a directory of text files or to a single file.
#' @param verbose Logical; if TRUE, prints progress messages.
#' @param clean Logical; if TRUE (default), applies French-specific text
#'   cleaning (see \code{.clean_french_text}).
#' @param encoding Character; encoding of the input files. Defaults to
#'   \code{"UTF-8"} (an invalid file raises an informative error). Use
#'   \code{"latin1"} or \code{"windows-1252"} for legacy French files. Use
#'   \code{"auto"} to detect the encoding of each file: a BOM is honoured,
#'   valid UTF-8 (including plain ASCII) is read as UTF-8, and anything else
#'   is read as windows-1252 (a superset of Latin-1).
#' @param pattern Regular expression selecting files when \code{path} is a
#'   directory (case-insensitive). Default \code{"\\\\.txt$"}; use \code{NULL}
#'   to read every non-hidden file. Ignored when \code{path} is a single file.
#' @param recursive Logical; if TRUE, also read files in sub-directories.
#'   \code{doc_id} is then the path relative to \code{path}.
#' @returns A \code{data.table} with columns \code{doc_id} (file name, or
#'   relative path when \code{recursive = TRUE}) and \code{text}.
build_corpus <- function(path, verbose = FALSE, clean = TRUE, encoding = "UTF-8",
                         pattern = "\\.txt$", recursive = FALSE) {
  if (dir.exists(path)) {
    doc_ids <- list.files(path = path, pattern = pattern, recursive = recursive,
                          ignore.case = TRUE)
    doc_ids <- doc_ids[!dir.exists(file.path(path, doc_ids))]
    if (length(doc_ids) == 0L) {
      stop("build_corpus | no files matching '", pattern, "' in ", path)
    }
    file_paths <- file.path(path, doc_ids)
    if (!recursive) doc_ids <- basename(doc_ids)
    if (verbose) message("build_corpus | directory detected, ",
                         length(file_paths), " file(s)")
  } else if (file_test("-f", path)) {
    file_paths <- normalizePath(path)
    doc_ids    <- basename(file_paths)
  } else {
    stop("build_corpus | path does not exist")
  }

  reads <- lapply(file_paths, function(fp) {
    if (verbose) message("working on ", fp)
    .read_text_file(fp, encoding)
  })
  texts     <- vapply(reads, `[[`, character(1), "text")
  used_encs <- vapply(reads, `[[`, character(1), "encoding")

  if (tolower(encoding) == "auto") {
    unique_enc <- unique(used_encs)
    if (length(unique_enc) == 1L) {
      message("build_corpus | auto-detected encoding: ", unique_enc)
    } else {
      enc_table <- table(used_encs)
      warning("build_corpus | mixed encodings detected: ",
              paste0(names(enc_table), " (", enc_table, " file",
                     ifelse(enc_table > 1, "s", ""), ")", collapse = ", "),
              ". Each file was read with its detected encoding.",
              call. = FALSE)
    }
  }

  texts <- utf8::utf8_normalize(texts, map_quote = TRUE)
  texts <- stringi::stri_replace_all_regex(
    texts, "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]", "")
  texts <- stringi::stri_replace_all_regex(texts, "\r\n?", "\n")
  if (clean) texts <- .clean_french_text(texts)

  dt_corpus <- data.table(doc_id = doc_ids, text = texts)

  empty <- !nzchar(trimws(dt_corpus$text))
  if (any(empty)) {
    warning("build_corpus | empty document(s): ",
            paste(dt_corpus$doc_id[empty], collapse = ", "), call. = FALSE)
  }

  # Replacement characters are a sign of an encoding mismatch
  has_replacement <- grepl("\uFFFD", dt_corpus$text, fixed = TRUE)
  if (any(has_replacement)) {
    warning(
      "build_corpus | replacement characters (\uFFFD) detected in: ",
      paste(dt_corpus$doc_id[has_replacement], collapse = ", "),
      ". Check the 'encoding' parameter (current: '", encoding, "').",
      call. = FALSE
    )
  }

  return(dt_corpus)
}

# Read one file as raw bytes and decode it to UTF-8.
# Returns list(text, encoding) where encoding is the one actually used.
.read_text_file <- function(file_path, encoding) {
  bytes <- readBin(file_path, what = "raw", n = file.size(file_path))
  auto  <- tolower(encoding) == "auto"

  # Byte-order marks (always stripped; they pick the encoding in auto mode)
  bom_enc <- NULL
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]; bom_enc <- "UTF-8"
  } else if (length(bytes) >= 2L && identical(bytes[1:2], as.raw(c(0xFF, 0xFE)))) {
    bytes <- bytes[-(1:2)]; bom_enc <- "UTF-16LE"
  } else if (length(bytes) >= 2L && identical(bytes[1:2], as.raw(c(0xFE, 0xFF)))) {
    bytes <- bytes[-(1:2)]; bom_enc <- "UTF-16BE"
  }

  enc <- if (!auto) {
    encoding
  } else if (!is.null(bom_enc)) {
    bom_enc
  } else if (stringi::stri_enc_isutf8(bytes)) {
    "UTF-8"
  } else {
    "windows-1252"
  }

  # NUL bytes (e.g. the terminator writeBin() leaves on a string) cannot be
  # decoded; they are meaningful only in UTF-16
  if (!grepl("^UTF-?16", toupper(enc))) bytes <- bytes[bytes != as.raw(0)]

  if (length(bytes) == 0L) return(list(text = "", encoding = enc))

  is_utf8 <- toupper(enc) %in% c("UTF-8", "UTF8")
  if (is_utf8) {
    if (!stringi::stri_enc_isutf8(bytes)) {
      stop("build_corpus | '", basename(file_path), "' is not valid UTF-8.\n",
           "Try: build_corpus(..., encoding = 'auto') or encoding = 'windows-1252'",
           call. = FALSE)
    }
    text <- rawToChar(bytes)
    Encoding(text) <- "UTF-8"
  } else {
    text <- tryCatch(
      stringi::stri_encode(bytes, from = enc, to = "UTF-8"),
      error = function(e) {
        stop("build_corpus | failed to read '", basename(file_path), "' as ",
             enc, ": ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  list(text = text, encoding = if (is_utf8) "UTF-8" else enc)
}

# French-specific cleaning, vectorised over a character vector. Expects "\n"
# line endings (build_corpus normalises them beforehand).
.clean_french_text <- function(x) {
  rx <- stringi::stri_replace_all_regex
  fx <- stringi::stri_replace_all_fixed

  x <- fx(x, "\\", "")                       # remove backslashes

  # Line breaks: a single "\n" before text becomes a space, then whitespace
  # after a line break is dropped. NB: this collapses blank lines too, so
  # paragraphs end up separated by a single "\n".
  x <- rx(x, "\n(?=[^\n])", " ")
  x <- rx(x, "(\n)\\s+", "$1")
  x <- rx(x, "\n{3,}", "\n\n")

  # Punctuation. "<<" / ">>" are guillemets typed as ASCII.
  x <- fx(x, c("<<", ">>", "\u2019", "''", "\u00AB", "\u00BB", "!", ";", ":"),
          c("\u00AB", "\u00BB", "'", "'", " \u00AB ", " \u00BB ", "! ", " ; ", " : "),
          vectorize_all = FALSE)

  # Space after a sentence-ending dot (before uppercase/digit). Dots between
  # lowercase letters (inclusive writing: "\u00E9tudiant.e.s") are untouched.
  x <- rx(x, "\\.(?=\\s*[A-Z0-9\u00C0\u00C9\u00C8\u00D9])", ". ")
  x <- rx(x, ",(?=[A-Za-z\u00C0\u00C9\u00C8\u00D90-9])", ", ")
  x <- rx(x, "\\)(?=[A-Za-z\u00C0\u00C9\u00C8\u00D90-9])", ") ")

  rx(x, " {2,}", " ")                        # collapse runs of spaces
}

#' Parse Text with UDPipe
#'
#' Tokenises, tags, and dependency-parses text using a UDPipe model. Supports
#' parallel processing via the \pkg{future} / \pkg{future.apply} framework.
#'
#' @param txt Either a data.frame with columns \code{doc_id} and \code{text},
#'   or a character vector of texts.
#' @param backend One of \code{"udpipe"} (default), \code{"spacy"}, or
#'   \code{"trankit"}. The trankit backend requires Python and will download
#'   XLM-RoBERTa base (~1.1 GB) on first use.
#' @param ud_model Path to a UDPipe \code{.udpipe} model file.
#'   Ignored when \code{backend != "udpipe"}.
#' @param trankit_model Path to the Trankit model directory (the folder
#'   containing \code{xlm-roberta-base/}). Ignored when
#'   \code{backend != "trankit"}.
#' @param trankit_category Category name used when training the Trankit model.
#'   Default \code{"customized-mwt"}.
#' @param trankit_gpu Logical; use GPU for Trankit inference. Default FALSE.
#' @param n_cores Number of parallel workers (1 = sequential, udpipe only).
#' @param chunk_size Number of documents per chunk when parallelising.
#' @param show_progress Logical; if TRUE, shows a progress bar or messages.
#' @param future_plan Optional \pkg{future} plan. If NULL, uses the current
#'   plan or falls back to sequential.
#' @returns A \code{data.table} in CoNLL-U format (one row per token).
parse_text <- function(txt,
                       backend          = c("udpipe", "spacy", "trankit"),
                       ud_model         = "models/french_gsd-remix_3.udpipe",
                       spacy_model      = "models/spacy_fr_gsd_alsi_v1",
                       trankit_model    = NULL,
                       trankit_category = "customized-mwt",
                       trankit_gpu      = FALSE,
                       n_cores          = 1,
                       chunk_size       = 10,
                       show_progress    = TRUE,
                       future_plan      = NULL,
                       parser           = "default",
                       reparse_copulas  = TRUE) {

  backend <- match.arg(backend)

  if (backend == "trankit") {
    return(.parse_text_trankit(
      txt              = txt,
      trankit_model    = trankit_model,
      trankit_category = trankit_category,
      trankit_gpu      = trankit_gpu,
      show_progress    = show_progress,
      chunk_size       = chunk_size
    ))
  }

  if (backend == "spacy") {
    return(.parse_text_spacy(
      txt           = txt,
      spacy_model   = spacy_model,
      show_progress = show_progress,
      chunk_size    = chunk_size
    ))
  }

  # ── UDPipe backend (original implementation) ─────────────────────────────────

  # Check if the model file exists
  if (!file.exists(ud_model)) {
    stop(paste("UDPipe model not found at:", ud_model))
  }
  
  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x))) {
        stop("parse_text | data.frame must contain columns: doc_id, text")
      }
      dt <- as.data.table(x[, c("doc_id", "text")])
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    if (is.character(x)) {
      ids <- if (!is.null(names(x))) names(x) else paste0("doc_", seq_along(x))
      dt  <- data.table(doc_id = ids, text = unname(x))
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
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
    parsed <- udpipe::udpipe(x = chunk, object = get(".udpipe_model", envir = .GlobalEnv), trace = FALSE, parser = parser)
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
  
  err_idx <- purrr::map_lgl(res, inherits, "parse_text_error")
  if (any(err_idx)) {
    msg <- paste(purrr::map_chr(res[err_idx], "error"), collapse = "\n- ")
    stop(paste0("parse_text | worker error(s):\n- ", msg))
  }

  result <- data.table::rbindlist(res, use.names = TRUE, fill = TRUE)

  if (reparse_copulas) {
    if (!exists(".udpipe_model", envir = .GlobalEnv)) {
      assign(".udpipe_model", udpipe::udpipe_load_model(file = ud_model), envir = .GlobalEnv)
    }
    result <- reparse_copular_etre(result, get(".udpipe_model", envir = .GlobalEnv))
  }

  result
}

# ── Shared Python setup ───────────────────────────────────────────────────────
#
# reticulate::py_require() can only set `python_version` (and the package
# list as a whole) BEFORE Python initializes; once a backend has triggered
# initialization (e.g. via source_python()), later calls may only `action =
# "add"` new packages — passing `python_version` again throws, even with an
# identical value. Since spaCy and Trankit can both run in the same session
# and Trankit needs Python 3.10, we declare the full combined requirement
# exactly once, on whichever backend initializes Python first.
.alsi_py_state <- new.env(parent = emptyenv())
.alsi_py_state$declared <- FALSE

.alsi_py_require <- function() {
  if (!isTRUE(.alsi_py_state$declared)) {
    reticulate::py_require(
      packages = c("spacy>=3.8.14,<3.9.0", "click",
                   "trankit==1.1.2", "adapters==1.0.0",
                   "transformers>=4.40,<4.44", "huggingface_hub<0.26",
                   "numpy<2", "syntok"),
      python_version = "3.10"
    )
    .alsi_py_state$declared <- TRUE
  }
  return(invisible(NULL))
}

# ── spaCy backend ─────────────────────────────────────────────────────────────

.parse_text_spacy <- function(txt, spacy_model, show_progress, chunk_size = 50L) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }

  .alsi_py_require()

  reticulate::source_python(
    system.file("py/spacy_backend.py", package = "ALSI", mustWork = FALSE) |>
      (\(p) if (nzchar(p)) p else here::here("py/spacy_backend.py"))()
  )

  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x)))
        stop("parse_text | data.frame must contain columns: doc_id, text")
      dt <- as.data.table(x[, c("doc_id", "text")])
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    if (is.character(x)) {
      ids <- if (!is.null(names(x))) names(x) else paste0("doc_", seq_along(x))
      dt  <- data.table(doc_id = ids, text = unname(x))
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    stop("parse_text | txt must be a data.frame with doc_id/text or a character vector")
  }

  txt_dt <- normalize_input(txt)

  if (!dir.exists(spacy_model)) {
    stop("parse_text | spaCy model not found at: ", spacy_model,
         "\nPlace the model folder under models/ or pass spacy_model = <path>.")
  }

  n <- nrow(txt_dt)
  if (show_progress)
    message("parse_text (spacy) | ", n, " text(s) to process")

  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  pb <- if (show_progress && n > 1L)
    utils::txtProgressBar(min = 0, max = n, style = 3)
  else
    NULL
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  rows <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    rows[[i]] <- reticulate::py$spacy_parse_texts(
      texts      = as.list(txt_dt$text[idx]),
      doc_ids    = as.list(txt_dt$doc_id[idx]),
      model_path = spacy_model
    )
    if (!is.null(pb)) utils::setTxtProgressBar(pb, idx[length(idx)])
  }
  rows <- unlist(rows, recursive = FALSE)

  if (length(rows) == 0L) return(data.table())

  dt <- data.table::rbindlist(lapply(rows, as.list), use.names = TRUE, fill = TRUE)

  dt[, token_id      := as.integer(token_id)]
  dt[, head_token_id := as.integer(head_token_id)]
  dt[, feats         := ifelse(feats == "NA", NA_character_, feats)]
  dt[, paragraph_id  := 1L]
  dt[, term_id       := .I]

  data.table::setcolorder(dt, intersect(
    c("doc_id", "paragraph_id", "sentence_id", "term_id",
      "token_id", "token", "lemma", "upos", "xpos", "feats",
      "head_token_id", "dep_rel"),
    names(dt)
  ))

  return(dt)
}

# ── Trankit backend ────────────────────────────────────────────────────────────

.trankit_ensure_model <- function(model_dir) {
  default <- "models/trankit_fr_v1"
  path <- if (!is.null(model_dir)) model_dir else default
  if (!dir.exists(path)) {
    stop(
      "parse_text | Trankit model not found at: ", path, "\n",
      "Place the trankit_fr_v1 folder under models/ or pass trankit_model = <path>."
    )
  }
  path
}

.parse_text_trankit <- function(txt, trankit_model, trankit_category,
                                trankit_gpu, show_progress,
                                chunk_size = 50L) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }

  .alsi_py_require()

  reticulate::source_python(
    system.file("py/trankit_backend.py", package = "ALSI", mustWork = FALSE) |>
      (\(p) if (nzchar(p)) p else here::here("py/trankit_backend.py"))()
  )

  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x)))
        stop("parse_text | data.frame must contain columns: doc_id, text")
      dt <- as.data.table(x[, c("doc_id", "text")])
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    if (is.character(x)) {
      ids <- if (!is.null(names(x))) names(x) else paste0("doc_", seq_along(x))
      dt  <- data.table(doc_id = ids, text = unname(x))
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    stop("parse_text | txt must be a data.frame with doc_id/text or a character vector")
  }

  txt_dt    <- normalize_input(txt)
  model_dir <- .trankit_ensure_model(trankit_model)
  n         <- nrow(txt_dt)

  # Warm up the model before the progress bar so loading messages appear first.
  reticulate::py$trankit_load(model_dir, trankit_category, trankit_gpu)

  if (show_progress)
    message("parse_text (trankit) | ", n, " text(s) to process")

  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  pb <- if (show_progress && n > 1L)
    utils::txtProgressBar(min = 0, max = n, style = 3)
  else
    NULL
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  rows <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    rows[[i]] <- reticulate::py$trankit_parse_texts(
      texts     = as.list(txt_dt$text[idx]),
      doc_ids   = as.list(txt_dt$doc_id[idx]),
      model_dir = model_dir,
      category  = trankit_category,
      gpu       = trankit_gpu
    )
    if (!is.null(pb)) utils::setTxtProgressBar(pb, idx[length(idx)])
  }
  rows <- unlist(rows, recursive = FALSE)

  if (length(rows) == 0L) return(data.table())

  dt <- data.table::rbindlist(lapply(rows, as.list), use.names = TRUE, fill = TRUE)

  dt[, token_id      := as.integer(token_id)]
  dt[, head_token_id := as.integer(head_token_id)]
  dt[, feats         := ifelse(feats == "NA", NA_character_, feats)]
  dt[, paragraph_id  := 1L]
  dt[, term_id       := .I]

  data.table::setcolorder(dt, intersect(
    c("doc_id", "paragraph_id", "sentence_id", "term_id",
      "token_id", "token", "lemma", "upos", "xpos", "feats",
      "head_token_id", "dep_rel"),
    names(dt)
  ))

  return(dt)
}

#' Reparse Sentences Containing Copular Être
#'
#' For sentences where \emph{être} is annotated with \code{dep_rel = "cop"},
#' replaces its lemma with \emph{paraitre} in the CoNLL-U input and re-runs the
#' parser only (tokenizer and tagger skipped). The parser then heads the copula
#' as the main predicate and attaches the nominal/adjectival predicate as
#' \code{xcomp}. Afterwards the original lemma is kept (only the dependency
#' structure changes).
#'
#' This is a no-op when the custom ALSI model is used, since that model already
#' outputs the correct structure. It is useful when running with the standard
#' French-GSD model, which still uses the \code{cop} relation.
#'
#' @param dt   A \code{data.table} of raw UDPipe output (from \code{parse_text}).
#' @param ud_model_obj A loaded UDPipe model object (\code{udpipe_load_model()}).
#' @returns A \code{data.table} with updated \code{head_token_id} and
#'   \code{dep_rel} for affected sentences.
reparse_copular_etre <- function(dt, ud_model_obj) {
  cop_keys <- unique(dt[dep_rel == "cop" & tolower(lemma) %in% c("être", "etre"),
                         .(doc_id, paragraph_id, sentence_id)])
  if (nrow(cop_keys) == 0L) return(dt)

  message("reparse_copular_etre | reparsing ", nrow(cop_keys), " sentence(s)")

  fna <- function(x) ifelse(is.na(x) | x == "", "_", as.character(x))

  # Build CoNLL-U blocks and track expected row counts per sentence
  blocks     <- character(nrow(cop_keys))
  row_counts <- integer(nrow(cop_keys))

  for (i in seq_len(nrow(cop_keys))) {
    k    <- cop_keys[i]
    rows <- dt[doc_id == k$doc_id & paragraph_id == k$paragraph_id &
                 sentence_id == k$sentence_id]

    lm <- fna(rows$lemma)
    lm[rows$dep_rel == "cop" & tolower(rows$lemma) %in% c("être", "etre")] <- "paraitre"

    token_lines <- paste(
      fna(rows$token_id), fna(rows$token), lm,
      fna(rows$upos), fna(rows$xpos), fna(rows$feats),
      fna(rows$head_token_id), fna(rows$dep_rel),
      "_", "_", sep = "\t"
    )
    blocks[i]     <- paste(c(token_lines, ""), collapse = "\n")
    row_counts[i] <- nrow(rows)
  }

  ann <- udpipe::udpipe_annotate(
    ud_model_obj,
    x         = paste(blocks, collapse = "\n"),
    tokenizer = "conllu",
    tagger    = "none",
    parser    = "default"
  )
  new_dt <- as.data.table(as.data.frame(ann))

  # Assign sentence index using expected row counts (robust to MWT rows)
  row_ends   <- cumsum(row_counts)
  row_starts <- c(1L, row_ends[-length(row_ends)] + 1L)
  new_dt[, .row := seq_len(.N)]
  new_dt[, .sent_idx := findInterval(.row, row_starts)]

  # Attach original keys
  cop_keys[, .sent_idx := .I]
  new_dt[cop_keys, on = ".sent_idx",
          `:=`(.orig_doc = i.doc_id, .orig_para = i.paragraph_id,
               .orig_sent = i.sentence_id)]

  # Collect updates for syntactic words only (no MWT / empty nodes)
  upd <- new_dt[!grepl("[-.]", token_id) & !is.na(.orig_doc),
                 .(doc_id     = .orig_doc,
                   paragraph_id = .orig_para,
                   sentence_id  = .orig_sent,
                   token_id,
                   new_head   = head_token_id,
                   new_deprel = dep_rel)]

  dt_out <- copy(dt)
  dt_out[upd, on = .(doc_id, paragraph_id, sentence_id, token_id),
          `:=`(head_token_id = i.new_head, dep_rel = i.new_deprel)]

  dt_out
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
  
  # Ensure copula UPOS is VERB (fallback for any remaining cop relations)
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


#' Segment texts into sentences with syntok (Python)
#'
#' A lightweight alternative to \code{parse_text()} when only sentence
#' boundaries are needed and installing a UDPipe model is undesirable.
#' Calls \code{py/segment_sentences.py} via \pkg{reticulate}.
#'
#' @param txt Either a \code{data.frame}/\code{data.table} with columns
#'   \code{doc_id} and \code{text}, or a character vector of texts.
#' @param min_sent_len Integer. Trailing sentences shorter than this many
#'   tokens are merged into the preceding sentence. Set to 0 to disable.
#'   Default 4.
#' @param merge_colon_semicolon Logical; if TRUE, sentences ending with
#'   \code{:} or \code{;} are merged with the following sentence when safe.
#'   Default FALSE.
#' @param max_prev_len,max_next_len,max_merged_len Token-length guards for
#'   the colon/semicolon merge heuristic. See \code{segment_sentences.py}.
#' @returns A \code{data.table} with columns \code{doc_id},
#'   \code{sentence_id} (restarts at 1 per document), and \code{sentence}.
segment_sentences_syntok <- function(txt,
                                     min_sent_len = 4L,
                                     merge_colon_semicolon = FALSE,
                                     max_prev_len = 35L,
                                     max_next_len = 35L,
                                     max_merged_len = 60L) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }
  .alsi_py_require()
  reticulate::source_python(
    system.file("py/segment_sentences.py", package = "ALSI",
                mustWork = FALSE) |>
      (\(p) if (nzchar(p)) p else "py/segment_sentences.py")()
  )

  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x))) {
        stop("segment_sentences_syntok | data.frame must have columns: doc_id, text")
      }
      dt <- as.data.table(x[, c("doc_id", "text")])
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    if (is.character(x)) {
      ids <- if (!is.null(names(x))) names(x) else paste0("doc_", seq_along(x))
      dt  <- data.table(doc_id = ids, text = unname(x))
      dups <- dt$doc_id[duplicated(dt$doc_id)]
      if (length(dups) > 0)
        stop("parse_text | duplicate doc_id values: ",
             paste(unique(dups), collapse = ", "),
             "\nEach document must have a unique doc_id.")
      return(dt)
    }
    stop("segment_sentences_syntok | txt must be a data.frame with doc_id/text or a character vector")
  }

  txt_dt <- normalize_input(txt)

  rows <- vector("list", nrow(txt_dt))
  for (i in seq_len(nrow(txt_dt))) {
    did  <- txt_dt$doc_id[i]
    text <- txt_dt$text[i]
    sents <- reticulate::py$segment_text_syntok(
      text,
      min_sent_len         = as.integer(min_sent_len),
      merge_colon_semicolon = merge_colon_semicolon,
      max_prev_len         = as.integer(max_prev_len),
      max_next_len         = as.integer(max_next_len),
      max_merged_len       = as.integer(max_merged_len)
    )
    if (length(sents) == 0L) next
    rows[[i]] <- data.table(
      doc_id      = did,
      sentence_id = seq_along(sents),
      sentence    = as.character(sents)
    )
  }

  return(data.table::rbindlist(rows, use.names = TRUE))
}


#' Read a wikiviki TSV dump into a long-format corpus table
#'
#' The wikiviki TSV has one article per row with columns
#' \code{doc_id_hard}, \code{hard_text}, \code{doc_id_easy}, \code{easy_text}.
#' This function reshapes it to a \code{data.table(doc_id, text)} suitable for
#' \code{parse_text()}.
#'
#' @param path Path to the TSV file.
#' @param which One of \code{"both"} (default), \code{"hard"}, or \code{"easy"}.
#' @returns A \code{data.table} with columns \code{doc_id} and \code{text}.
