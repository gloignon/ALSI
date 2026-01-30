# This script contains functions for processing and analyzing text data.
library(data.table)
library(tidyverse)
library(utf8)
library(udpipe)

constituerCorpus <- function(dossier, verbose = FALSE, clean = TRUE) {
  if (dir.exists(dossier)) {
    chemins <- list.files(path = dossier, full.names = TRUE)
    if (verbose) message("constituerCorpus | C'est un dossier!")
  } else if (file_test("-f", dossier)) {
    chemins <- normalizePath(dossier)
  } else {
    stop("constituerCorpus | dossier ou fichier inexistant")
  }
  
  nettoyerTexte <- function(chemin) {
    if (verbose) message("working on ", chemin)
    contenu <- read_file(chemin)
    contenu <- utf8_normalize(contenu, map_quote = TRUE)
    
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
    doc_id = basename(chemins),
    text = if (clean) unlist(lapply(chemins, nettoyerTexte)) else unlist(lapply(chemins, function(p) {
      utf8_normalize(read_file(p), map_quote = TRUE)
    }))
  )
  
  return(dt_corpus)
}

parserTexte <- function(txt, ud_model = "models/french_gsd-remix_2.udpipe", nCores = 1) {
  parse_text(txt, ud_model = ud_model, n_cores = nCores, show_progress = TRUE)
}

# This functionn will parse text using the udpipe package.
# It can handle parallel processing.
parse_text <- function(txt, ud_model = "models/french_gsd-remix_2.udpipe", n_cores = 1, chunk_size = 10, show_progress = TRUE) {
  
  # Check if the model file exists
  if (!file.exists(ud_model)) {
    stop(paste("UDPipe model not found at:", ud_model))
  }
  
  normalize_input <- function(x) {
    if (is.data.frame(x)) {
      if (!all(c("doc_id", "text") %in% names(x))) {
        stop("parserTexte | data.frame must contain columns: doc_id, text")
      }
      return(as.data.table(x[, c("doc_id", "text")]))
    }
    if (is.character(x)) {
      return(data.table(doc_id = paste0("doc_", seq_along(x)), text = x))
    }
    stop("parserTexte | txt must be a data.frame with doc_id/text or a character vector")
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
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, { library(udpipe); library(data.table) })
  
  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(chunks), style = 3)
    on.exit(close(pb), add = TRUE)
  }
  
  n_tasks <- length(chunks)
  res <- vector("list", n_tasks)
  next_task <- 1
  n_workers <- length(cl)
  
  for (w in seq_len(min(n_workers, n_tasks))) {
    parallel:::sendCall(cl[[w]], worker_parse, list(chunks[[next_task]], ud_model), tag = next_task)
    next_task <- next_task + 1
  }
  
  completed <- 0
  while (completed < n_tasks) {
    ans <- parallel:::recvOneResult(cl)
    res[[ans$tag]] <- ans$value
    completed <- completed + 1
    if (show_progress) utils::setTxtProgressBar(pb, completed)
    if (next_task <= n_tasks) {
      parallel:::sendCall(cl[[ans$node]], worker_parse, list(chunks[[next_task]], ud_model), tag = next_task)
      next_task <- next_task + 1
    }
  }
  
  data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
}

#' Post treatment on the output of a udpipe parsing
#'
#' @param dt A data.table containing the output of a udpipe parsing.
#
#' @return A data.table containing the edited output of the udpipe parsing.
#' @import data.table
postTraitementLexique <- function(dt) {
  cat("Dans PostTraitement lexique\n")
  
  parsed.post <- setDT(copy(dt))
  
  cat("Class of parsed.post: ", class(parsed.post), "\n")

  
  # Add a unique token ID
  parsed.post[, vrai_token_id := 1:.N]
  
  # Clean document IDs
  parsed.post[, doc_id := str_remove_all(doc_id, "\\.txt$")]
  
  # Remove rows with missing tokens
  parsed.post <- parsed.post[!is.na(token)]
  
  # Handle double tokens introduced by parsing
  parsed.post[, estDoubleMot := is.na(head_token_id) & is.na(upos)]
  dt.intrus <- parsed.post[estDoubleMot == TRUE, .(
    doc_id, term_id = c(term_id + 1, term_id + 2)
  )][, estIntrus := TRUE]
  
  parsed.post$compte <- TRUE
  parsed.post <- merge(parsed.post, dt.intrus, all = TRUE)
  parsed.post[estIntrus == TRUE, compte := FALSE]
  parsed.post[, c("estIntrus", "estDoubleMot") := NULL]
  
  # Remove punctuation and particles from counts
  parsed.post[upos %in% c("PUNCT", "PART"), compte := FALSE]
  
  # Correct copula to VERB
  parsed.post[dep_rel == "cop", upos := "VERB"]
  
  # Remove duplicates
  parsed.post <- parsed.post[!duplicated(parsed.post[, .(doc_id, term_id)])]
  
  # Clean columns
  cols_to_remove <- c("start", "end", "xpos", "deps")
  parsed.post <- parsed.post[, .SD, .SDcols = setdiff(names(parsed.post), cols_to_remove)]
  
  # Reclassify relative pronouns as PRON
  parsed.post[upos == "ADV" & feats == "PronType=Rel", upos := "PRON"]
  
  # Normalize tokens and lemmas
  parsed.post[upos != "PROPN", `:=`(
    token = str_to_lower(token),
    lemma = str_to_lower(lemma)
  )]
  parsed.post[, `:=`(
    token = str_replace_all(token, "œ", "oe"),
    lemma = str_replace_all(lemma, "œ", "oe")
  )]
  parsed.post[!is.na(token) & str_starts(token, "['-]"), 
              token := str_sub(token, 2)]
  
  # Sort for consistent ordering
  parsed.post <- parsed.post[order(doc_id, paragraph_id, sentence_id, term_id)]
  
  # Add lowercase token column
  parsed.post[, lower_token := tolower(token)]
  
  return(parsed.post)
}
