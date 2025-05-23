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
    contenu <- str_replace_all(contenu, "’", "'")
    contenu <- str_replace_all(contenu, "''", "'")
    contenu <- str_replace_all(contenu, "«", " « ")
    contenu <- str_replace_all(contenu, "»", " » ")
    contenu <- str_replace_all(contenu, "!", "! ")
    contenu <- str_replace_all(contenu, ";", " ; ")
    contenu <- str_replace_all(contenu, ":", " : ")
    contenu <- gsub("\\.(?=[A-Za-zÀÉÈÙ0-9])", ". ", contenu, perl = TRUE)
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
  
  # Check if the model file exists
  if (!file.exists(ud_model)) {
    stop(paste("UDPipe model not found at:", ud_model))
  }
  
  # Try to load the model safely
  model <- tryCatch({
    udpipe::udpipe_load_model(file = ud_model)
  }, error = function(e) {
    stop("Failed to load UDPipe model: ", e$message)
  })
  
  parsed <- udpipe(x = txt, object = model, trace = TRUE, parallel.cores = nCores)
  parsed <- as.data.table(parsed)
  return(parsed)
}


#' Post treatment on the output of a udpipe parsing
#'
#' @param dt A data.table containing the output of a udpipe parsing.

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

