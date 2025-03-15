constituerCorpus <- function (dossier, verbose = FALSE) {
  #TODO: explorer une solution (avec quanteda?) qui permettrait aussi de prendre un corpus en .zip
  #dossier = "./textes/textes_items_peloquin" 
  if (dir.exists(dossier)) {  # dossier existe
    chemins <- list.files(path=dossier, full.names = TRUE) #liste des fichiers avec le "path"
    message("constituerCorpus | C'est un dossier!")
    n <- length(chemins) #le nombre de textes dans le dossier 
  } else {
    if (file_test("-f", dossier)) {  # si c'est finalement un fichier
      chemins <- paste(getwd(), dossier, sep="/")
      n <- 1
    } else {
      message("constituerCorpus | dossier ou fichier inexistant \n")
      return()
    }
  }
  
  dt_corpus <- data.table ( #initialisation
    doc_id = basename(chemins),
    text = ""
  )
  
  for (i in (1:n)) {
    if (verbose == TRUE) {message("working on ", chemins[i])}
    contenu <- read_file(chemins[i])
    contenu <- utf8_normalize(contenu, map_quote = TRUE)
    
    contenu <- gsub("\\\\", "", contenu) # correction pour les slash (SATO)
    
    contenu <- gsub("\r?\n(?!\r?\n)", " ", contenu, perl = T)  # remove unwanted line breaks (we replace by a space)
    contenu <- gsub("(\\r?\\n)\\s+", "\\1", contenu) # remove leading spaces at the beginning new lines
    
    contenu <- str_replace_all(contenu, "’", "'") # correction pour le type d'apostrophes
    contenu <- str_replace_all(contenu, "''", "'") # fix double apostrophe
    contenu <- str_replace_all(contenu, "«", " « ") # UDpipe est fussy pour les guillemets il aime un espace
    contenu <- str_replace_all(contenu, "»", " » ") # UDpipe est fussy pour les guillemets il aime un espace
    contenu <- str_replace_all(contenu, "!", "! ") #      
    contenu <- str_replace_all(contenu, ";", " ; ") #
    contenu <- str_replace_all(contenu, ":", " : ") #
    
    contenu <- gsub("\\.(?=[A-Za-z1-9ÀÉÇ-])", ". ", contenu, perl = TRUE)   # fix point sans espace
    contenu <- gsub("\\,(?=[A-Za-z1-9])", ", ", contenu, perl = TRUE)  # fix virgule sans espace
    contenu <- gsub("\\)(?=[A-Za-z1-9])", ") ", contenu, perl = TRUE)  # fix par fermante suivi d'une lettre ou chiffre
    
    # contenu <- str_replace_all(contenu, "   ", " ") # triple espace
    # contenu <- str_replace_all(contenu, "  ", " ") # double espace
    contenu <- str_remove_all(contenu, fixed("**")) # retrait des marques ** dans les documents SATO
    contenu <- str_remove_all(contenu, fixed("**")) # retrait des marques ** dans les documents SATO
    
    contenu <- str_replace_all(contenu, "   ", " ") # retrait triple espace
    contenu <- str_replace_all(contenu, "  ", " ") # retrait double espace
    
    
    #gsub("/’", "/'", contenu, ignore.case = TRUE)
    dt_corpus[i, "text"] <- contenu
  }
  return (dt_corpus)
}

parserTexte <- function(txt, nCores = 1) {
  #txt <- str_replace_all(txt, "’", "'") #correction pour les apostrophes
  #note : on fait maintenant ce remplacement dans une autre fonction, ConstituerCorpus
  parsed <- udpipe(x = txt, object = udmodel_french, trace = TRUE, parallel.cores = nCores)
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
