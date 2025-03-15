# This file contains functions to compute sentence heights and head final/initial stats
# 
# loignon.guillaume@uqam.ca
#

library(data.table)
library(igraph)

# nouvelle méthode, printemps 2022
# beaucoup plus rapide
sentenceHeight <- function(dt, verbose = TRUE) {
  
  # if dt is not data.table, make it so
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  
  dt_corpus_temp <- copy(dt[!upos %in% c("PUNCT", NA)])
  # for each combination of doc_id and sentence_id....
  dt_corpus_temp[, idPhrase := paste(doc_id, paragraph_id, sentence_id)]
  
  climbATree <- function(phrase) {
    # j'initialise la liste avec les mots qui dépendent directement du root
    root_id <- phrase[head_token_id == 0, token_id ]
    if(length(root_id) == 0) { # root pas trouvé
      return (1)
    }
    tree_heigth <- 1
    phrase[, estParent := token_id %in% phrase$head_token_id]
    liste_check <- phrase[head_token_id == root_id, .(token_id, estParent, head_token_id) ]
    height_found <- FALSE
    
    while(nrow(liste_check > 0)) {
      tree_heigth <- tree_heigth + 1
      # je limite la liste aux mots qui ont effectivement des descendants
      liste_check <- liste_check[estParent == TRUE]
      
      # je remplis une liste de mots dont je veux vérifier la descendance
      liste_check <- phrase[head_token_id %in% liste_check$token_id, .(token_id, estParent, head_token_id) ]
    }
    
    return(tree_heigth)
  }
  
  df_depths <- data.frame(idPhrase = unique(dt_corpus_temp$idPhrase), sentenceDepth = NA, sentence_id = NA, doc_id = NA)
  for (id in unique(dt_corpus_temp$idPhrase)) {
    cur_phrase <- dt_corpus_temp[idPhrase == id] 
    # if cur_phrase is the first of its doc_id, message the doc_id
    if (verbose) {
      if (cur_phrase$sentence_id[1] == 1) {
        message("Calcul de la hauteur des phrases pour le document ", cur_phrase$doc_id[1])
      }
    }
    # write to results
    df_depths[df_depths$idPhrase == id, "sentenceDepth"] <- climbATree(cur_phrase)
    df_depths[df_depths$idPhrase == id, "sentence_id"] <- cur_phrase$sentence_id[1]
    df_depths[df_depths$idPhrase == id, "doc_id"] <- cur_phrase$doc_id[1]
  }
  
  return(df_depths)
}

# new code using igraph
sentence_graph_stats <- function(dt_sentence, verbose = FALSE) {
  
  # if there is a doc_id column, check there is a single value
  if ("doc_id" %in% names(dt_sentence)) {
    if (length(unique(dt_sentence$doc_id)) > 1) {
      stop("The data.table must contain a single doc_id.")
    }
  }
  
  # if there is a sentence_id column, check there is a single value
  if ("sentence_id" %in% names(dt_sentence)) {
    if (length(unique(dt_sentence$sentence_id)) > 1) {
      stop("The data.table must contain a single sentence_id.")
    }
  }
  
  # keep what we need only
  dt_sentence <- dt_sentence[, .(token_id, head_token_id)]
  
  # Remove head_token_id = 0 (root indicator)
  edges <- subset(dt_sentence, head_token_id != 0)
  
  # Create the dependency graph
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Max path length (diameter)
  max_path <- diameter(g) 
  
  # Average path length
  avg_path <- mean_distance(g, directed = TRUE)
  
  # Total number of paths
  count_path <- vcount(g) * avg_path
  
  # Number of nodes (i.e., sentence length)
  sentence_length <- vcount(g)
  
  # Log adjusted max path length
  adj_max_path <- log(max_path) / log(sentence_length)
  
  if (verbose) {
    cat("Max path:", max_path, "\n")
    cat("Average path:", avg_path, "\n")
    cat("Sentence length:", sentence_length, "\n")
    cat("Adjusted max path": adj_max_path)
  }
  
  return(list(
    max_path = max_path,
    avg_path = avg_path,
    count_path = count_path,
    sentence_length = sentence_length,
    adj_max_path = adj_max_path
  ))
}

# Head final or head initial tokens
head_final_initial <- function(df) {
  
  df$temp_global_id <- 1:nrow(df)
  
  df_copy <- df
  
  # remove punctuation and tokens without a token_id or head_token_id
  df <- df %>% filter(!is.na(token_id) & !is.na(head_token_id) & 
                        !(upos == "PUNCT"))
  
  df$token_id <- as.numeric(df$token_id)
  df$head_token_id <- as.numeric(df$head_token_id)
  
  df <- df %>% group_by(doc_id, paragraph_id, sentence_id) %>% 
    mutate(head_final = if_else(head_token_id > token_id, TRUE, FALSE, missing = NA),
           head_initial = if_else(head_token_id < token_id, TRUE, FALSE, missing = NA))
  
  # find rows in df_copy that are not in df and add them back to df
  df_lost <- df_copy %>% 
    anti_join(df, by = "temp_global_id")
  
  # token_id and head_token_id back to char
  df$token_id <- as.character(df$token_id)
  df$head_token_id <- as.character(df$head_token_id)
  
  # add back, and sort
  df <- rbind(df, df_lost) %>% arrange(temp_global_id)
  
  # select what we need only for the output
  df_result <- df %>%
    select(doc_id, paragraph_id, sentence_id, token_id, head_final, head_initial)
  return(df_result)
}

# Will accept a whole corpus and batch process the sentences
# Batch processing function
batch_graph_stats <- function(dt, verbose = FALSE) {
  
  # If data is not data.table, convert it
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  
  # Clean token list
  dt <- dt[!is.na(head_token_id) & !is.na(token_id) & !(upos == "PUNCT")]
  
  # Make a unique sentence identifier
  dt[, unique_sentence_id := paste(doc_id, paragraph_id, sentence_id, sep = "___")]
  
  # Vector of unique sentence identifiers
  v_sentences <- unique(dt$unique_sentence_id)
  n_sentence <- length(v_sentences)
  
  # Initialize a list to store results
  results <- vector("list", n_sentence)
  
  # Loop through each sentence
  for (i in seq_len(n_sentence)) {
    # Get the current sentence
    sentence <- dt[unique_sentence_id == v_sentences[i]]
    
    # Get sentence identifiers
    doc_id <- sentence$doc_id[1]
    paragraph_id <- sentence$paragraph_id[1]
    sentence_id <- sentence$sentence_id[1]
    
    # Get the sentence graph stats
    stats <- sentence_graph_stats(dt_sentence = sentence, verbose = verbose)
    
    # Add the stats to the results list
    results[[i]] <- data.table(
      doc_id = doc_id,
      paragraph_id = paragraph_id,
      sentence_id = sentence_id,
      max_path = stats$max_path,
      avg_path = stats$avg_path,
      count_path = stats$count_path,
      sentence_length = stats$sentence_length,
      adj_max_path = stats$adj_max_path
    )
  }
  
  # Combine list of results into a single data.table
  results <- rbindlist(results, use.names = TRUE, fill = TRUE)
  
  return(results)
}

# Will accept a whole corpus and batch process the sentences
# computing height & height final stats
docwise_graph_stats <- function(df_corpus) {
  
  df_heights <- batch_graph_stats(df_corpus, verbose = FALSE)
  
  sum_heights <- df_heights %>% 
    group_by(doc_id) %>% 
    summarise(
      avg_sent_height = mean(max_path, na.rm = T),
      norm_sent_height = mean(adj_max_path, na.rm = T),
      n = sum(sentence_length),
      s = n(),
      total_paths = sum(count_path, na.rm = T)
    ) %>% 
    mutate(
      avg_path = (1 / (n - s)) * total_paths
    )
  
  df_head_final <- head_final_initial(df_corpus)
  sum_head_final <- df_head_final %>% 
    group_by(doc_id) %>% 
    summarise(
      prop_hf = sum(head_final, na.rm = T) / n(),
      prop_hi = sum(head_initial, na.rm = T) / n()
    )  
  
  # merge 
  df_result <- merge(sum_heights, sum_head_final, by = "doc_id")
  
  return(df_result)
}

