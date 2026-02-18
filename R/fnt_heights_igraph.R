library(data.table)
library(igraph)

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


head_final_initial <- function(df) {
  
  df <- as.data.table(df)
  df[, temp_global_id := .I]  # Preserve original row order
  
  df_copy <- copy(df)
  
  # Filter valid rows and compute booleans
  df_valid <- df[!is.na(token_id) & !is.na(head_token_id) & upos != "PUNCT", {
    token_id_num <- as.numeric(token_id)
    head_token_id_num <- as.numeric(head_token_id)
    
    list(
      doc_id = doc_id,
      paragraph_id = paragraph_id,
      sentence_id = sentence_id,
      token_id = token_id,
      head_final = head_token_id_num > token_id_num,
      head_initial = head_token_id_num < token_id_num,
      temp_global_id = temp_global_id
    )
  }]
  
  # Rows that were excluded in filtering
  df_lost <- df_copy[!temp_global_id %in% df_valid$temp_global_id, 
                     .(doc_id, paragraph_id, sentence_id, token_id, head_final = NA, head_initial = NA, temp_global_id)]
  
  # Combine and restore original order
  df_result <- rbindlist(list(df_valid, df_lost), use.names = TRUE, fill = TRUE)
  setorder(df_result, temp_global_id)
  
  # Return selected columns
  df_result[, .(doc_id, paragraph_id, sentence_id, token_id, head_final, head_initial)]
}


# This function will batch process sentence height/depth calculations
# Uses data.table for efficient processing.
# Note: in current version, we let data.table print progress messages even
# if verbose = FALSE
batch_graph_stats <- function(dt_corpus, verbose = FALSE) {
  
  dt <- as.data.table(copy(dt_corpus))  # ensure dt is a data.table and safely copied
  
  # Filter and create sentence ID
  dt <- dt[!is.na(head_token_id) & !is.na(token_id) & upos != "PUNCT"]
  dt[, unique_sentence_id := paste(doc_id, paragraph_id, sentence_id, sep = "___")]
  
  # Apply sentence_graph_stats by sentence using data.table's grouping
  results <- dt[, {
    stats <- sentence_graph_stats(.SD, verbose = verbose)
    .(max_path = stats$max_path,
      avg_path = stats$avg_path,
      count_path = stats$count_path,
      sentence_length = stats$sentence_length,
      adj_max_path = stats$adj_max_path)
  }, by = .(doc_id, paragraph_id, sentence_id)]
  
  return(results)
}

# Will accept a whole corpus and batch process the sentences
# computing height & height final stats
docwise_graph_stats <- function(df_corpus) {
  
  df_corpus <- copy(df_corpus)
  
  message("Head final/initial...")
  df_head_final <- head_final_initial(df_corpus)
  sum_head_final <- df_head_final %>% 
    group_by(doc_id) %>% 
    summarise(
      prop_hf = sum(head_final, na.rm = T) / n(),
      prop_hi = sum(head_initial, na.rm = T) / n()
    )  
  
  message("Heights...")
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
  
  # merge 
  df_result <- merge(sum_heights, sum_head_final, by = "doc_id")
  
  return(df_result)
}