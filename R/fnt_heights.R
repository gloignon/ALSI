# This file contains functions to compute sentence heights and head final/initial stats
# 
# loignon.guillaume@uqam.ca
#



# 
# New versions 2026-02 without igraph ------
# 

sentence_graph_stats <- function(dt_sentence, verbose = FALSE) {

  dt_sentence <- as.data.table(copy(dt_sentence))
  dt_sentence <- dt_sentence[!is.na(head_token_id) & !is.na(token_id) & upos != "PUNCT"]

  if (nrow(dt_sentence) == 0L) {
    return(list(
      max_path = 0,
      avg_path = 0,
      count_path = 0,
      sentence_length = 0,
      adj_max_path = 0
    ))
  }

  dt_sentence[, `:=`(
    token_id = as.integer(token_id),
    head_token_id = as.integer(head_token_id)
  )]
  dt_sentence <- dt_sentence[!is.na(token_id) & !is.na(head_token_id)]

  dt_sentence[, depth := fifelse(head_token_id == 0L, 0L, NA_integer_)]
  dt_sentence[, row_id := .I]

  for (i in seq_len(200L)) {
    missing_rows <- dt_sentence[is.na(depth), row_id]
    if (length(missing_rows) == 0L) break

    parents <- dt_sentence[!is.na(depth), .(token_id, parent_depth = depth)]
    children <- dt_sentence[is.na(depth), .(row_id, head_token_id)]

    joined <- parents[children, on = .(token_id = head_token_id)]
    new_depth <- joined$parent_depth + 1L
    fill <- children$row_id[!is.na(new_depth)]

    if (length(fill) == 0L) break

    dt_sentence[fill, depth := new_depth[!is.na(new_depth)]]
  }

  valid_depth <- dt_sentence[!is.na(depth), depth]

  if (length(valid_depth) == 0L) {
    return(list(
      max_path = 0,
      avg_path = 0,
      count_path = 0,
      sentence_length = 0,
      adj_max_path = 0
    ))
  }

  n <- length(valid_depth)
  max_path <- max(valid_depth)
  avg_path <- mean(valid_depth)
  count_path <- sum(valid_depth)
  adj_max_path <- if (n > 1L && max_path > 0L) log(max_path) / log(n) else 0

  if (verbose) {
    cat("Max path:", max_path, "\n")
    cat("Average path:", avg_path, "\n")
    cat("Sentence length:", n, "\n")
    cat("Adjusted max path:", adj_max_path, "\n")
  }

  list(
    max_path = max_path,
    avg_path = avg_path,
    count_path = count_path,
    sentence_length = n,
    adj_max_path = adj_max_path
  )
}

head_final_initial <- function(df) {

  df <- as.data.table(df)
  df[, temp_global_id := .I]

  df_copy <- copy(df)

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

  df_lost <- df_copy[!temp_global_id %in% df_valid$temp_global_id,
                     .(doc_id, paragraph_id, sentence_id, token_id, head_final = NA, head_initial = NA, temp_global_id)]

  df_result <- rbindlist(list(df_valid, df_lost), use.names = TRUE, fill = TRUE)
  setorder(df_result, temp_global_id)

  df_result[, .(doc_id, paragraph_id, sentence_id, token_id, head_final, head_initial)]
}

batch_graph_stats <- function(dt_corpus, verbose = FALSE) {

  dt <- as.data.table(copy(dt_corpus))
  dt <- dt[!is.na(head_token_id) & !is.na(token_id) & upos != "PUNCT"]

  if (nrow(dt) == 0L) {
    return(dt[, .(
      max_path = integer(),
      avg_path = numeric(),
      count_path = integer(),
      sentence_length = integer(),
      adj_max_path = numeric()
    ), by = .(doc_id, paragraph_id, sentence_id)])
  }

  dt[, `:=`(
    token_id = as.integer(token_id),
    head_token_id = as.integer(head_token_id)
  )]
  dt <- dt[!is.na(head_token_id) & !is.na(token_id)]

  dt[, sent_key := .GRP, by = .(doc_id, paragraph_id, sentence_id)]
  dt[, depth := fifelse(head_token_id == 0L, 0L, NA_integer_)]
  dt[, row_id := .I]

  for (i in seq_len(200L)) {
    missing_rows <- dt[is.na(depth), row_id]
    if (length(missing_rows) == 0L) break

    parents <- dt[!is.na(depth), .(sent_key, token_id, parent_depth = depth)]
    children <- dt[is.na(depth), .(row_id, sent_key, head_token_id)]

    joined <- parents[children, on = .(sent_key, token_id = head_token_id)]
    new_depth <- joined$parent_depth + 1L
    fill <- children$row_id[!is.na(new_depth)]

    if (length(fill) == 0L) break

    dt[fill, depth := new_depth[!is.na(new_depth)]]
  }

  if (verbose && any(is.na(dt$depth))) {
    warning(
      "batch_graph_stats: some tokens could not be assigned a depth; ",
      "check for disconnected or cyclic dependency structures."
    )
  }

  dt[!is.na(depth), {
    n <- .N
    mp <- max(depth)
    ap <- mean(depth)

    .(
      max_path = mp,
      avg_path = ap,
      count_path = sum(depth),
      sentence_length = n,
      adj_max_path = if (n > 1L && mp > 0L) log(mp) / log(n) else 0
    )
  }, by = .(doc_id, paragraph_id, sentence_id)]
}

docwise_graph_stats <- function(df_corpus) {

  df_corpus <- copy(df_corpus)

  message("Head final/initial...")
  df_head_final <- head_final_initial(df_corpus)
  sum_head_final <- df_head_final %>%
    group_by(doc_id) %>%
    summarise(
      prop_hf = sum(head_final, na.rm = TRUE) / n(),
      prop_hi = sum(head_initial, na.rm = TRUE) / n()
    )

  message("Heights...")
  df_heights <- batch_graph_stats(df_corpus, verbose = FALSE)

  sum_heights <- df_heights %>%
    group_by(doc_id) %>%
    summarise(
      avg_sent_height = mean(max_path, na.rm = TRUE),
      norm_sent_height = mean(adj_max_path, na.rm = TRUE),
      n = sum(sentence_length),
      s = n(),
      total_paths = sum(count_path, na.rm = TRUE)
    ) %>%
    mutate(
      avg_path = (1 / (n - s)) * total_paths
    )

  merge(sum_heights, sum_head_final, by = "doc_id")
}
