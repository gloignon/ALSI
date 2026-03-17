# This file contains functions to compute sentence heights and head final/initial stats
#
# loignon.guillaume@uqam.ca
#



#
# New versions 2026-02 without igraph ------
#

# Plot dependency arcs for a parsed sentence.
# Input: a data.frame/data.table from udpipe (one sentence).
# Draws words along the x-axis and arcs above them connecting each token to its head.
# Crossing arcs are highlighted in red.
plot_dependency_arcs <- function(dt_sentence, title = NULL, show_deprel = TRUE,
                                highlight_crossings = TRUE, cex_text = 0.8) {

  dt <- as.data.table(copy(dt_sentence))
  dt[, token_id_num := to_integer_quiet(token_id)]
  dt[, head_token_id_num := to_integer_quiet(head_token_id)]
  dt <- dt[!is.na(token_id_num) & !is.na(head_token_id_num)]

  if (nrow(dt) == 0L) {
    message("No valid arcs to plot.")
    return(invisible(NULL))
  }

  n <- nrow(dt)
  tokens <- dt$token
  tok_ids <- dt$token_id_num
  head_ids <- dt$head_token_id_num

  # Build arcs (skip root arcs where head == 0)
  arcs <- dt[head_token_id_num > 0L, .(from = token_id_num, to = head_token_id_num, deprel = dep_rel)]
  arcs[, lo := pmin(from, to)]
  arcs[, hi := pmax(from, to)]
  # Arc height proportional to span width (avoid overlap)
  arcs[, height := (hi - lo)]

  # Detect crossing pairs
  arc_is_crossing <- rep(FALSE, nrow(arcs))
  if (highlight_crossings && nrow(arcs) > 1L) {
    for (i in seq_len(nrow(arcs) - 1L)) {
      for (j in (i + 1L):nrow(arcs)) {
        if ((arcs$lo[i] < arcs$lo[j] & arcs$lo[j] < arcs$hi[i] & arcs$hi[i] < arcs$hi[j]) |
            (arcs$lo[j] < arcs$lo[i] & arcs$lo[i] < arcs$hi[j] & arcs$hi[j] < arcs$hi[i])) {
          arc_is_crossing[i] <- TRUE
          arc_is_crossing[j] <- TRUE
        }
      }
    }
  }

  # Scale heights so arcs don't get too tall
  max_span <- max(arcs$height, 1L)
  arc_scaled_height <- arcs$height / max_span

  # Plot setup
  x_range <- range(tok_ids)
  y_max <- 1.3
  par(mar = c(3, 0.5, 2, 0.5))
  plot(NULL, xlim = c(x_range[1] - 0.5, x_range[2] + 0.5),
       ylim = c(-0.3, y_max),
       xlab = "", ylab = "", axes = FALSE,
       main = if (!is.null(title)) title else "Dependency arcs")

  # Draw tokens along x-axis
  text(tok_ids, rep(-0.05, n), tokens, cex = cex_text, srt = 45, adj = c(1, 1))
  # Token id below
  text(tok_ids, rep(-0.22, n), tok_ids, cex = cex_text * 0.7, col = "grey50")
  points(tok_ids, rep(0, n), pch = 16, cex = 0.5)

  # Draw arcs
  for (k in seq_len(nrow(arcs))) {
    from_x <- arcs$from[k]
    to_x <- arcs$to[k]
    h <- arc_scaled_height[k] * 0.9
    col <- if (arc_is_crossing[k]) "red" else "steelblue"
    lwd <- if (arc_is_crossing[k]) 2.5 else 1.5

    # Draw a semicircular arc
    mid <- (from_x + to_x) / 2
    radius <- abs(to_x - from_x) / 2
    theta <- seq(0, pi, length.out = 50)
    arc_x <- mid + radius * cos(theta)
    arc_y <- h * sin(theta)
    lines(arc_x, arc_y, col = col, lwd = lwd)

    # Arrow at the dependent end (from = dependent, to = head)
    arrows(arc_x[49], arc_y[49], to_x, 0, length = 0.08, col = col, lwd = lwd)

    # Dependency relation label
    if (show_deprel && !is.na(arcs$deprel[k])) {
      text(mid, max(arc_y) + 0.02, arcs$deprel[k], cex = cex_text * 0.6, col = col)
    }
  }

  # Mark root token
  root_ids <- dt[head_token_id_num == 0L, token_id_num]
  if (length(root_ids) > 0L) {
    points(root_ids, rep(0, length(root_ids)), pch = 17, cex = 1.2, col = "darkgreen")
    text(root_ids, 0.05, "ROOT", cex = cex_text * 0.7, col = "darkgreen")
  }

  invisible(NULL)
}

to_integer_quiet <- function(x) {
  x_chr <- as.character(x)
  out <- rep.int(NA_integer_, length(x_chr))
  ok <- grepl("^-?[0-9]+$", x_chr)
  out[ok] <- as.integer(x_chr[ok])
  out
}

# computes the height of a single sentence using a data.table approach.
# include_punct_in_metrics = TRUE by default: punctuation tokens participate
# in the dependency tree and contribute meaningful depth information.
sentence_graph_stats <- function(dt_sentence, verbose = FALSE, include_punct_in_metrics = TRUE) {

  dt_sentence <- as.data.table(copy(dt_sentence))
  dt_sentence <- dt_sentence[!is.na(head_token_id) & !is.na(token_id)]

  if (nrow(dt_sentence) == 0L) {
    return(list(
      max_path = 0,
      avg_dependency_depth = 0,
      avg_dependency_depth_adj = 0,
      sd_depth = 0,
      branching_factor = 0,
      count_path = 0,
      sentence_length = 0,
      max_incomplete_deps = 0L,
      avg_incomplete_deps = 0,
      max_incomplete_deps_adj = 0,
      avg_incomplete_deps_adj = 0
    ))
  }

  dt_sentence[, `:=`(
    token_id = to_integer_quiet(token_id),
    head_token_id = to_integer_quiet(head_token_id)
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

  metric_rows <- if (isTRUE(include_punct_in_metrics)) {
    !is.na(dt_sentence$depth)
  } else {
    !is.na(dt_sentence$depth) & !is.na(dt_sentence$upos) & dt_sentence$upos != "PUNCT"
  }
  valid_depth <- dt_sentence[metric_rows, depth]

  if (length(valid_depth) == 0L) {
    return(list(
      max_path = 0,
      avg_dependency_depth = 0,
      avg_dependency_depth_adj = 0,
      sd_depth = 0,
      branching_factor = 0,
      count_path = 0,
      sentence_length = 0,
      max_incomplete_deps = 0L,
      avg_incomplete_deps = 0,
      max_incomplete_deps_adj = 0,
      avg_incomplete_deps_adj = 0
    ))
  }

  n <- length(valid_depth)
  max_path <- max(valid_depth)
  count_path <- sum(valid_depth)
  avg_dependency_depth <- mean(valid_depth)
  avg_dependency_depth_adj <- if (n > 1L) count_path / (n - 1L) else 0
  sd_depth <- if (n > 1L) sd(valid_depth) else 0

  # Branching factor: average number of dependents per internal (non-leaf) node
  metric_tokens <- dt_sentence[metric_rows]
  arcs <- metric_tokens[head_token_id > 0L & token_id %in% metric_tokens$token_id]
  if (nrow(arcs) > 0L) {
    dep_per_head <- arcs[, .N, by = head_token_id]
    branching_factor <- mean(dep_per_head$N)
  } else {
    branching_factor <- 0
  }

  # Gibson DLT incomplete dependency count
  m_tok <- metric_tokens$token_id
  m_head <- metric_tokens$head_token_id
  tok_sorted <- sort(m_tok)
  head_for_tok <- m_head[order(m_tok)]
  hf_idx <- which(head_for_tok > tok_sorted)
  if (length(hf_idx) > 0L) {
    open_at <- tok_sorted[hf_idx]
    close_at <- head_for_tok[hf_idx]
    inc_counts <- vapply(tok_sorted, function(pos) {
      sum(open_at <= pos & close_at > pos)
    }, integer(1L))
    max_incomplete_deps <- max(inc_counts)
    avg_incomplete_deps <- mean(inc_counts)
  } else {
    max_incomplete_deps <- 0L
    avg_incomplete_deps <- 0
  }

  if (verbose) {
    cat("Max path:", max_path, "\n")
    cat("Average dependency depth:", avg_dependency_depth, "\n")
    cat("Adjusted average dependency depth:", avg_dependency_depth_adj, "\n")
    cat("Depth variance (sd):", sd_depth, "\n")
    cat("Branching factor:", branching_factor, "\n")
    cat("Sentence length:", n, "\n")
    cat("Max incomplete dependencies (Gibson DLT):", max_incomplete_deps, "\n")
    cat("Max incomplete deps (adjusted):", round(max_incomplete_deps / pmax(n, 1L), 3), "\n")
    cat("Avg incomplete dependencies:", round(avg_incomplete_deps, 2), "\n")
  }

  list(
    max_path = max_path,
    avg_dependency_depth = avg_dependency_depth,
    avg_dependency_depth_adj = avg_dependency_depth_adj,
    sd_depth = sd_depth,
    branching_factor = branching_factor,
    count_path = count_path,
    sentence_length = n,
    max_incomplete_deps = max_incomplete_deps,
    avg_incomplete_deps = avg_incomplete_deps,
    max_incomplete_deps_adj = max_incomplete_deps / pmax(n, 1L),
    avg_incomplete_deps_adj = avg_incomplete_deps / pmax(n, 1L)
  )
}

# proportion of head final/initial tokens for one document
head_final_initial_doc <- function(df_doc, include_punct_in_metrics = FALSE) {

  dt <- as.data.table(copy(df_doc))

  if (nrow(dt) == 0L) {
    return(data.table(
      doc_id = character(),
      paragraph_id = integer(),
      sentence_id = integer(),
      token_id = integer(),
      head_final = logical(),
      head_initial = logical(),
      head_distance = integer(),
      head_direction = integer(),
      head_distance_adj = numeric()
    ))
  }

  dt[, `:=`(
    token_id_num = to_integer_quiet(token_id),
    head_token_id_num = to_integer_quiet(head_token_id),
    head_final = as.logical(NA),
    head_initial = as.logical(NA),
    head_distance = as.integer(NA),
    head_direction = as.integer(NA),
    head_distance_adj = as.numeric(NA)
  )]

  dt[, sent_len := if (isTRUE(include_punct_in_metrics)) {
      sum(!is.na(token_id_num))
    } else {
      sum(!is.na(token_id_num) & upos != "PUNCT")
    },
    by = .(paragraph_id, sentence_id)]

  valid <- if (isTRUE(include_punct_in_metrics)) {
    !is.na(dt$token_id_num) & !is.na(dt$head_token_id_num)
  } else {
    !is.na(dt$token_id_num) & !is.na(dt$head_token_id_num) & dt$upos != "PUNCT"
  }
  valid_dependency <- valid & dt$head_token_id_num > 0L

  dt[valid, `:=`(
    head_final = head_token_id_num > token_id_num,
    head_initial = head_token_id_num < token_id_num
  )]

  dt[valid_dependency, head_distance := abs(head_token_id_num - token_id_num)]

  dt[valid_dependency, head_direction := fifelse(
    head_token_id_num > token_id_num, 1L,
    fifelse(head_token_id_num < token_id_num, -1L, 0L)
  )]

  dt[valid_dependency, head_distance_adj := head_distance / pmax(sent_len - 1L, 1L)]

  dt[, .(doc_id, paragraph_id, sentence_id, token_id, head_final, head_initial, head_distance, head_direction, head_distance_adj)]
}

# safer wrapper: process one document at a time
head_final_initial <- function(df, include_punct_in_metrics = FALSE) {

  dt <- as.data.table(copy(df))

  if (nrow(dt) == 0L) {
    return(data.table(
      doc_id = character(),
      paragraph_id = integer(),
      sentence_id = integer(),
      token_id = integer(),
      head_final = logical(),
      head_initial = logical(),
      head_distance = integer(),
      head_direction = integer(),
      head_distance_adj = numeric()
    ))
  }

  if (!"doc_id" %in% names(dt)) {
    stop("head_final_initial: missing required column 'doc_id'")
  }

  doc_ids <- unique(dt$doc_id)

  rbindlist(lapply(doc_ids, function(id) {
    head_final_initial_doc(
      dt[doc_id == id],
      include_punct_in_metrics = include_punct_in_metrics
    )
  }), use.names = TRUE)
}

# Will process sentence heights in batch.
# include_punct_in_metrics = TRUE by default: punctuation tokens participate
# in the dependency tree and contribute meaningful depth information.
batch_graph_stats <- function(dt_corpus, verbose = FALSE, include_punct_in_metrics = TRUE) {

  dt <- as.data.table(copy(dt_corpus))
  dt <- dt[!is.na(head_token_id) & !is.na(token_id)]

  if (nrow(dt) == 0L) {
    return(dt[, .(
      max_path = integer(),
      avg_dependency_depth = numeric(),
      avg_dependency_depth_adj = numeric(),
      sd_depth = numeric(),
      branching_factor = numeric(),
      count_path = integer(),
      sentence_length = integer(),
      max_incomplete_deps = integer(),
      avg_incomplete_deps = numeric(),
      max_incomplete_deps_adj = numeric(),
      avg_incomplete_deps_adj = numeric()
    ), by = .(doc_id, paragraph_id, sentence_id)])
  }

  dt[, `:=`(
    token_id = to_integer_quiet(token_id),
    head_token_id = to_integer_quiet(head_token_id)
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
    bad_sentences <- unique(dt[is.na(depth), .(doc_id, sentence_id)])
    bad_labels <- paste0("(", bad_sentences$doc_id, ", ", bad_sentences$sentence_id, ")")
    warning(
      "batch_graph_stats: some tokens could not be assigned a depth; ",
      "check for disconnected or cyclic dependency structures. ",
      "Affected (doc_id, sentence_id): ",
      paste(bad_labels, collapse = "; ")
    )
  }

  dt[, {
    metric_depth <- if (isTRUE(include_punct_in_metrics)) {
      depth[!is.na(depth)]
    } else {
      depth[!is.na(depth) & !is.na(upos) & upos != "PUNCT"]
    }
    n <- length(metric_depth)

    if (n == 0L) {
      .(
        max_path = 0L,
        avg_dependency_depth = 0,
        avg_dependency_depth_adj = 0,
        sd_depth = 0,
        branching_factor = 0,
        count_path = 0L,
        sentence_length = 0L,
        max_incomplete_deps = 0L,
        avg_incomplete_deps = 0,
        max_incomplete_deps_adj = 0,
        avg_incomplete_deps_adj = 0
      )
    } else {
      mp <- max(metric_depth)
      cp <- sum(metric_depth)
      ap <- mean(metric_depth)

      # Metric token mask (same filter as metric_depth)
      m_mask <- if (isTRUE(include_punct_in_metrics)) {
        !is.na(depth)
      } else {
        !is.na(depth) & !is.na(upos) & upos != "PUNCT"
      }
      m_tok <- token_id[m_mask]
      m_head <- head_token_id[m_mask]

      # Branching factor: average dependents per internal node
      m_arcs <- m_head[m_head > 0L]
      if (length(m_arcs) > 0L) {
        dep_counts <- tabulate(match(m_arcs, m_tok))
        bf <- mean(dep_counts[dep_counts > 0L])
      } else {
        bf <- 0
      }

      # Gibson DLT incomplete dependency count:
      # sweep left-to-right; at each position count how many dependencies
      # are still unresolved (dependent seen but head not yet reached).
      tok_sorted <- sort(m_tok)
      head_for_tok <- m_head[order(m_tok)]
      # head-final arcs (head comes later) create incomplete deps
      hf_idx <- which(head_for_tok > tok_sorted)
      if (length(hf_idx) > 0L) {
        open_at <- tok_sorted[hf_idx]   # dep opens at this position
        close_at <- head_for_tok[hf_idx] # dep resolves at this position
        # count open deps at each token position
        inc_counts <- vapply(tok_sorted, function(pos) {
          sum(open_at <= pos & close_at > pos)
        }, integer(1L))
        max_inc <- max(inc_counts)
        avg_inc <- mean(inc_counts)
      } else {
        max_inc <- 0L
        avg_inc <- 0
      }

      .(
        max_path = mp,
        avg_dependency_depth = ap,
        avg_dependency_depth_adj = if (n > 1L) cp / (n - 1L) else 0,
        sd_depth = if (n > 1L) sd(metric_depth) else 0,
        branching_factor = bf,
        count_path = cp,
        sentence_length = n,
        max_incomplete_deps = max_inc,
        avg_incomplete_deps = avg_inc,
        max_incomplete_deps_adj = max_inc / pmax(n, 1L),
        avg_incomplete_deps_adj = avg_inc / pmax(n, 1L)
      )
    }
  }, by = .(doc_id, paragraph_id, sentence_id)]
}

# Aggregate dependency features at the document level.
# Uses the empirically optimal punctuation setting for each feature group:
# - Tree height/depth: PUNCT included (punctuation arcs add tree structure)
# - Head direction/distance: PUNCT excluded (punctuation arcs add noise)
docwise_graph_stats <- function(df_corpus) {

  df_corpus <- copy(df_corpus)

  # Head direction/distance: exclude punctuation (less noise)
  message("Head final/initial...")
  df_head_final <- head_final_initial(
    df_corpus,
    include_punct_in_metrics = FALSE
  )
  sum_head_final <- df_head_final %>%
    group_by(doc_id) %>%
    summarise(
      prop_hf = sum(head_final, na.rm = TRUE) / n(),
      prop_hi = sum(head_initial, na.rm = TRUE) / n(),
      avg_head_distance = mean(head_distance, na.rm = TRUE),
      avg_head_distance_adj = mean(head_distance_adj, na.rm = TRUE),
      max_head_distance = if (all(is.na(head_distance))) NA_integer_ else max(head_distance, na.rm = TRUE),
      max_head_distance_adj = if (all(is.na(head_distance_adj))) NA_real_ else max(head_distance_adj, na.rm = TRUE),
      dependency_direction_index = {
        n_dep <- sum(!is.na(head_direction))
        if (n_dep > 0L) sum(head_direction, na.rm = TRUE) / n_dep else NA_real_
      }
    )

  # Tree height/depth: include punctuation (meaningful tree structure)
  message("Heights...")
  df_heights <- batch_graph_stats(
    df_corpus,
    verbose = FALSE,
    include_punct_in_metrics = TRUE
  )

  sum_heights <- df_heights %>%
    group_by(doc_id) %>%
    summarise(
      avg_sent_height = mean(max_path, na.rm = TRUE),
      avg_sent_height_adj = mean(max_path / pmax(sentence_length - 1L, 1L), na.rm = TRUE),
      sd_sent_height = sd(max_path, na.rm = TRUE),
      avg_dependency_depth_adj = mean(avg_dependency_depth_adj, na.rm = TRUE),
      avg_sd_depth = mean(sd_depth, na.rm = TRUE),
      avg_branching_factor = mean(branching_factor, na.rm = TRUE),
      avg_max_incomplete_deps = mean(max_incomplete_deps, na.rm = TRUE),
      avg_max_incomplete_deps_adj = mean(max_incomplete_deps_adj, na.rm = TRUE),
      avg_incomplete_deps = mean(avg_incomplete_deps, na.rm = TRUE),
      avg_incomplete_deps_adj = mean(avg_incomplete_deps_adj, na.rm = TRUE),
      n = sum(sentence_length),
      s = n(),
      total_paths = sum(count_path, na.rm = TRUE)
    ) %>%
    mutate(
      avg_dependency_depth = (1 / (n - s)) * total_paths
    )

  df_result <- merge(sum_heights, sum_head_final, by = "doc_id")
  return(df_result)
}

# Apply saved GAM calibration to sentence-level features.
# Subtracts the expected value for each sentence length, returning residuals
# that are (approximately) uncorrelated with sentence length.
#
# dt_sent: output of batch_graph_stats()
# gam_models: named list of GAM fits (from models/gam_sent_length_calibration.Rds)
#             or a path to the .Rds file.
# suffix: appended to calibrated column names (default "_resid")
apply_calibration <- function(dt_sent, gam_models, suffix = "_resid") {

  if (is.character(gam_models) && length(gam_models) == 1L) {
    gam_models <- readRDS(gam_models)
  }

  dt_out <- copy(dt_sent)
  newdata <- data.frame(sentence_length = dt_out$sentence_length)

  for (feat in names(gam_models)) {
    if (!feat %in% names(dt_out)) {
      warning("apply_calibration: column '", feat, "' not found in input; skipping.")
      next
    }
    predicted <- predict(gam_models[[feat]], newdata = newdata)
    dt_out[[paste0(feat, suffix)]] <- dt_out[[feat]] - predicted
  }

  dt_out
}
