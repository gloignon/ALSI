# This file contains functions to compute sentence heights and head final/initial stats
#
# loignon.guillaume@uqam.ca
#



#
# New versions 2026-02 without igraph ------
#

#' Plot dependency arcs for a parsed sentence
#'
#' Draws words along the x-axis and semicircular arcs connecting each token
#' to its head. Crossing arcs are highlighted in red.
#'
#' @param dt_sentence A data.table/data.frame from UDPipe (one sentence).
#' @param title Optional plot title.
#' @param show_deprel Logical; if TRUE, labels arcs with dependency relations.
#' @param highlight_crossings Logical; if TRUE, crossing arcs are drawn in red.
#' @param cex_text Text size multiplier.
#' @returns Invisible NULL; called for its side effect (plot).
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

#' Quietly convert to integer
#'
#' Converts character to integer, returning NA for non-integer strings
#' (e.g., multi-word token ranges like "30-31"). Avoids coercion warnings.
#'
#' @param x A character or numeric vector.
#' @returns An integer vector.
#' @keywords internal
to_integer_quiet <- function(x) {
  x_chr <- as.character(x)
  out <- rep.int(NA_integer_, length(x_chr))
  ok <- grepl("^-?[0-9]+$", x_chr)
  out[ok] <- as.integer(x_chr[ok])
  out
}

#' Compute dependency tree statistics for a single sentence
#'
#' Calculates max path depth, mean/adjusted dependency depth, depth SD,
#' branching factor, Yngve depth, and Gibson DLT incomplete dependency counts.
#'
#' @details
#' Pseudocode for each statistic (\code{depth(w)} = path length from token
#' \code{w} to the sentence root, root depth = 0; \code{n} = number of
#' metric-eligible tokens in the sentence):
#' \preformatted{
#' max_path              = max(depth(w) for w in tokens)
#' avg_dependency_depth   = mean(depth(w) for w in tokens)
#' avg_dependency_depth_adj = sum(depth(w) for w in tokens) / (n - 1)
#' sd_depth               = sd(depth(w) for w in tokens)
#' branching_factor       = mean(count_dependents(w) for w in tokens)
#'   (leaves contribute 0; this is node out-degree, averaged over every
#'   token, not just heads that have at least one dependent)
#' yngve(w)                = 0                                  if w is root
#'                          = right_siblings(w) + yngve(parent(w))   otherwise
#'   where right_siblings(w) = number of co-dependents of the same head
#'   that appear to the right of w in linear order
#' avg_incomplete_deps, max_incomplete_deps: Gibson (1998) SPLT memory cost
#'   (Formula 10). Sweep tokens left to right; a dependency is "open"
#'   (predicted) between the position of a dependent and the (later)
#'   position of its head. At each position, each open dependency
#'   contributes M(n) = n, where n = count of new discourse referents
#'   (upos in NOUN/PROPN/VERB) processed since that dependency opened;
#'   sum the contributions of all open dependencies at that position.
#'   avg/max_incomplete_deps = mean/max of this per-position total across
#'   the sentence. (Same discourse-referent mechanism as
#'   \code{head_final_initial_doc}'s \code{integration_cost}.)
#' }
#'
#' @citation_type "computationally identical" (\code{avg_dependency_depth} -- Chen
#'   et al. 2024's \code{depthMean}, mean depth over all metric tokens including
#'   the root, matched exactly at this sentence level; \code{docwise_graph_stats}
#'   pools this correctly via \code{total_paths / n});
#'   "adapted" (\code{max_path}, \code{branching_factor} -- formula matches Chen
#'   et al. 2024's \code{Height}/\code{degreeMean} exactly when punctuation policy
#'   aligns, but the default \code{include_punct_in_metrics = TRUE} here includes
#'   punctuation while Chen's processing excludes it;
#'   \code{sd_depth} -- same underlying statistic as \code{depthVar}, reported as
#'   SD (sqrt of variance) rather than variance;
#'   \code{avg_dependency_depth_adj} -- length-normalized \code{depthMean}, no
#'   normalized analogue in Chen et al. 2024;
#'   \code{avg_incomplete_deps}/\code{max_incomplete_deps} -- Gibson 1998 SPLT
#'   memory cost, fixed UD-upos set approximates his discourse-referent criterion;
#'   \code{yngve} -- Yngve 1960's branch-numbering/depth-sum formula, applied to
#'   dependency trees instead of his original phrase-structure trees)
#'
#' @param dt_sentence A data.table for one sentence with \code{token_id},
#'   \code{head_token_id}, and optionally \code{upos}.
#' @param verbose Logical; if TRUE, prints stats to console.
#' @param include_punct_in_metrics Logical; if TRUE (default), punctuation
#'   tokens participate in metrics.
#' @returns A named list of sentence-level statistics.
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
      max_incomplete_deps = 0,
      avg_incomplete_deps = 0,
      max_incomplete_deps_adj = 0,
      avg_incomplete_deps_adj = 0,
      yngve = 0,
      max_yngve = 0L,
      sd_yngve = 0
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

  # Yngve depth: right_siblings(w) + Yngve(parent(w)), root = 0.
  # right_siblings(w) = co-dependents of same head that appear to the right of w.
  dt_sentence[head_token_id == 0L, right_siblings := 0L]
  dt_sentence[head_token_id > 0L, right_siblings := {
    k <- .N
    k - frank(token_id, ties.method = "first")
  }, by = head_token_id]
  dt_sentence[is.na(right_siblings), right_siblings := 0L]

  dt_sentence[, yngve := fifelse(head_token_id == 0L, 0L, NA_integer_)]
  for (i in seq_len(200L)) {
    missing_rows <- dt_sentence[is.na(yngve), row_id]
    if (length(missing_rows) == 0L) break

    parents <- dt_sentence[!is.na(yngve), .(token_id, parent_yngve = yngve)]
    children <- dt_sentence[is.na(yngve), .(row_id, head_token_id, right_siblings)]

    joined <- parents[children, on = .(token_id = head_token_id)]
    new_yngve <- joined$parent_yngve + children$right_siblings
    fill <- children$row_id[!is.na(joined$parent_yngve)]
    nv <- new_yngve[!is.na(joined$parent_yngve)]

    if (length(fill) == 0L) break
    dt_sentence[fill, yngve := nv]
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
      max_incomplete_deps = 0,
      avg_incomplete_deps = 0,
      max_incomplete_deps_adj = 0,
      avg_incomplete_deps_adj = 0,
      yngve = 0,
      max_yngve = 0L,
      sd_yngve = 0
    ))
  }

  n <- length(valid_depth)
  max_path <- max(valid_depth)
  count_path <- sum(valid_depth)
  avg_dependency_depth <- mean(valid_depth)
  avg_dependency_depth_adj <- if (n > 1L) count_path / (n - 1L) else 0
  sd_depth <- if (n > 1L) sd(valid_depth) else 0

  valid_yngve <- dt_sentence[metric_rows & !is.na(yngve), yngve]
  yngve_score <- if (length(valid_yngve) > 0L) mean(valid_yngve) else 0
  max_yngve   <- if (length(valid_yngve) > 0L) max(valid_yngve) else 0L
  sd_yngve    <- if (length(valid_yngve) > 1L) sd(valid_yngve) else 0

  # Branching factor: mean node degree (dependent count) over ALL metric
  # tokens, including leaves with degree 0 (Chen et al. 2024 degreeMean)
  metric_tokens <- dt_sentence[metric_rows]
  arcs <- metric_tokens[head_token_id > 0L & token_id %in% metric_tokens$token_id]
  dep_per_head <- arcs[, .N, by = head_token_id]
  degree <- rep(0L, nrow(metric_tokens))
  match_idx <- match(metric_tokens$token_id, dep_per_head$head_token_id)
  degree[!is.na(match_idx)] <- dep_per_head$N[match_idx[!is.na(match_idx)]]
  branching_factor <- if (length(degree) > 0L) mean(degree) else 0

  # Gibson DLT memory cost (Formula 10): sweep tokens left to right; a
  # dependency is "open" (predicted) between the position of a dependent
  # and the later position of its head. Each open dependency contributes
  # M(n) = n, where n = count of new discourse referents (NOUN/PROPN/VERB)
  # processed since it opened; sum contributions of all open dependencies
  # at each position.
  discourse_upos <- c("NOUN", "PROPN", "VERB")
  m_tok <- metric_tokens$token_id
  m_head <- metric_tokens$head_token_id
  m_upos <- metric_tokens$upos
  ord <- order(m_tok)
  tok_sorted <- m_tok[ord]
  head_for_tok <- m_head[ord]
  cumref_sorted <- cumsum(m_upos[ord] %in% discourse_upos)
  hf_idx <- which(head_for_tok > tok_sorted)
  if (length(hf_idx) > 0L) {
    open_at <- tok_sorted[hf_idx]
    close_at <- head_for_tok[hf_idx]
    cumref_open <- cumref_sorted[hf_idx]
    open_mat <- outer(tok_sorted, open_at, ">=") & outer(tok_sorted, close_at, "<")
    weight_mat <- outer(cumref_sorted, cumref_open, "-") * open_mat
    inc_counts <- rowSums(weight_mat)
    max_incomplete_deps <- max(inc_counts)
    avg_incomplete_deps <- mean(inc_counts)
  } else {
    max_incomplete_deps <- 0
    avg_incomplete_deps <- 0
  }

  if (verbose) {
    message("Max path: ", max_path)
    message("Average dependency depth: ", avg_dependency_depth)
    message("Adjusted average dependency depth: ", avg_dependency_depth_adj)
    message("Depth variance (sd): ", sd_depth)
    message("Branching factor: ", branching_factor)
    message("Sentence length: ", n)
    message("Max incomplete dependencies (Gibson DLT): ", max_incomplete_deps)
    message("Max incomplete deps (adjusted): ", round(max_incomplete_deps / pmax(n, 1L), 3))
    message("Avg incomplete dependencies: ", round(avg_incomplete_deps, 2))
    message("Yngve depth (mean): ", round(yngve_score, 3))
    message("Yngve depth (max):  ", max_yngve)
    message("Yngve depth (sd):   ", round(sd_yngve, 3))
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
    avg_incomplete_deps_adj = avg_incomplete_deps / pmax(n, 1L),
    yngve = yngve_score,
    max_yngve = max_yngve,
    sd_yngve = sd_yngve
  )
}

#' Compute head-final/initial stats for one document
#'
#' For each token, determines whether its head is to the right (head-final)
#' or left (head-initial), computes head distance, direction, and Gibson DLT
#' integration cost.
#'
#' @details
#' Pseudocode per token \code{w} with head \code{h}. \code{head_final},
#' \code{head_initial}, and \code{head_direction} compare raw token indices
#' (removing punctuation never changes the relative order of two tokens, so
#' raw indices are sufficient there). \code{head_distance} instead uses
#' \code{dep_pos}, a position renumbered sequentially over only the tokens
#' counted in \code{sent_len} (all tokens if \code{include_punct_in_metrics =
#' TRUE}, non-punctuation tokens otherwise); this keeps the distance on the
#' same token scale as \code{sent_len}, avoiding gaps from punctuation tokens
#' falling between a dependent and its head. Only applies to tokens with a
#' real head (root tokens, whose head is a sentinel 0, are excluded -- a root
#' has no dependency relation):
#' \preformatted{
#' head_final        = position(h) > position(w)
#' head_initial       = position(h) < position(w)
#' head_distance      = abs(dep_pos(h) - dep_pos(w))
#' head_direction     = +1 if head_final, -1 if head_initial, 0 if h == w
#' integration_cost   = count of tokens strictly between w and h (exclusive)
#'   whose upos is in {NOUN, PROPN, VERB}  (Gibson 1998 integration cost:
#'   new discourse referents intervening between dependent and head)
#' }
#'
#' @citation_type "computationally identical" (\code{head_distance} -- Liu 2008 MDD;
#'   \code{head_final}/\code{head_initial}/\code{head_direction} -- Liu 2010 head directionality);
#'   "adapted" (\code{integration_cost} -- Gibson 1998, our discourse-referent set is a fixed
#'   UD-upos approximation of his original referent-introduction criterion)
#'
#' @param df_doc A data.table for one document with \code{doc_id},
#'   \code{paragraph_id}, \code{sentence_id}, \code{token_id},
#'   \code{head_token_id}, and \code{upos}.
#' @param include_punct_in_metrics Logical; if FALSE (default), punctuation
#'   is excluded.
#' @returns A token-level \code{data.table} with \code{dep_pos} (punctuation-
#'   renumbered token position), \code{head_final}, \code{head_initial},
#'   \code{head_distance}, \code{head_direction}, and \code{integration_cost}.
head_final_initial_doc <- function(df_doc, include_punct_in_metrics = FALSE) {

  dt <- as.data.table(copy(df_doc))

  if (nrow(dt) == 0L) {
    return(data.table(
      doc_id = character(),
      paragraph_id = integer(),
      sentence_id = integer(),
      token_id = integer(),
      token_id_num = integer(),
      head_token_id_num = integer(),
      sent_len = integer(),
      dep_pos = integer(),
      head_final = logical(),
      head_initial = logical(),
      head_distance = integer(),
      head_direction = integer(),
      integration_cost = integer()
    ))
  }

  dt[, `:=`(
    token_id_num = to_integer_quiet(token_id),
    head_token_id_num = to_integer_quiet(head_token_id),
    dep_pos = as.integer(NA),
    head_final = as.logical(NA),
    head_initial = as.logical(NA),
    head_distance = as.integer(NA),
    head_direction = as.integer(NA),
    integration_cost = as.integer(NA)
  )]

  dt[, sent_len := if (isTRUE(include_punct_in_metrics)) {
      sum(!is.na(token_id_num))
    } else {
      sum(!is.na(token_id_num) & upos != "PUNCT")
    },
    by = .(paragraph_id, sentence_id)]

  # dep_pos: sequential position among only the tokens counted in sent_len
  # (i.e. punctuation-renumbered when include_punct_in_metrics = FALSE), so
  # that head_distance lives on the same token scale as sent_len -- raw
  # token_id_num would still count punctuation gaps even though punctuation
  # tokens themselves are excluded from sent_len.
  pos_rows <- if (isTRUE(include_punct_in_metrics)) {
    !is.na(dt$token_id_num)
  } else {
    !is.na(dt$token_id_num) & dt$upos != "PUNCT"
  }
  dt[pos_rows, dep_pos := frank(token_id_num, ties.method = "first"),
     by = .(paragraph_id, sentence_id)]

  valid <- if (isTRUE(include_punct_in_metrics)) {
    !is.na(dt$token_id_num) & !is.na(dt$head_token_id_num)
  } else {
    !is.na(dt$token_id_num) & !is.na(dt$head_token_id_num) & dt$upos != "PUNCT"
  }
  valid_dependency <- valid & dt$head_token_id_num > 0L

  dt[valid_dependency, `:=`(
    head_final = head_token_id_num > token_id_num,
    head_initial = head_token_id_num < token_id_num
  )]

  pos_lookup <- dt[!is.na(dep_pos), .(paragraph_id, sentence_id, token_id_num, dep_pos)]
  dt[pos_lookup, on = .(paragraph_id, sentence_id, head_token_id_num = token_id_num),
     head_pos := i.dep_pos]

  dt[valid_dependency, head_distance := abs(head_pos - dep_pos)]

  dt[valid_dependency, head_direction := fifelse(
    head_token_id_num > token_id_num, 1L,
    fifelse(head_token_id_num < token_id_num, -1L, 0L)
  )]

  # Gibson DLT integration cost: count of new discourse referents (NOUN, PROPN, VERB)
  # intervening between dependent and head (excluding endpoints)
  discourse_upos <- c("NOUN", "PROPN", "VERB")
  dt[valid_dependency, integration_cost := {
    lo <- pmin(token_id_num, head_token_id_num) + 1L
    hi <- pmax(token_id_num, head_token_id_num) - 1L
    mapply(
      function(l, h, pid, sid) {
        if (l > h) return(0L)
        sum(dt$paragraph_id == pid & dt$sentence_id == sid &
              !is.na(dt$token_id_num) & dt$token_id_num >= l & dt$token_id_num <= h &
              dt$upos %in% discourse_upos, na.rm = TRUE)
      },
      lo, hi, paragraph_id, sentence_id
    )
  }]

  dt[, .(doc_id, paragraph_id, sentence_id, token_id, token_id_num, head_token_id_num,
         sent_len, dep_pos, head_final, head_initial, head_distance, head_direction,
         integration_cost)]
}

#' Compute head-final/initial stats for a corpus
#'
#' Wrapper that applies \code{head_final_initial_doc} to each document
#' and combines results.
#'
#' @param df A parsed \code{data.table} with a \code{doc_id} column.
#' @param include_punct_in_metrics Logical; if FALSE (default), punctuation
#'   is excluded from metrics.
#' @returns A combined token-level \code{data.table}.
head_final_initial <- function(df, include_punct_in_metrics = FALSE) {

  dt <- as.data.table(copy(df))

  if (nrow(dt) == 0L) {
    return(data.table(
      doc_id = character(),
      paragraph_id = integer(),
      sentence_id = integer(),
      token_id = integer(),
      token_id_num = integer(),
      head_token_id_num = integer(),
      sent_len = integer(),
      dep_pos = integer(),
      head_final = logical(),
      head_initial = logical(),
      head_distance = integer(),
      head_direction = integer(),
      integration_cost = integer()
    ))
  }

  if (!"doc_id" %in% names(dt)) {
    stop("head_final_initial: missing required column 'doc_id'")
  }

  doc_ids <- unique(dt$doc_id)

  rbindlist(
    lapply(doc_ids, function(id) {
      head_final_initial_doc(dt[doc_id == id], include_punct_in_metrics)
    }),
    use.names = TRUE
  )
}

#' Batch-compute sentence-level dependency tree statistics
#'
#' Vectorised version of \code{sentence_graph_stats} for an entire corpus,
#' using data.table grouping for efficiency.
#'
#' @details
#' Computes the same statistics as \code{sentence_graph_stats} (see that
#' function's \code{@details} for the pseudocode and citation backing of
#' \code{max_path}, \code{avg_dependency_depth}, \code{avg_dependency_depth_adj},
#' \code{sd_depth}, \code{branching_factor}, \code{yngve}/\code{max_yngve}/\code{sd_yngve},
#' and the Gibson incomplete-dependency counts), one row per sentence instead
#' of one call per sentence.
#'
#' @param dt_corpus A parsed \code{data.table} with \code{doc_id},
#'   \code{paragraph_id}, \code{sentence_id}, \code{token_id},
#'   \code{head_token_id}, and optionally \code{upos}.
#' @param verbose Logical; if TRUE, warns about unresolvable depths.
#' @param include_punct_in_metrics Logical; if TRUE (default), punctuation
#'   tokens participate in metrics.
#' @returns A \code{data.table} with one row per sentence and columns for
#'   each tree statistic.
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
      avg_incomplete_deps_adj = numeric(),
      yngve = numeric(),
      max_yngve = integer(),
      sd_yngve = numeric()
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

  # Yngve depth: right_siblings(w) + Yngve(parent(w)), root = 0.
  # right_siblings(w) = co-dependents of same head that appear to the right of w.
  dt[head_token_id == 0L, right_siblings := 0L]
  dt[head_token_id > 0L, right_siblings := {
    k <- .N
    k - frank(token_id, ties.method = "first")
  }, by = .(sent_key, head_token_id)]
  dt[is.na(right_siblings), right_siblings := 0L]

  dt[, yngve := fifelse(head_token_id == 0L, 0L, NA_integer_)]
  for (i in seq_len(200L)) {
    missing_rows <- dt[is.na(yngve), row_id]
    if (length(missing_rows) == 0L) break

    parents <- dt[!is.na(yngve), .(sent_key, token_id, parent_yngve = yngve)]
    children <- dt[is.na(yngve), .(row_id, sent_key, head_token_id, right_siblings)]

    joined <- parents[children, on = .(sent_key, token_id = head_token_id)]
    new_yngve <- joined$parent_yngve + children$right_siblings
    fill <- children$row_id[!is.na(joined$parent_yngve)]
    nv <- new_yngve[!is.na(joined$parent_yngve)]

    if (length(fill) == 0L) break
    dt[fill, yngve := nv]
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
        max_incomplete_deps = 0,
        avg_incomplete_deps = 0,
        max_incomplete_deps_adj = 0,
        avg_incomplete_deps_adj = 0,
        yngve = 0,
        max_yngve = 0L,
        sd_yngve = 0
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
      m_upos <- upos[m_mask]

      # Branching factor: mean node degree over ALL metric tokens, including
      # leaves with degree 0 (Chen et al. 2024 degreeMean)
      m_arcs <- m_head[m_head > 0L]
      degree <- tabulate(match(m_arcs, m_tok), nbins = length(m_tok))
      bf <- mean(degree)

      # Gibson DLT memory cost (Formula 10): sweep left-to-right; each open
      # (head-final) dependency contributes M(n) = n, where n = count of
      # new discourse referents (NOUN/PROPN/VERB) processed since it opened;
      # sum contributions of all open dependencies at each position.
      discourse_upos <- c("NOUN", "PROPN", "VERB")
      ord <- order(m_tok)
      tok_sorted <- m_tok[ord]
      head_for_tok <- m_head[ord]
      cumref_sorted <- cumsum(m_upos[ord] %in% discourse_upos)
      hf_idx <- which(head_for_tok > tok_sorted)
      if (length(hf_idx) > 0L) {
        open_at <- tok_sorted[hf_idx]   # dep opens at this position
        close_at <- head_for_tok[hf_idx] # dep resolves at this position
        cumref_open <- cumref_sorted[hf_idx]
        open_mat <- outer(tok_sorted, open_at, ">=") &
          outer(tok_sorted, close_at, "<")
        weight_mat <- outer(cumref_sorted, cumref_open, "-") * open_mat
        inc_counts <- rowSums(weight_mat)
        max_inc <- max(inc_counts)
        avg_inc <- mean(inc_counts)
      } else {
        max_inc <- 0
        avg_inc <- 0
      }

      valid_yngve <- yngve[m_mask & !is.na(yngve)]
      vy <- if (length(valid_yngve) > 0L) mean(valid_yngve) else 0
      mxy <- if (length(valid_yngve) > 0L) max(valid_yngve) else 0L
      sdy <- if (length(valid_yngve) > 1L) sd(valid_yngve) else 0

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
        avg_incomplete_deps_adj = avg_inc / pmax(n, 1L),
        yngve = vy,
        max_yngve = mxy,
        sd_yngve = sdy
      )
    }
  }, by = .(doc_id, paragraph_id, sentence_id)]
}

#' Aggregate dependency features at the document level
#'
#' Combines tree height/depth stats (with punctuation) and head
#' direction/distance stats (without punctuation) into a single
#' document-level summary.
#'
#' @details
#' Document-level aggregation pseudocode (sentence-level inputs come from
#' \code{batch_graph_stats} and \code{head_final_initial}; \code{n}, \code{s},
#' and \code{total_paths} are retained aggregation helpers, not independent
#' features):
#' \preformatted{
#' avg_sent_height        = mean(max_path)                 across sentences
#' sd_sent_height          = sd(max_path)                   across sentences
#' avg_sd_depth            = mean(sd_depth)                 across sentences
#' avg_branching_factor    = mean(branching_factor)         across sentences
#' avg_max_incomplete_deps = mean(max_incomplete_deps)      across sentences
#' avg_incomplete_deps     = mean(avg_incomplete_deps)      across sentences
#' avg_integration_cost    = mean(integration_cost)         over real dependencies
#' n            = sum(sentence_length)
#' s            = count(sentences)
#' total_paths  = sum(count_path)
#' avg_dependency_depth = total_paths / n   # sentence-length-weighted pooled depth,
#'   matches Chen et al. 2024's depthMean (mean depth over all nodes including
#'   roots); count_path/sentence_length already include the root's depth (0)
#'   in both numerator and denominator at the sentence level, so pooling must
#'   divide by n (total nodes), not n - s
#'
#' # avg_head_distance_adj: Normalized Dependency Distance (Lei & Jockers 2020),
#' # per sentence, then averaged across sentences. root_position = dep_pos of
#' # the root token (head_token_id_num == 0), i.e. its position renumbered
#' # over punctuation-excluded tokens only, matching the sentence_length scale;
#' # mdd = mean(head_distance) within the sentence, also on dep_pos terms;
#' # sentence_length = punctuation-excluded token count.
#' avg_head_distance_adj = mean(abs(log(mdd / sqrt(root_position * sentence_length))))
#'
#' # prop_hf/prop_hi: percentage of head-final/head-initial dependencies
#' # (Liu 2010, Formulae in Sec. 2), denominator is real dependencies only
#' # (root and punctuation excluded, matching "total number of dependencies
#' # in the treebank"); every real dependency is exactly one of the two, so
#' # prop_hf + prop_hi == 1.
#' prop_hf = count(head_final) / count(non-NA head_final)
#' prop_hi = count(head_initial) / count(non-NA head_initial)
#' avg_head_distance = mean(head_distance)
#' max_head_distance = max(head_distance)
#' avg_yngve         = mean(yngve)      across sentences
#' avg_max_yngve     = mean(max_yngve)  across sentences
#' avg_sd_yngve      = mean(sd_yngve)   across sentences
#' }
#'
#' @citation_type "adapted" (\code{avg_sent_height}, \code{avg_branching_factor} --
#'   formula matches Chen et al. 2024's per-sentence \code{Height}/\code{degreeMean}
#'   exactly when punctuation policy aligns, but ALSI includes punctuation by
#'   default while Chen's processing excludes it;
#'   \code{avg_sd_depth} -- same underlying statistic as \code{depthVar}, reported
#'   as SD (sqrt of variance) rather than variance;
#'   \code{avg_max_incomplete_deps} -- Gibson 1998 Formula 10 peak memory cost,
#'   with a fixed UD-upos set approximating his discourse-referent criterion;
#'   \code{avg_incomplete_deps} -- same Formula 10 cost, using ALSI's secondary
#'   mean aggregation instead of Gibson's peak predictor;
#'   \code{avg_integration_cost} -- Gibson 1998 Formula 9 integration cost,
#'   with the same fixed UD-upos approximation of discourse referents;
#'   \code{avg_yngve}, \code{avg_max_yngve}, \code{avg_sd_yngve} -- Yngve 1960's
#'   branch-numbering/depth-sum formula, applied to dependency trees instead of
#'   his original phrase-structure trees);
#'   "computationally identical" (\code{avg_dependency_depth} -- \code{total_paths / n},
#'   the sentence-length-weighted pool of the per-sentence \code{depthMean}
#'   (mean depth over all nodes including roots), matching Chen et al. 2024;
#'   \code{prop_hf}, \code{prop_hi} -- Liu 2010;
#'   \code{avg_head_distance} -- Liu 2008 Formula 2, sample-level MDD;
#'   \code{avg_head_distance_adj} -- Lei & Jockers 2020's Normalized Dependency
#'   Distance, NDD, applied per sentence then averaged across the document);
#'   "inspired" (\code{max_head_distance} -- Liu 2008 discusses maximum dependency
#'   distance per sentence as a candidate complexity measure but explicitly rejects
#'   it as unstable in favor of MDD; he never defines a formal max-DD statistic, so
#'   this is motivated by his discussion, not a formula match);
#'   "derived" (\code{sd_sent_height} -- cross-sentence variability of tree height;
#'   not the same aggregation level as any Chen et al. 2024 statistic, ALSI-original)
#'
#' @param df_corpus A parsed \code{data.table} (full corpus).
#' @returns A \code{data.table} with one row per document and columns for
#'   all dependency-tree features.
docwise_graph_stats <- function(df_corpus) {

  df_corpus <- copy(df_corpus)

  # Head direction/distance: exclude punctuation (less noise)
  message("Head final/initial...")
  df_head_final <- head_final_initial(
    df_corpus,
    include_punct_in_metrics = FALSE
  )
  sum_head_final <- df_head_final |>
    group_by(doc_id) |>
    summarise(
      prop_hf = sum(head_final, na.rm = TRUE) / sum(!is.na(head_final)),
      prop_hi = sum(head_initial, na.rm = TRUE) / sum(!is.na(head_initial)),
      avg_head_distance = mean(head_distance, na.rm = TRUE),
      max_head_distance = if (all(is.na(head_distance))) NA_integer_ else max(head_distance, na.rm = TRUE),
      avg_integration_cost = mean(integration_cost, na.rm = TRUE)
    )

  # Normalized Dependency Distance (Lei & Jockers 2020): per-sentence MDD
  # normalized by root-token position and sentence length, replaces the
  # naive head_distance / (sent_len - 1) division (which does not actually
  # decorrelate from sentence length -- verified empirically on the demo
  # corpus, see FEATURES_CITATIONS.yaml).
  sum_ndd <- df_head_final |>
    group_by(doc_id, paragraph_id, sentence_id) |>
    summarise(
      mdd = mean(head_distance, na.rm = TRUE),
      sent_length = dplyr::first(sent_len),
      root_position = dplyr::first(dep_pos[head_token_id_num == 0L]),
      .groups = "drop"
    ) |>
    mutate(
      ndd = abs(log(mdd / sqrt(root_position * sent_length)))
    ) |>
    group_by(doc_id) |>
    summarise(avg_head_distance_adj = mean(ndd, na.rm = TRUE))

  sum_head_final <- merge(sum_head_final, sum_ndd, by = "doc_id")

  # Tree height/depth: include punctuation (meaningful tree structure)
  message("Heights...")
  df_heights <- batch_graph_stats(
    df_corpus,
    verbose = FALSE,
    include_punct_in_metrics = TRUE
  )

  sum_heights <- df_heights |>
    group_by(doc_id) |>
    summarise(
      avg_sent_height = mean(max_path, na.rm = TRUE),
      sd_sent_height = sd(max_path, na.rm = TRUE),
      avg_sd_depth = mean(sd_depth, na.rm = TRUE),
      avg_branching_factor = mean(branching_factor, na.rm = TRUE),
      avg_max_incomplete_deps = mean(max_incomplete_deps, na.rm = TRUE),
      avg_incomplete_deps = mean(avg_incomplete_deps, na.rm = TRUE),
      avg_yngve = mean(yngve, na.rm = TRUE),
      avg_max_yngve = mean(max_yngve, na.rm = TRUE),
      avg_sd_yngve = mean(sd_yngve, na.rm = TRUE),
      n = sum(sentence_length),
      s = n(),
      total_paths = sum(count_path, na.rm = TRUE)
    ) |>
    mutate(
      avg_dependency_depth = total_paths / n
    )

  df_result <- merge(sum_heights, sum_head_final, by = "doc_id")
  return(df_result)
}

#' [EXPERIMENTAL] Sentence-depth variability within documents
#'
#' Computes the mean absolute difference between consecutive sentence tree
#' heights (\code{max_path}) within each document. Simplified texts show
#' smoother depth rhythm than originals (Alector corpus, d ≈ 0.65, p ≈ 0.04
#' controlling for sentence length and average height). No signal on grade-level
#' corpora (Wikiviki, d ≈ 0.15).
#'
#' @param sent_stats A \code{data.table} returned by \code{batch_graph_stats},
#'   with columns \code{doc_id}, \code{sentence_id}, and \code{max_path}.
#' @returns A \code{data.table} keyed by \code{doc_id} with columns
#'   \code{avg_sent_height} and \code{depth_delta}.
#' @export
sent_depth_variability <- function(sent_stats) {
  dt <- as.data.table(sent_stats)[order(doc_id, sentence_id)]

  result <- dt[, {
    n     <- .N
    avg_h <- mean(max_path, na.rm = TRUE)
    delta <- if (n > 1L) mean(abs(diff(max_path)), na.rm = TRUE) else NA_real_
    .(avg_sent_height = avg_h, depth_delta = delta)
  }, by = doc_id]

  setkey(result, doc_id)
  return(result)
}

#' Apply GAM calibration to sentence-level features
#'
#' Subtracts the expected value for each sentence length (from saved GAM
#' models), producing residuals approximately uncorrelated with sentence
#' length.
#'
#' @param dt_sent A \code{data.table} from \code{batch_graph_stats} with a
#'   \code{sentence_length} column.
#' @param gam_models Named list of GAM fits, or a path to an \code{.Rds} file.
#' @param suffix String appended to calibrated column names (default
#'   \code{"_resid"}).
#' @returns A copy of \code{dt_sent} with additional \code{*_resid} columns.
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

  return(dt_out)
}
