
#' Load a language model for token-level scoring
#'
#' Initialises a masked or autoregressive language model via the Python
#' \code{llm_scoring.py} backend. The model is cached in the Python session.
#'
#' @param model_name HuggingFace model identifier.
#' @param mode Either \code{"mlm"} (masked LM) or \code{"ar"} (autoregressive).
#' @param use_fast Logical; use the fast tokenizer if available.
#' @param trust_remote_code Logical; allow remote code execution for the model.
#' @param add_prefix_space Logical or NULL; add a leading space to tokens.
#' @returns Invisible TRUE on success.
load_llm_scorer <- function(model_name = "almanach/moderncamembert-base",
                            mode = "mlm",
                            use_fast = TRUE,
                            trust_remote_code = TRUE,
                            add_prefix_space = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }
  message(sprintf("Loading %s LLM model '%s' (this may take a while on first run)...", mode, model_name))
  reticulate::py_require(c("torch", "transformers"))
  reticulate::source_python("py/llm_scoring.py")
  load_llm_model(
    model_name,
    mode = mode,
    use_fast = use_fast,
    trust_remote_code = trust_remote_code,
    add_prefix_space = add_prefix_space
  )
  invisible(TRUE)
}

#' Compute token-level LLM surprisal and entropy
#'
#' Scores each sentence with a masked or autoregressive language model and
#' returns per-token surprisal, entropy, and sub-word token count.
#'
#' @param dt_corpus A \code{data.table} with \code{doc_id},
#'   \code{sentence_id}, and \code{token}.
#' @param model_name HuggingFace model identifier.
#' @param mode Either \code{"mlm"} or \code{"ar"}.
#' @param context Optional context string prepended to each sentence.
#' @param batch_size Batch size for MLM scoring (0 = auto).
#' @param temperature Softmax temperature (default 1.0).
#' @param use_fast Logical; use the fast tokenizer.
#' @param trust_remote_code Logical; allow remote code for the model.
#' @param add_prefix_space Logical or NULL; add a leading space to tokens.
#' @returns The input \code{data.table} augmented with \code{llm_surprisal},
#'   \code{llm_entropy}, and \code{llm_subword_n} columns.
llm_surprisal_entropy <- function(dt_corpus,
                                  model_name = "almanach/moderncamembert-base",
                                  mode = "mlm",
                                  context = NULL,
                                  batch_size = 0,
                                  temperature = 1.0,
                                  use_fast = TRUE,
                                  trust_remote_code = TRUE,
                                  add_prefix_space = NULL) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').")
  }
  if (!("doc_id" %in% names(dt_corpus)) ||
      !("sentence_id" %in% names(dt_corpus)) ||
      !("token" %in% names(dt_corpus))) {
    stop("dt_corpus must contain columns: doc_id, sentence_id, token")
  }

  dt <- data.table::as.data.table(data.table::copy(dt_corpus))

  # Choose a stable ordering within sentence
  if ("token_id" %in% names(dt)) {
    order_col <- "token_id"
  } else if ("vrai_token_id" %in% names(dt)) {
    order_col <- "vrai_token_id"
  } else if ("term_id" %in% names(dt)) {
    order_col <- "term_id"
  } else {
    stop("No token order column found. Expected token_id, vrai_token_id, or term_id.")
  }

  data.table::setorderv(dt, c("doc_id", "sentence_id", order_col))
  dt[, token_index := seq_len(.N), by = .(doc_id, sentence_id)]

  # Load Python scoring backend (only if not already loaded)
  reticulate::source_python("py/llm_scoring.py")
  py_state <- tryCatch(get_llm_state(), error = function(e) NULL)
  py_model <- if (!is.null(py_state)) py_state$model_name else NULL
  py_mode <- if (!is.null(py_state)) py_state$mode else NULL
  py_prefix_space <- if (!is.null(py_state)) py_state$add_prefix_space else NULL

  if (is.null(py_model) ||
      is.null(py_mode) ||
      py_model != model_name ||
      py_mode != mode ||
      is.null(py_prefix_space) ||
      (!is.null(add_prefix_space) && isTRUE(add_prefix_space) && !isTRUE(py_prefix_space))) {
    load_llm_model(
      model_name,
      mode = mode,
      use_fast = use_fast,
      trust_remote_code = trust_remote_code,
      add_prefix_space = add_prefix_space
    )
  }

  dt_sentences <- dt[, .(tokens = list(token)), by = .(doc_id, sentence_id)]
  total_sentences <- nrow(dt_sentences)

  results <- vector("list", total_sentences)
  start_time <- Sys.time()

  print_status <- function(processed, total, start_time) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    speed <- processed / elapsed * 60  # sentences per minute
    remaining <- (total - processed) / speed * 60  # seconds
    cat(sprintf("\rProcessed %d/%d sentences (%.1f sent/min) — ~%.1f min left     ",
                processed, total, speed, remaining / 60), sep = "")
    flush.console()
  }

  for (i in seq_len(total_sentences)) {
    sent_tokens <- dt_sentences$tokens[[i]]
    sent_tokens <- as.character(sent_tokens)
    if (anyNA(sent_tokens)) {
      sent_tokens[is.na(sent_tokens)] <- ""
    }
    doc_id <- dt_sentences$doc_id[[i]]
    sentence_id <- dt_sentences$sentence_id[[i]]

    score <- tryCatch({
      if (mode == "mlm") {
        score_masked_lm_tokens(
          tokens = sent_tokens,
          temperature = temperature,
          batch_size = batch_size,
          context_text = context
        )
      } else if (mode == "ar") {
        score_autoregressive_tokens(
          tokens = sent_tokens,
          temperature = temperature,
          context_text = context
        )
      } else {
        stop("mode must be one of: 'mlm', 'ar'")
      }
    }, error = function(e) {
      warning(sprintf("LLM scoring failed for doc_id = %s, sentence_id = %s: %s",
                      doc_id, sentence_id, e$message))
      NULL
    })

    if (!is.null(score)) {
      n_tokens <- length(sent_tokens)
      results[[i]] <- data.table::data.table(
        doc_id = doc_id,
        sentence_id = sentence_id,
        token_index = seq_len(n_tokens),
        llm_surprisal = unlist(score$word_surprisals),
        llm_entropy = unlist(score$word_entropies),
        llm_subword_n = unlist(score$word_token_counts)
      )
    }

    if (i %% 3 == 0 || i == total_sentences) {
      print_status(i, total_sentences, start_time)
    }
  }

  cat("\nDone!\n")

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) {
    dt[, `:=`(
      llm_surprisal = NA_real_,
      llm_entropy = NA_real_,
      llm_subword_n = NA_integer_
    )]
    dt_out <- dt
  } else {
    dt_scores <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
    dt_out <- merge(
      dt,
      dt_scores,
      by = c("doc_id", "sentence_id", "token_index"),
      all.x = TRUE
    )
  }

  dt_out[, token_index := NULL]

  return(dt_out)
}
