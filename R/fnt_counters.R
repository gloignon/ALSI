# This file contains functions for simple calculations of counts and proportions
# Includes counts for part-of-speech tags

library(data.table)

#' Compute Document-Level Count Features from Parsed Data
#'
#' Calculates word counts, unique words, content words, sentences, paragraphs,
#' character counts, average word/sentence length, and UPOS tag distribution
#' (counts and proportions pivoted wide) for each document.
#'
#' @param parsed_data A \code{data.table} of parsed tokens with columns
#'   \code{doc_id}, \code{token}, \code{compte}, \code{upos},
#'   \code{sentence_id}, and optionally \code{paragraph_id}.
#' @param content_word_upos Character vector of UPOS tags considered content
#'   words. Defaults to \code{c("NOUN", "VERB", "ADJ", "ADV", "PRON")}.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{doc_level_counts}{A \code{data.table} with one row per document
#'       containing word_count, unique_word_count, content_word_count,
#'       unique_content_word_count, sentence_count, paragraph_count,
#'       avg_word_length, char_count, char_count_content,
#'       avg_sentence_length, and avg_content_word_length.}
#'     \item{upos_counts}{A wide \code{data.table} with per-document UPOS
#'       counts and proportions (one column per UPOS tag).}
#'   }
simple_count_features <- function(parsed_data, content_word_upos = c("NOUN", "VERB", "ADJ", "ADV", "PRON")) {
  # Ensure the input is a data.table
  if (!is.data.table(parsed_data)) {
    stop("Input must be a data.table.")
  }
  
  # Group by doc_id and calculate features
  results <- parsed_data[, .(
    
    # Total word count: Count all tokens that are not empty or NA
    word_count = sum(!is.na(compte) & compte == TRUE & !is.na(upos) & !upos == "PUNCT"),
    
    # unique word count
    unique_word_count = length(unique(token[!is.na(compte) & compte == TRUE & !is.na(upos) & !upos == "PUNCT"])),
    
    # Content word count: Count tokens with UPOS in the specified list
    content_word_count = sum(!compte == FALSE & upos %in% content_word_upos),
    
    # unique content word count
    unique_content_word_count = length(unique(token[!compte == FALSE & upos %in% content_word_upos])),
    
    # Sentence count: Count unique sentence IDs
    sentence_count = length(unique(sentence_id)),
    
    # Paragraph count: Count unique paragraph IDs (if available)
    paragraph_count = if ("paragraph_id" %in% names(parsed_data)) {
      length(unique(paragraph_id))
    } else {
      NA_real_
    },
    
    avg_word_length = mean(nchar(token[upos %in% content_word_upos]), na.rm = TRUE),
    
    # number of characters in the text
    char_count = sum(nchar(token[!compte == FALSE & !upos == "PUNCT" & !upos == "PART" & !is.na(token)]), na.rm = TRUE),
    char_count_content = sum(nchar(token[!compte == FALSE & upos %in% content_word_upos]), na.rm = TRUE)
    
  ), by = doc_id]
  
  # add average word and sentence length
  results[, avg_word_length := char_count / word_count]
  results[, avg_sentence_length := word_count / sentence_count]
  
  # average content word length
  results[, avg_content_word_length := char_count / content_word_count]
  
  # UPOS distribution: Add UPOS counts as a separate table grouped by doc_id
  upos_counts <- parsed_data[, .N, by = .(doc_id, upos)]
  setnames(upos_counts, "N", "count")
  
  # add a column with proportions of each UPOS
  upos_counts[, prop := count / sum(count), by = doc_id]
  
  # pivot the count and proportion table to wide, fill missing values with 0
  upos_counts <- dcast(upos_counts, doc_id ~ upos, value.var = c("count", "prop"), fill = 0)
  
  # Add the UPOS distribution as part of the results
  results <- list(
    doc_level_counts = results,
    upos_counts = upos_counts
  )
  
  return(results)
}

#' Summarise one verb-form indicator within documents
#'
#' Computes the observation count, mean, and sample standard deviation of a
#' binary indicator over its eligible tokens. Documents with no eligible tokens
#' have a zero observation count and missing mean and SD; singleton streams
#' have a missing SD.
#'
#' @param dt A parsed-corpus \code{data.table} containing \code{doc_id}.
#' @param hit Logical vector marking tokens that meet the indicator definition.
#' @param eligible Logical vector marking tokens in the indicator population.
#' @param prefix Column-name prefix for the three statistics.
#' @returns A \code{data.table} with one row per document and columns
#'   \code{<prefix>_n}, \code{<prefix>_mean}, and \code{<prefix>_sd}.
#' @keywords internal
.verb_indicator_summary <- function(dt, hit, eligible, prefix) {
  result <- unique(dt[, .(doc_id)])
  observed <- data.table(doc_id = dt$doc_id, hit = hit, eligible = eligible)
  stats <- observed[eligible == TRUE, .(
    n = .N,
    mean = mean(hit),
    sd = if (.N > 1L) sd(as.numeric(hit)) else NA_real_
  ), by = doc_id]
  result <- merge(result, stats, by = "doc_id", all.x = TRUE, sort = FALSE)
  result[is.na(n), n := 0L]
  setnames(result, c("n", "mean", "sd"),
           paste0(prefix, c("_n", "_mean", "_sd")))
  return(result)
}

#' Compute verb tense and mood features per document
#'
#' Preserves the existing tag counts and word-count proportions, and adds
#' document-level distribution summaries of binary verb indicators.
#'
#' @param parsed_corpus A \code{data.table} of parser output after
#'   \code{post_process_lexicon}, with \code{doc_id}, \code{feats},
#'   \code{upos}, and \code{compte} columns.
#' @param counts A \code{data.table} with \code{doc_id} and
#'   \code{word_count}, used for the existing proportions.
#'
#' @details
#' Existing counts inspect all rows, including tokens without a verb UPOS tag;
#' their proportions divide by \code{word_count}. The added
#' \code{present_indicative_finite_count} counts countable VERB/AUX tokens
#' jointly tagged \code{Mood=Ind}, \code{Tense=Pres}, and
#' \code{VerbForm=Fin}; its \code{_prop} also divides by word count.
#'
#' Summary means and sample SDs use one binary observation per countable
#' VERB/AUX token bearing the relevant UD attribute. Present, past, and future
#' use tokens bearing \code{Tense}; mood indicators use tokens bearing
#' \code{Mood}; infinitive uses tokens bearing \code{VerbForm}. Participles
#' require \code{Tense} and \code{VerbForm}, and finite indicative forms
#' require all three attributes. Missing required attributes are excluded
#' from that indicator's population, not scored as absence. Zero eligible
#' tokens yield \code{n=0} and missing mean/SD; a singleton yields missing
#' SD; a constant stream of at least two yields SD zero. These summaries are
#' local UD-tag statistics, not exact FABRA verb-group measures.
#'
#' @returns A list of three \code{data.table}s, each with one row per document:
#'   \code{counts} (existing counts plus joint present-indicative-finite
#'   count), \code{proportions} (counts divided by word count), and
#'   \code{summaries} (\code{_n}, \code{_mean}, \code{_sd} per indicator).
#' @citation_type "derived" (document-wise UD-tag indicator summaries)
#' @export
verb_tense_features <- function(parsed_corpus, counts) {
  if (!is.data.table(parsed_corpus)) {
    stop("parsed_corpus must be a data.table.")
  }
  if (!is.data.table(counts)) {
    stop("Counts must be a data.table.")
  }
  required <- c("doc_id", "feats", "upos", "compte")
  if (!all(required %in% names(parsed_corpus))) {
    stop("parsed_corpus missing columns: ",
         paste(setdiff(required, names(parsed_corpus)), collapse = ", "))
  }
  dt <- copy(parsed_corpus)
  verb <- dt$compte == TRUE & dt$upos %in% c("VERB", "AUX")
  has <- function(key, value = NULL) {
    pattern <- if (is.null(value)) {
      paste0("(^|\\|)", key, "=[^|]+(\\||$)")
    } else {
      paste0("(^|\\|)", key, "=", value, "(\\||$)")
    }
    return(!is.na(dt$feats) & grepl(pattern, dt$feats))
  }
  tense <- has("Tense")
  mood <- has("Mood")
  form <- has("VerbForm")
  finite <- has("VerbForm", "Fin")
  present <- has("Tense", "Pres")
  past <- has("Tense", "Past")
  future <- has("Tense", "Fut")
  conditional <- has("Mood", "Cnd")
  subjunctive <- has("Mood", "Sub")
  indicative <- has("Mood", "Ind")
  imperative <- has("Mood", "Imp")
  infinitive <- has("VerbForm", "Inf")
  participle <- has("VerbForm", "Part")
  joint <- verb & present & indicative & finite

  results_n <- dt[, .(
    present_count = sum(grepl("Tense=Pres", feats), na.rm = TRUE),
    past_count = sum(grepl("Tense=Past", feats), na.rm = TRUE),
    future_count = sum(grepl("Tense=Fut", feats), na.rm = TRUE),
    conditional_count = sum(grepl("Mood=Cnd", feats), na.rm = TRUE),
    subjunctive_count = sum(grepl("Mood=Sub", feats), na.rm = TRUE),
    indicative_count = sum(grepl("Mood=Ind", feats), na.rm = TRUE),
    imperative_count = sum(grepl("Mood=Imp", feats), na.rm = TRUE),
    infinitive_count = sum(grepl("VerbForm=Inf", feats), na.rm = TRUE),
    past_participle_count = sum(grepl("VerbForm=Part", feats) &
                                 grepl("Tense=Past", feats), na.rm = TRUE),
    present_participle_count = sum(grepl("VerbForm=Part", feats) &
                                    grepl("Tense=Pres", feats), na.rm = TRUE),
    past_simple_count = sum(grepl("Tense=Past", feats) &
                              grepl("Mood=Ind", feats) &
                              grepl("VerbForm=Fin", feats), na.rm = TRUE)
  ), by = doc_id]
  results_n[, present_indicative_finite_count :=
              dt[, .(doc_id, joint = joint)][,
                  .(n = sum(joint)), by = doc_id]$n]

  results_p <- melt(results_n, id.vars = "doc_id",
                    variable.name = "feature", value.name = "count")
  results_p <- merge(results_p, counts[, .(doc_id, word_count)],
                     by = "doc_id", all.x = TRUE)
  results_p[, proportion := count / word_count]
  results_p <- dcast(results_p, doc_id + word_count ~ feature,
                     value.var = "proportion")
  setnames(results_p,
           old = setdiff(names(results_p), c("doc_id", "word_count")),
           new = gsub("_count$", "_prop",
                      setdiff(names(results_p), c("doc_id", "word_count"))))
  results_p[, word_count := NULL]

  indicators <- list(
    present = list(hit = present, eligible = verb & tense),
    past = list(hit = past, eligible = verb & tense),
    future = list(hit = future, eligible = verb & tense),
    conditional = list(hit = conditional, eligible = verb & mood),
    subjunctive = list(hit = subjunctive, eligible = verb & mood),
    indicative = list(hit = indicative, eligible = verb & mood),
    imperative = list(hit = imperative, eligible = verb & mood),
    infinitive = list(hit = infinitive, eligible = verb & form),
    past_participle = list(hit = past & participle,
                           eligible = verb & tense & form),
    present_participle = list(hit = present & participle,
                              eligible = verb & tense & form),
    past_simple = list(hit = past & indicative & finite,
                       eligible = verb & tense & mood & form),
    present_indicative_finite = list(hit = joint,
                                     eligible = verb & tense & mood & form)
  )
  summaries <- unique(dt[, .(doc_id)])
  for (feature in names(indicators)) {
    indicator <- indicators[[feature]]
    stats <- .verb_indicator_summary(dt, indicator$hit,
                                     indicator$eligible, feature)
    summaries <- merge(summaries, stats, by = "doc_id", sort = FALSE)
  }

  return(list(counts = results_n, proportions = results_p,
              summaries = summaries))
}

#' Sentence-length variability within documents
#'
#' Measures how much sentence length varies across sentences in each document.
#' Four complementary metrics:
#' \itemize{
#'   \item \code{sent_length_sd}: standard deviation of per-sentence word counts.
#'   \item \code{sent_length_cv}: coefficient of variation (SD / mean), scale-free.
#'   \item \code{sent_length_delta}: mean absolute difference between consecutive
#'     sentence lengths — captures sequential rhythm rather than global spread.
#'   \item \code{sent_length_autocor}: lag-1 autocorrelation of sentence lengths —
#'     how strongly each sentence's length predicts the next one's.
#' }
#' Documents with fewer than 2 sentences return \code{NA} for all metrics.
#' Documents with fewer than 3 sentences return \code{NA} for \code{sent_length_autocor}
#' (cor() is undefined on length-2 vectors with zero variance).
#'
#' @param parsed_corpus A \code{data.table} from \code{post_process_lexicon}
#'   with columns \code{doc_id}, \code{sentence_id}, and \code{compte}.
#' @returns A \code{data.table} keyed by \code{doc_id} with columns
#'   \code{sent_length_sd}, \code{sent_length_cv}, \code{sent_length_delta},
#'   \code{sent_length_autocor}.
#' @export
sent_length_variability <- function(parsed_corpus) {
  # Sentence-level word counts (countable tokens only)
  sent_wc <- parsed_corpus[compte == TRUE,
    .(n_words = .N),
    by = .(doc_id, sentence_id)]

  result <- sent_wc[, {
    n    <- .N
    mu   <- mean(n_words)
    sdev <- if (n > 1) sd(n_words) else NA_real_
    cv   <- if (n > 1 && mu > 0) sdev / mu else NA_real_
    delta   <- if (n > 1) mean(abs(diff(n_words))) else NA_real_
    autocor <- if (n > 2) suppressWarnings(cor(n_words[-n], n_words[-1])) else NA_real_
    .(sent_length_sd = sdev, sent_length_cv = cv,
      sent_length_delta = delta, sent_length_autocor = autocor)
  }, by = doc_id]

  setkey(result, doc_id)
  return(result)
}