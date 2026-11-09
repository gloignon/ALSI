# L2SCA Validation: T-unit counts against published examples
#
# What this validates: T-UNIT COUNT only. This is the one genuinely
# independent check here, because a different parser/annotator can
# legitimately segment T-units differently — that's exactly the kind of
# disagreement worth catching.
#
# Word count and MLT are NOT checks — word count is trivial (any correct
# tokenizer gets it right), and MLT is pure arithmetic on word count and
# T-unit count. They are reported for context only, never scored.
#
# The source CSV also carries paper-stated clause counts (paper_clauses,
# paper_clauses_per_tunit) for one example, but clause-count validation is
# out of scope here — that belongs in a separate, dedicated demo.
#
# This script:
#   1. Loads example sentences + paper-stated T-unit counts from CSV
#   2. Parses all sentences with the English UDPipe model
#   3. Computes tunit_features() for each
#   4. Compares ALSI's T-unit count against the paper-stated one
#
# CSV sources:
#   validation/l2sca_reference_texts/bardovi_harlig_bofman_1988_examples.csv
#   validation/l2sca_reference_texts/polio_1997_examples.csv
# Sources: Bardovi-Harlig & Bofman (1988); Polio (1997)
# Local copies: docs/papers/md/bardovi-harlig-bofman-1988-tunit-analysis.md
#               docs/papers/md/polio-1997-measures-linguistic-accuracy-l2-writing.md
#

library(tidyverse)
library(data.table)

source("R/fnt_corpus.R", encoding = "UTF-8")
source("R/fnt_syntactic_complexity.R", encoding = "UTF-8")
source("R/fnt_tunits.R", encoding = "UTF-8")

csv_paths <- c(
  "validation/l2sca_reference_texts/bardovi_harlig_bofman_1988_examples.csv",
  "validation/l2sca_reference_texts/polio_1997_examples.csv"
)
model_path <- "models/english-ewt-ud-2.5-191206.udpipe"

required_cols <- c(
  "source", "paper_example_label", "subject_label", "data_type", "text",
  "paper_t_units", "paper_words", "paper_mlt", "notes"
)

examples <- csv_paths |>
  map(\(p) read_csv(p, na = c("", "NA"), show_col_types = FALSE)) |>
  list_rbind() |>
  mutate(doc_id = paste0("ex_", row_number()))

missing_cols <- setdiff(required_cols, names(examples))
if (length(missing_cols) > 0L) {
  stop("CSV is missing required column(s): ", paste(missing_cols, collapse = ", "))
}

if (!file.exists(model_path)) {
  stop(
    "English UDPipe model not found at ", model_path, "\n",
    "Place the model there before running this validation demo."
  )
}

message("\nL2SCA T-unit count validation demo")
message("Loaded ", nrow(examples), " examples from ", paste(csv_paths, collapse = ", "))

texts <- examples |> select(doc_id, text)

message("Parsing ", nrow(texts), " sentences with English UDPipe...")

parsed_all <- parse_text(
  texts,
  backend = "udpipe",
  ud_model = model_path,
  n_cores = 1,
  show_progress = FALSE,
  reparse_copulas = FALSE
)

# compte: countable tokens (excludes PUNCT/SYM). Only used to derive MLT for
# context — not itself a validation target.
parsed_all[, compte := !upos %in% c("PUNCT", "SYM")]

parsed_ids <- sort(unique(parsed_all$doc_id))
missing_docs <- setdiff(examples$doc_id, parsed_ids)
if (length(missing_docs) > 0L) {
  stop("Parsed output is missing document(s): ", paste(missing_docs, collapse = ", "))
}

message("Computing T-unit metrics...")

word_counts <- parsed_all[compte == TRUE, .(alsi_words = .N), by = doc_id]

alsi_tunits <- tunit_features(parsed_all) |>
  as_tibble() |>
  left_join(as_tibble(word_counts), by = "doc_id") |>
  transmute(doc_id, alsi_t_units = n_tunits, alsi_words, alsi_mlt = mlt)

comparison <- examples |>
  left_join(alsi_tunits, by = "doc_id") |>
  mutate(t_units_delta = alsi_t_units - paper_t_units)

n_reported <- sum(!is.na(comparison$paper_t_units))
matches <- sum(abs(comparison$alsi_t_units - comparison$paper_t_units) <= 1e-8, na.rm = TRUE)
message(sprintf("T-units: %d/%d match", matches, n_reported))

comparison |>
  select(source, paper_example_label, paper_t_units, alsi_t_units, t_units_delta) |>
  print(n = Inf, width = Inf)
