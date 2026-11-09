# Fetch and combine the French UniversalCEFR datasets for use as a CEFR-level
# validation corpus in ALSI demos.
#
# Datasets (HuggingFace, UniversalCEFR collection):
#   - readme_fr  — 1,669 sentence-level texts, CEFR A1-C2, from the ReadMe++
#     benchmark. CC BY-NC-SA 4.0.
#       Naous, T. et al. (2024). Benchmarking Multilingual Language Models
#       for Multi-Domain Readability Assessment. EMNLP 2024.
#   - kwiqiz_fr  — 344 document-level texts (full articles), CEFR levels vary,
#     sourced from the Kwiziq learning platform. CC BY-NC 4.0.
#
# Both are public, non-gated HuggingFace dataset repos, redistributed by
# UniversalCEFR as derived parquet under their original (non-commercial)
# licenses — see CEFR_FR_NOTICE for full terms. This script downloads the
# HuggingFace auto-converted parquet files directly (no authentication
# needed).
#
# Text cleaning (kwiqiz_fr in particular ships document-level text with
# artefacts that confuse downstream sentence segmentation/parsing):
#   - bracket glosses, e.g. "p'tits [petits] tours", "c[e n]'est",
#     "Les règles [sic: la règle]" — inline editorial corrections/glosses
#     from the Kwiziq learning materials, not part of the running text.
#     Stripped (along with one leading space, if any).
#   - doubled straight quotes (`""like this""`), an export artefact for
#     embedded quotations — collapsed to a single `"`.
#   - runs of whitespace/newlines (paragraph breaks in the source) —
#     collapsed to a single space, since downstream consumers re-segment
#     into sentences themselves.
# readme_fr's sentences are already clean; the same cleaning is applied
# uniformly to both for consistency and is a no-op on text that doesn't
# need it.
#
# Output:
#   demo_corpora/cefr_fr_corpus.csv with columns:
#     corpus      — "readme_fr" or "kwiqiz_fr"
#     level       — CEFR level (A1..C2)
#     doc_id      — unique id, "uni_cefr_<row>_<level>", e.g. "uni_cefr_1_B1"
#     title       — original title/identifier, if present
#     category    — domain/category tag from source
#     license     — license string from source
#     text        — cleaned text content
#
# Run from the repository root:
#   Rscript R/artefact_builders/fetch_universalcefr_fr.R

library(data.table)

if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  stop("Package 'nanoparquet' is required. Install with: install.packages('nanoparquet')")
}

DATASETS <- list(
  readme_fr  = "https://huggingface.co/datasets/UniversalCEFR/readme_fr/resolve/refs%2Fconvert%2Fparquet/default/train/0000.parquet",
  kwiqiz_fr  = "https://huggingface.co/datasets/UniversalCEFR/kwiqiz_fr/resolve/refs%2Fconvert%2Fparquet/default/train/0000.parquet"
)

RAW_DIR <- "demo_corpora/raw_universalcefr"
OUT_CSV <- "demo_corpora/cefr_fr_corpus.csv"

dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)


# ── 1) Download ──────────────────────────────────────────────────────────────

fetch_one <- function(name, url) {
  local_path <- file.path(RAW_DIR, paste0(name, ".parquet"))
  if (file.exists(local_path)) {
    message("Loading local copy: ", local_path)
    return(local_path)
  }
  message("Downloading ", name, " from HuggingFace...")
  result <- tryCatch(
    download.file(url, local_path, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error") || !file.exists(local_path) || file.size(local_path) == 0) {
    unlink(local_path)
    warning(
      "Skipping ", name, " — download failed. Dataset page:\n",
      "  https://huggingface.co/datasets/UniversalCEFR/", name, "\n",
      "Download 0000.parquet manually and place it at: ", local_path, call. = FALSE
    )
    return(NULL)
  }
  return(local_path)
}

raw_paths <- Filter(Negate(is.null), Map(fetch_one, names(DATASETS), DATASETS))

if (length(raw_paths) == 0) {
  stop("No datasets could be downloaded. See warnings above.")
}


# ── 2) Load and normalise ────────────────────────────────────────────────────

clean_text <- function(text) {
  text <- gsub("\\s?\\[[^\\[\\]]*\\]", "", text, perl = TRUE)  # bracket glosses
  text <- gsub('""', '"', text, fixed = TRUE)                   # doubled quotes
  text <- gsub("\\s+", " ", text, perl = TRUE)                  # collapse whitespace
  text <- trimws(text)
  return(text)
}

load_one <- function(name, path) {
  dt <- as.data.table(nanoparquet::read_parquet(path))
  message(name, ": ", nrow(dt), " rows — columns: ", paste(names(dt), collapse = ", "))

  required <- c("title", "category", "cefr_level", "license", "text")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(name, " is missing expected column(s): ", paste(missing, collapse = ", "))
  }

  out <- dt[, .(corpus = name, level = cefr_level,
                title, category, license, text = clean_text(text))]
  return(out)
}

combined <- rbindlist(Map(load_one, names(raw_paths), raw_paths))
combined[, doc_id := sprintf("uni_cefr_%d_%s", .I, level)]
setcolorder(combined, c("corpus", "level", "doc_id", "title", "category", "license", "text"))


# ── 3) Save ───────────────────────────────────────────────────────────────────

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
fwrite(combined, OUT_CSV)

message("Saved: ", OUT_CSV, " (", nrow(combined), " rows)")
print(combined[, .N, by = .(corpus, level)])
