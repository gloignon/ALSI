# Build the MorphoLex-fr lexical database for use in ALSI.
#
# Output:
#   lexical_dbs/dt_morpholex_fr.Rds   — data.table keyed on `word`
#
# The pre-built file ships with ALSI. If it is absent (e.g. removed for IP
# reasons) run this script once to regenerate it from the upstream source.
#
# Source:
#   Mailhot, H., Bhatt, K., & Tucker, B. V. (2019). MorphoLex-fr.
#   https://github.com/hugomailhot/MorphoLex-fr
#
# Columns retained in the output:
#   word           — lowercase word form
#   n_morphemes    — total number of morphemes
#   canon_segm     — canonical morphemic segmentation string
#   root_fam_size  — morphological family size of the root
#   root_freq      — HAL frequency of the root
#   root_length    — character length of the root morpheme
#   pref_fam_size  — family size of prefix 1 (NA for words with no prefix)
#   suff_fam_size  — family size of suffix 1 (NA for words with no suffix)
#
library(data.table)

# Paths are relative to the repo root. In RStudio, open the .Rproj file and
# the working directory is set automatically. From the command line, run as:
#   Rscript R/artefact_builders/build_morpholex_fr.R
BASE_URL <- "https://raw.githubusercontent.com/hugomailhot/MorphoLex-fr/master/csv/"

# All CSV files in the repo's csv/ folder (n_prefixes_n_roots_n_suffixes.csv).
CSV_FILES <- c(
  "0_1_0.csv", "0_1_1.csv", "0_1_2.csv", "0_1_3.csv",
  "0_2_0.csv", "0_2_2.csv", "0_3_0.csv",
  "1_1_0.csv", "1_1_1.csv", "1_1_2.csv", "1_1_3.csv",
  "1_2_0.csv", "1_2_1.csv",
  "2_1_0.csv", "2_1_1.csv", "2_1_2.csv"
)

message("Fetching MorphoLex-fr CSVs from GitHub...")
chunks <- lapply(CSV_FILES, function(f) {
  url <- paste0(BASE_URL, f)
  message(sprintf("  %s", f))
  tryCatch(
    fread(url, encoding = "UTF-8", showProgress = FALSE),
    error = function(e) {
      warning(sprintf("Failed to fetch %s: %s", f, conditionMessage(e)))
      NULL
    }
  )
})
chunks <- Filter(Negate(is.null), chunks)

dt_raw <- rbindlist(chunks, fill = TRUE)
message(sprintf("Raw rows: %d", nrow(dt_raw)))


# ── Normalise column names ────────────────────────────────────────────────────
# The repo uses PREF_1_*/ROOT_1_*/SUFF_1_* naming.
setnames(dt_raw, "item", "word", skip_absent = TRUE)

dt <- dt_raw[, .(
  word          = tolower(word),
  n_morphemes   = as.integer(n_morphemes),
  canon_segm    = as.character(canon_segm),
  root_fam_size = as.numeric(ROOT_1_FamSize),
  root_freq     = as.numeric(ROOT_1_Freq),
  root_length   = as.integer(ROOT_1_Length)
)]

if ("PREF_1_FamSize" %in% names(dt_raw)) {
  dt[, pref_fam_size := as.numeric(dt_raw$PREF_1_FamSize)]
} else {
  dt[, pref_fam_size := NA_real_]
}

if ("SUFF_1_FamSize" %in% names(dt_raw)) {
  dt[, suff_fam_size := as.numeric(dt_raw$SUFF_1_FamSize)]
} else {
  dt[, suff_fam_size := NA_real_]
}

# Drop any duplicate word forms (keep first occurrence).
dt <- unique(dt, by = "word")
setkey(dt, word)

message(sprintf("Unique word forms: %d", nrow(dt)))
message(sprintf("  n_morphemes range: %d – %d", min(dt$n_morphemes, na.rm = TRUE), max(dt$n_morphemes, na.rm = TRUE)))
message(sprintf("  root_fam_size: %.0f%% non-NA", 100 * mean(!is.na(dt$root_fam_size))))


# ── Save ──────────────────────────────────────────────────────────────────────
out_path <- "lexical_dbs/dt_morpholex_fr.Rds"
saveRDS(dt, out_path)
message(sprintf("\nWritten: %s", out_path))
