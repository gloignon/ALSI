# Fetch and pre-process Lexique 3.83 (New et al., 2001/2004) for use in ALSI.
#
# Lexique3 is a large-scale French lexical database (~140,000 entries)
# providing orthographic and phonemic forms, lemmas, POS, syllabation, and
# word frequencies estimated from two corpora: film subtitles and books.
#
# License: CC BY-SA 4.0 — commercial use permitted; ShareAlike on derivatives.
#   http://www.lexique.org
#
# Source:
#   New, B., Pallier, C., Brysbaert, M., & Ferrand, L. (2004). Lexique 2:
#   A new French lexical database. Behavior Research Methods, Instruments,
#   & Computers, 36(3), 516–524. https://doi.org/10.3758/BF03195598
#   Data: http://www.lexique.org/databases/Lexique383/Lexique383.tsv
#
# Output:
#   lexical_dbs/dt_lexique3.Rds — data.table keyed on `word`
#
# Columns retained:
#   word         — orthographic word form (lowercase)
#   lemma        — lemma (lowercase)
#   pos          — grammatical category (NOM, VER, ADJ, ADV, …)
#   gender       — m / f / NA
#   n_letters    — orthographic length
#   n_syllables  — syllable count
#   n_phonemes   — phoneme count
#   freq_films   — frequency per million, film subtitles corpus
#   freq_books   — frequency per million, books corpus
#   old20        — mean Levenshtein distance to 20 nearest orthographic neighbors
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_lexique3.R

library(data.table)

REMOTE_URL <- "http://www.lexique.org/databases/Lexique383/Lexique383.tsv"
LOCAL_RAW  <- "lexical_dbs/raw/Lexique383.tsv"
OUT_PATH   <- "lexical_dbs/dt_lexique3.Rds"

dir.create("lexical_dbs/raw", showWarnings = FALSE, recursive = TRUE)


# ── 1) Load ───────────────────────────────────────────────────────────────────

if (file.exists(LOCAL_RAW)) {
  message("Loading local copy: ", LOCAL_RAW)
  dt_raw <- fread(LOCAL_RAW, encoding = "UTF-8", showProgress = FALSE)
} else {
  message("Downloading Lexique 3.83 (~17 MB)...")
  dt_raw <- tryCatch(
    fread(REMOTE_URL, encoding = "UTF-8", showProgress = TRUE),
    error = function(e) stop(
      "Download failed: ", conditionMessage(e), "\n\n",
      "Obtain Lexique383.tsv (or Lexique383.zip) manually from:\n",
      "  http://www.lexique.org/databases/Lexique383/\n",
      "Then place the TSV at: ", LOCAL_RAW
    )
  )
  fwrite(dt_raw, LOCAL_RAW, sep = "\t")
}

message("Loaded: ", nrow(dt_raw), " rows")


# ── 2) Select and rename ──────────────────────────────────────────────────────

dt <- dt_raw[, .(
  word        = tolower(trimws(ortho)),
  lemma       = tolower(trimws(lemme)),
  pos         = trimws(cgram),
  gender      = trimws(genre),
  n_letters   = as.integer(nblettres),
  n_syllables = as.integer(nbsyll),
  n_phonemes  = as.integer(nbphons),
  freq_films  = as.numeric(freqfilms2),
  freq_books  = as.numeric(freqlivres),
  old20       = as.numeric(old20)
)]

# Normalise gender: keep m/f, set empty strings to NA.
dt[gender == "", gender := NA_character_]

# Drop rows with no usable word form.
dt <- dt[nzchar(word)]

# Deduplicate: Lexique3 has multiple entries per orthographic form (one per
# POS/homograph). Collapse by keeping the row with the highest combined
# frequency, so lookup_lexique3_freq() gets a single representative value.
dt[, freq_combined := freq_films + freq_books]
setorder(dt, word, -freq_combined)
dt_dedup <- unique(dt, by = "word")
dt_dedup[, freq_combined := NULL]

setkey(dt_dedup, word)
message(sprintf("Rows: %d raw → %d after dedup on orthographic form",
                nrow(dt), nrow(dt_dedup)))


# ── 3) Save ───────────────────────────────────────────────────────────────────

saveRDS(dt_dedup, OUT_PATH)
message("Saved: ", OUT_PATH)
print(head(dt_dedup, 5))
