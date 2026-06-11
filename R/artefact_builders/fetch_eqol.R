# Fetch and pre-process ÉQOL-Infra (Stanké et al., 2019) for use in ALSI.
#
# ÉQOL (Échelle Québécoise d'Orthographes Lexicales) provides orthographic
# acquisition norms for Quebec French primary school words (Grades 1–6),
# along with phonological and graphemic properties.
#
# We use the ÉQOL-Infra all version (inflected forms) and extract a simplified
# word-level table — frequency norms, POS, and orthographic length — discarding
# the detailed grapheme–phoneme association tables (see *-associations files
# for those). Use eqol_infra_lemme_v1.2.xlsx for lemma-level lookup instead.
#
# License: No open license — default Canadian copyright, research/study use
#   only. Commercial use requires written authorization from the authors.
#   Contact: Stanké et al. / Appligogiques (https://www.appligogiques.com/eqol)
#
# Source:
#   Stanké, B., Le Mené, M., Rezzonico, S., Moreau, A., Dumais, C.,
#   Robidoux, J., Dault, C., & Royle, P. (2019). ÉQOL: Une nouvelle base de
#   données québécoise du lexique scolaire du primaire comportant une échelle
#   d'acquisition de l'orthographe lexicale. Corpus, 19.
#   https://doi.org/10.4000/corpus.3818
#   Data: https://inframorph.github.io/eqolinfra/telechargement.html
#
# Output:
#   lexical_dbs/dt_eqol.Rds — data.table keyed on `word`
#
# Columns in output:
#   word        — lemma (lowercase)
#   pos         — grammatical category
#   n_letters   — orthographic length
#   n_syllables — syllable count
#   n_phonemes  — phoneme count
#   freq_grade1 … freq_grade6 — frequency per grade (if present in source)
#   acq_grade   — estimated grade of orthographic acquisition (ÉQOL score)
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_eqol.R

library(data.table)

# ÉQOL-Infra all: one row per inflected form — better for token-level lookup.
# (Use eqol_infra_lemme_v1.2.xlsx for lemma-level lookup instead.)
REMOTE_URL <- "https://inframorph.github.io/eqolinfra/eqol_infra_all_v1.2.xlsx"
LOCAL_RAW  <- "lexical_dbs/raw/eqol_infra_all_v1.2.xlsx"
OUT_PATH  <- "lexical_dbs/dt_eqol.Rds"

dir.create("lexical_dbs/raw", showWarnings = FALSE, recursive = TRUE)


# ── 1) Load ───────────────────────────────────────────────────────────────────

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required. Install with: install.packages('readxl')")
}

if (file.exists(LOCAL_RAW)) {
  message("Loading local copy: ", LOCAL_RAW)
} else {
  message("Downloading ÉQOL-Infra lemme from inframorph.github.io...")
  tryCatch(
    download.file(REMOTE_URL, LOCAL_RAW, mode = "wb", quiet = TRUE),
    error = function(e) stop(
      "Download failed: ", conditionMessage(e), "\n\n",
      "Obtain eqol_infra_all_v1.2.xlsx manually from:\n",
      "  https://inframorph.github.io/eqolinfra/eqol_infra_all_v1.2.xlsx\n",
      "Then place it at: ", LOCAL_RAW
    )
  )
}

# The xlsx has a two-row merged header: row 1 = group labels, row 2 = column
# names. Skip row 1 so row 2 becomes the header, then drop the blank row 3.
raw <- as.data.table(readxl::read_excel(LOCAL_RAW, sheet = 1, skip = 1))
raw <- raw[!is.na(raw[[1]]) & nzchar(as.character(raw[[1]]))]
message("Loaded: ", nrow(raw), " rows — columns:\n  ",
        paste(names(raw), collapse = "\n  "))


# ── 2) Normalise ──────────────────────────────────────────────────────────────

dt <- copy(raw)
setnames(dt, trimws(names(dt)))

# Column detection: ÉQOL-Infra uses French column names; map to ALSI conventions.
# Adjust the vectors below if a future version renames columns.
col_map <- list(
  word        = c("ortho", "Ortho", "Lemme", "lemme", "Mot", "mot"),
  pos         = c("class", "Class", "Cat", "cat", "Catégorie", "POS"),
  n_letters   = c("letters", "Letters", "NbLettres", "NLettres", "Nlet"),
  n_syllables = c("syllabes", "Syllabes", "NbSyllabes", "NSyllabes", "Nsyl"),
  n_phonemes  = c("phonemes", "Phonemes", "NbPhonemes", "NPhonemes", "Nphon"),
  acq_grade   = c("frequency", "Frequency", "EQOL", "EqolGrade", "NiveauAcq",
                  "GradeAcq", "Acquisition"),
  old20       = c("OLD20", "old20", "Levenstein", "Levenshtein")
)

rename_col <- function(dt, candidates, target) {
  hit <- intersect(candidates, names(dt))[1]
  if (!is.na(hit) && hit != target) setnames(dt, hit, target)
  hit
}

found <- vapply(names(col_map), function(nm) {
  rename_col(dt, col_map[[nm]], nm)
}, character(1))
message("Column mapping: ", paste(names(found), "->", found, collapse = ", "))

if (is.na(found["word"])) {
  stop(
    "Cannot identify the lemma column. Inspect the raw file and update col_map.\n",
    "Available columns: ", paste(names(dt), collapse = ", ")
  )
}

# Frequency columns: detect grade-level freq columns (e.g. FreqG1..FreqG6)
freq_cols <- grep("^[Ff]req.*[Gg][1-6]$|^[Ff]req_grade[1-6]$|^[Ff][1-6]$",
                  names(dt), value = TRUE)
if (length(freq_cols) > 0) {
  new_names <- paste0("freq_grade", seq_along(freq_cols))
  setnames(dt, freq_cols, new_names)
  freq_cols <- new_names
}

# Keep only the columns we need.
keep <- c("word", intersect(c("pos", "n_letters", "n_syllables", "n_phonemes",
                               "acq_grade", "old20"), names(dt)), freq_cols)
dt <- dt[, keep, with = FALSE]

dt[, word := tolower(trimws(word))]
dt <- dt[nzchar(word)]

# Deduplicate on word form (keep first, which is typically the primary entry).
dt <- unique(dt, by = "word")
setkey(dt, word)

message("Rows after dedup: ", nrow(dt))


# ── 3) Save ───────────────────────────────────────────────────────────────────

saveRDS(dt, OUT_PATH)
message("Saved: ", OUT_PATH)
print(head(dt, 5))
