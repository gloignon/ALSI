# Fetch and pre-process Manulex (Lété et al., 2004) for use in ALSI.
#
# Manulex provides word-form frequency norms derived from French elementary
# school readers (CP through CM2, roughly grades 1–5).
#
# License: CC BY-NC-SA 3.0 Unported — non-commercial use only.
#   http://www.manulex.org
#
# Source (pre-built RDS via openlexicon):
#   Lété, B., Sprenger-Charolles, L., & Colé, P. (2004). MANULEX: A
#   grade-level lexical database from French elementary school readers.
#   Behavior Research Methods, Instruments, & Computers, 36(1), 156–166.
#   https://doi.org/10.3758/BF03195562
#   Data: https://github.com/chrplr/openlexicon (Lete_2004_Manulex)
#
# Output:
#   lexical_dbs/dt_manulex.Rds — data.table keyed on (word, upos_brut)
#
# Columns in output:
#   word       — orthographic word form (lowercase)
#   upos_brut  — Manulex POS tag (VER, NC, NP, ADJ, ADV, PRE, DET, PRO, …)
#   upos       — mapped Universal POS tag
#   freq_u     — frequency per million, CP–CM2 combined
#   freq_log10 — log10(freq_u); simpler alternative to SFI
#   freq_f     — raw occurrence count, CP–CM2 combined
#   grade      — grade band of first appearance (CP, CE1, CE2-CM2)
#   age        — approximate school age for that grade (6, 7, 9)
#   n_letters  — orthographic length
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_manulex.R

library(data.table)

REMOTE_URL <- paste0(
  "https://raw.githubusercontent.com/chrplr/openlexicon/master/",
  "datasets-info/Lete_2004_Manulex/Manulex-Ortho.rds"
)
LOCAL_RAW <- "lexical_dbs/raw/Manulex-Ortho.rds"
OUT_PATH  <- "lexical_dbs/dt_manulex.Rds"

dir.create("lexical_dbs/raw", showWarnings = FALSE, recursive = TRUE)


# ── 1) Load ───────────────────────────────────────────────────────────────────

if (file.exists(LOCAL_RAW)) {
  message("Loading local copy: ", LOCAL_RAW)
  raw <- readRDS(LOCAL_RAW)
} else {
  message("Downloading Manulex-Ortho.rds from openlexicon...")
  tmp <- tempfile(fileext = ".rds")
  tryCatch(
    download.file(REMOTE_URL, tmp, mode = "wb", quiet = TRUE),
    error = function(e) stop(
      "Download failed: ", conditionMessage(e), "\n\n",
      "Obtain Manulex-Ortho.rds manually from:\n",
      "  https://github.com/chrplr/openlexicon/tree/master/datasets-info/Lete_2004_Manulex\n",
      "Then place it at: ", LOCAL_RAW
    )
  )
  raw <- readRDS(tmp)
  file.copy(tmp, LOCAL_RAW)
}

dt <- as.data.table(raw)
message("Loaded: ", nrow(dt), " rows, ", ncol(dt), " columns")


# ── 2) Clean and reshape ──────────────────────────────────────────────────────

# Frequency columns use spaces as thousands separators — strip and coerce.
fix_freq <- function(x) as.numeric(gsub("[[:space:]]", "", as.character(x)))

dt[, word      := tolower(trimws(FORMES_ORTHOGRAPHIQUES))]
dt[, upos_brut := trimws(SYNT)]
dt[, n_letters := as.integer(NLET)]
dt[, freq_u    := fix_freq(`CP-CM2_U`)]
dt[, freq_f    := fix_freq(`CP-CM2_F`)]
dt[, freq_cp   := fix_freq(CP_U)]
dt[, freq_ce1  := fix_freq(CE1_U)]
dt[, freq_ce2  := fix_freq(`CE2-CM2_U`)]

# grade = grade band of FIRST appearance (lowest band with non-zero frequency).
dt[, grade := fcase(
  !is.na(freq_cp)  & freq_cp  > 0, "CP",
  !is.na(freq_ce1) & freq_ce1 > 0, "CE1",
  !is.na(freq_ce2) & freq_ce2 > 0, "CE2-CM2",
  default = NA_character_
)]
dt[, age := fcase(
  grade == "CP",      6L,
  grade == "CE1",     7L,
  grade == "CE2-CM2", 9L,
  default = NA_integer_
)]

# freq_log10: log10 of combined freq_u; NA-safe.
dt[, freq_log10 := ifelse(is.na(freq_u) | freq_u <= 0, NA_real_, log10(freq_u))]

# UPOS mapping from Manulex syntactic categories.
upos_map <- c(
  VER = "VERB", NC  = "NOUN", NP  = "PROPN", ADJ = "ADJ",
  ADV = "ADV",  PRE = "ADP",  DET = "DET",   PRO = "PRON",
  CON = "SCONJ", INT = "INTJ", ABR = "X",    UEUPH = "X"
)
dt[, upos := upos_map[upos_brut]]
dt[is.na(upos), upos := "X"]

dt <- dt[, .(word, upos_brut, upos, freq_u, freq_log10, freq_f,
             grade, age, n_letters)]

# Remove entries with no usable combined frequency.
dt <- dt[!is.na(freq_u)]

setkey(dt, word, upos_brut)
message("Rows: ", nrow(dt), " (", uniqueN(dt$word), " unique word forms)")


# ── 3) Save ───────────────────────────────────────────────────────────────────

saveRDS(dt, OUT_PATH)
message("Saved: ", OUT_PATH)
print(dt[word == "maison"])
print(head(dt, 5))
