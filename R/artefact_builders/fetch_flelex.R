# Fetch and pre-process FLELex (François et al., 2014) for use in ALSI.
#
# FLELex provides CEFR-graded lexical frequency norms for French as a foreign
# language — per-level frequencies across A1–C2 corpora.
#
# License: CC BY-NC-SA 4.0 — non-commercial use only.
#   https://cental.uclouvain.be/cefrlex/flelex/download/
#
# Source:
#   François, T., Gala, N., Watrin, P., & Fairon, C. (2014, May). FLELex:
#   A graded lexical resource for French foreign learners. In Proceedings of
#   LREC 2014. European Language Resources Association.
#   Data: https://cental.uclouvain.be/cefrlex/flelex/download/
#
# Output:
#   lexical_dbs/dt_flelex.Rds — data.table keyed on (word, upos)
#
# Columns in output:
#   word       — lemma / word form (lowercase)
#   upos       — Universal POS tag (TreeTagger mapped to UPOS)
#   freq_u     — total frequency per million across all CEFR levels
#   freq_log10 — log10(freq_u)
#   grade      — CEFR level of first appearance (lowest non-zero level)
#   num_grade  — numeric grade: A1=1 … C2=6
#
# Run from the repo root:
#   Rscript R/artefact_builders/fetch_flelex.R

library(data.table)

REMOTE_URL <- "https://cental.uclouvain.be/cefrlex/static/resources/fr/FleLex_TT.csv"
LOCAL_RAW  <- "lexical_dbs/raw/FleLex_TT.csv"
OUT_PATH   <- "lexical_dbs/dt_flelex.Rds"

dir.create("lexical_dbs/raw", showWarnings = FALSE, recursive = TRUE)


# ── 1) Load ───────────────────────────────────────────────────────────────────

if (file.exists(LOCAL_RAW)) {
  message("Loading local copy: ", LOCAL_RAW)
  dt_raw <- fread(LOCAL_RAW, encoding = "UTF-8", showProgress = FALSE)
} else {
  message("Downloading FLELex from UCLouvain...")
  dt_raw <- tryCatch(
    fread(REMOTE_URL, encoding = "UTF-8", showProgress = FALSE),
    error = function(e) stop(
      "Download failed: ", conditionMessage(e), "\n\n",
      "Obtain FleLex_TT.csv manually from:\n",
      "  https://cental.uclouvain.be/cefrlex/flelex/download/\n",
      "Then place it at: ", LOCAL_RAW
    )
  )
  fwrite(dt_raw, LOCAL_RAW)
}

message("Loaded: ", nrow(dt_raw), " rows")


# ── 2) Reshape ────────────────────────────────────────────────────────────────

dt <- copy(dt_raw)
# Raw columns: word, tag, freq_A1, freq_A2, freq_B1, freq_B2, freq_C1, freq_C2, freq_total
setnames(dt, tolower(names(dt)))
setnames(dt, "tag", "pos_raw")

LEVEL_COLS  <- c("freq_a1", "freq_a2", "freq_b1", "freq_b2", "freq_c1", "freq_c2")
CEFR_LABELS <- c("A1", "A2", "B1", "B2", "C1", "C2")

dt[, word      := tolower(trimws(word))]
dt[, freq_u    := as.numeric(freq_total)]
dt[, freq_log10 := ifelse(is.na(freq_u) | freq_u <= 0, NA_real_, log10(freq_u))]

# grade = CEFR level of FIRST appearance (lowest level with non-zero frequency).
dt[, grade := {
  mat <- as.matrix(.SD)
  first_nonzero <- apply(mat, 1, function(row) {
    hit <- which(!is.na(row) & row > 0)
    if (length(hit)) CEFR_LABELS[min(hit)] else NA_character_
  })
  first_nonzero
}, .SDcols = LEVEL_COLS]

dt[, num_grade := match(grade, CEFR_LABELS)]

# Map TreeTagger tags to UPOS.
# TreeTagger French tagset uses: NOM, VER, ADJ, ADV, PRE, DET:ART, DET:POS,
# PRO:*, KON, PUN, NUM, ABR, NAM, etc.
tt_to_upos <- function(tag) {
  tag <- sub(":.*", "", tag)  # strip subtype (e.g. DET:ART → DET)
  dplyr::case_match(
    tag,
    c("NOM", "NOM:propre") ~ "NOUN",
    "NAM"                  ~ "PROPN",
    c("VER", "AUX")        ~ "VERB",
    "ADJ"                  ~ "ADJ",
    "ADV"                  ~ "ADV",
    c("PRE", "PRE:det")    ~ "ADP",
    c("DET", "DET:ART", "DET:POS") ~ "DET",
    c("PRO", "PRO:PER", "PRO:DEM", "PRO:REL", "PRO:IND", "PRO:POS") ~ "PRON",
    c("KON", "CONJ")       ~ "CCONJ",
    "NUM"                  ~ "NUM",
    "PUN"                  ~ "PUNCT",
    "INT"                  ~ "INTJ",
    .default               = "X"
  )
}

# Use dplyr::case_match if available, otherwise fall back to a lookup table.
if (!requireNamespace("dplyr", quietly = TRUE)) {
  tag_map <- c(
    NOM="NOUN", NAM="PROPN", VER="VERB", AUX="VERB", ADJ="ADJ", ADV="ADV",
    PRE="ADP", DET="DET", PRO="PRON", KON="CCONJ", CONJ="CCONJ",
    NUM="NUM", PUN="PUNCT", INT="INTJ"
  )
  dt[, upos := {
    bare <- sub(":.*", "", pos_raw)
    mapped <- tag_map[bare]
    ifelse(is.na(mapped), "X", mapped)
  }]
} else {
  dt[, upos := tt_to_upos(sub(":.*", "", pos_raw))]
}

dt <- dt[nzchar(word), .(word, upos, freq_u, freq_log10, grade, num_grade)]
dt <- dt[!is.na(freq_u)]

setkey(dt, word, upos)
message("Rows: ", nrow(dt), " (", uniqueN(dt$word), " unique word forms)")


# ── 3) Save ───────────────────────────────────────────────────────────────────

saveRDS(dt, OUT_PATH)
message("Saved: ", OUT_PATH)
print(dt[word %in% c("maison", "aimer", "beau")])
print(head(dt, 5))
