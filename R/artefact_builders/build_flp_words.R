# Build the French Lexicon Project (FLP) words database for use in ALSI.
#
# Output:
#   lexical_dbs/dt_flp_words.Rds   — data.table keyed on `word`
#
# The pre-built file ships with ALSI. If it is absent (e.g. removed for IP
# reasons) run this script once to regenerate it from the upstream source.
#
# Source:
#   Ferrand et al. (2010). The French Lexicon Project: Lexical decision data
#   for 38,840 French words and 38,840 pseudowords. Behavior Research Methods,
#   42(2), 488–496. https://doi.org/10.3758/BRM.42.2.488
#   Data: http://www.lexique.org/databases/FrenchLexiconProject/FLP.words.csv
#
# If the download fails (lexique.org is occasionally unreachable), obtain
# FLP.words.csv manually from the project website and place it at:
#   lexical_dbs/raw/FLP.words.csv
# The script will use that local copy automatically.
#
# Columns retained in the output:
#   word       — lowercase word form
#   rt         — mean lexical decision RT (ms), outlier-trimmed
#   rt_z       — mean z-scored RT (subject-normalised)
#   err        — error rate (proportion of incorrect responses)
#   rt_sd      — SD of RT across trials
#   n_trials   — number of valid trials retained
#   n_letters  — orthographic length
#   n_syllables — syllable count
#
library(data.table)

# Paths are relative to the repo root. In RStudio, open the .Rproj file and
# the working directory is set automatically. From the command line, run as:
#   Rscript R/artefact_builders/build_flp_words.R
REMOTE_URL <- "http://www.lexique.org/databases/FrenchLexiconProject/FLP.words.csv"
LOCAL_RAW  <- "lexical_dbs/raw/FLP.words.csv"
OUT_PATH   <- "lexical_dbs/dt_flp_words.Rds"


# ── Load ──────────────────────────────────────────────────────────────────────

if (file.exists(LOCAL_RAW)) {
  message(sprintf("Loading local copy: %s", LOCAL_RAW))
  dt_raw <- fread(LOCAL_RAW, encoding = "UTF-8", showProgress = FALSE)
} else {
  message(sprintf("Downloading FLP words from %s ...", REMOTE_URL))
  dt_raw <- tryCatch(
    fread(REMOTE_URL, encoding = "UTF-8", showProgress = FALSE),
    error = function(e) {
      stop(
        "Download failed: ", conditionMessage(e), "\n\n",
        "Obtain FLP.words.csv manually from:\n",
        "  https://sites.google.com/site/frenchlexicon/\n",
        "or via the openlexicon Makefile at:\n",
        "  https://github.com/chrplr/openlexicon/tree/master/datasets-info/FrenchLexiconProject\n",
        "Then place the file at: ", LOCAL_RAW
      )
    }
  )
}

message(sprintf("Raw rows: %d  |  columns: %s",
                nrow(dt_raw), paste(names(dt_raw), collapse = ", ")))


# ── Select and rename ─────────────────────────────────────────────────────────

dt <- dt_raw[, .(
  word        = tolower(item),
  rt          = as.numeric(rt),
  rt_z        = as.numeric(rtz),
  err         = as.numeric(err),
  rt_sd       = as.numeric(sd),
  n_trials    = as.integer(nused),
  n_letters   = as.integer(nletters),
  n_syllables = as.integer(nsyllables)
)]

dt <- unique(dt, by = "word")
setkey(dt, word)

message(sprintf("Unique word forms: %d", nrow(dt)))
message(sprintf("  rt range:  %.0f – %.0f ms", min(dt$rt, na.rm = TRUE), max(dt$rt, na.rm = TRUE)))
message(sprintf("  err range: %.3f – %.3f",    min(dt$err, na.rm = TRUE), max(dt$err, na.rm = TRUE)))


# ── Save ──────────────────────────────────────────────────────────────────────

dir.create(dirname(OUT_PATH), showWarnings = FALSE, recursive = TRUE)
saveRDS(dt, OUT_PATH)
message(sprintf("\nWritten: %s", OUT_PATH))
