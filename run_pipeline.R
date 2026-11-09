# ALSI end-to-end demo pipeline
#
# This script runs the canonical pipeline in main.R: it loads and parses the
# bundled Viki-Wiki corpus, computes the available ALSI feature families, and
# saves the resulting RDS artefacts in out/.
#
# Run from the ALSI repository root:
#   cd /Volumes/Shared/ALSI_dev
#   Rscript run_pipeline.R
#
# Python-backed features are managed by reticulate::py_require(). Do not
# activate a virtual environment or set RETICULATE_PYTHON.

# 1) Check the project location ----

required_paths <- c(
  "main.R",
  file.path("R", "fnt_setup.R"),
  file.path("R", "fnt_corpus.R")
)

missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0L) {
  stop(
    "Run this script from /Volumes/Shared/ALSI_dev. Missing: ",
    paste(missing_paths, collapse = ", "),
    call. = FALSE
  )
}

# 2) Prepare the output directory ----

dir.create("out", recursive = TRUE, showWarnings = FALSE)

# 3) Run the canonical pipeline ----
#
# main.R owns the pipeline itself and sources each feature module it uses.
# Keeping that logic in one place prevents this runner from drifting out of
# sync as feature families are added or removed.

started_at <- Sys.time()
message("Starting the ALSI end-to-end pipeline...")

source("main.R", encoding = "UTF-8")

elapsed_minutes <- as.numeric(
  difftime(Sys.time(), started_at, units = "mins")
)

message(
  "ALSI pipeline complete in ",
  format(round(elapsed_minutes, 1), nsmall = 1),
  " minutes. Outputs are in out/."
)
