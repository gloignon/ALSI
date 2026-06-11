# Build demo_corpora/alector_corpus.csv from the bundled alector.zip.
#
# Source:
#   Gala et al. (2020). Alector: A French corpus for readability research.
#   LREC. License: CC BY-NC-ND 4.0.
#   https://journals.openedition.org/tipa/5763
#
# The zip ships unmodified .txt files (allowed under CC BY-NC-ND "Share").
# This script produces a derived CSV for local use only — do not redistribute.
#
# Output:
#   demo_corpora/alector_corpus.csv
#
# Run from the repository root:
#   Rscript R/artefact_builders/build_alector_corpus.R

library(data.table)

ZIP_FILE <- "demo_corpora/alector.zip"
OUT_CSV  <- "demo_corpora/alector_corpus.csv"

if (!file.exists(ZIP_FILE)) {
  stop("alector.zip not found at: ", ZIP_FILE)
}

tmp_dir <- tempfile("alector_")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

unzip(ZIP_FILE, exdir = tmp_dir)

txt_files <- sort(list.files(tmp_dir, pattern = "^[0-9]+_(source|target)\\.txt$", full.names = TRUE))

if (!length(txt_files)) {
  stop("No matching .txt files found in zip.")
}

rows <- lapply(txt_files, function(f) {
  base  <- basename(f)
  id    <- as.integer(sub("^([0-9]+)_.*", "\\1", base))
  class <- sub("^[0-9]+_(source|target)\\.txt$", "\\1", base)
  text  <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  data.table(id = id, text = text, class = class)
})

dt <- rbindlist(rows)
setorder(dt, id, class)

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write.csv(dt, OUT_CSV, row.names = FALSE, fileEncoding = "UTF-8")

message("Alector corpus built: ", nrow(dt), " documents (", OUT_CSV, ")")
