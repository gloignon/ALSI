# Build French reference burstiness norms from the full wikiviki corpus.
#
# Three norm sets are produced, each saved as a named list(beta, adaptation):
#
#   norms/burstiness_wiki.Rds    hard (original Wikipedia) texts only
#   norms/burstiness_viki.Rds    easy (simplified Vikidia) texts only
#   norms/burstiness_wikiviki.Rds both combined (largest vocabulary coverage)
#
# Input: data/wikiviki_dump_20251215.tsv  (12 221 article pairs)
#
# Runtime: ~15–30 min on a modern Mac (UDPipe on ~24k texts, parser = "none").
# Parsed corpus is cached to norms/wikiviki_parsed_hard.Rds and
# norms/wikiviki_parsed_easy.Rds so re-runs only redo the fitting step.
#
# References:
#   Altmann, Pierrehumbert & Motter (2009), PLoS ONE 4(11): e7678
#   Church & Gale (1995), Natural Language Engineering 1(2): 163-190
#
library(data.table)
library(udpipe)

ROOT <- normalizePath(file.path(
  if (!is.null(sys.frame(1)$ofile)) {
    dirname(sys.frame(1)$ofile)                         # source()
  } else if (requireNamespace("rstudioapi", quietly = TRUE) &&
             rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path) # RStudio Source button
  } else {
    stop("Cannot determine script path. Use source('R/calibration/build_wikiviki_norms.R') or run from RStudio.")
  },
  "../.."
), mustWork = TRUE)
setwd(ROOT)

source("R/fnt_corpus.R",     encoding = "UTF-8")
source("R/fnt_burstiness.R", encoding = "UTF-8")

TSV_PATH <- "data/wikiviki_dump_20251215.tsv"
stopifnot(file.exists(TSV_PATH))


read_wikiviki_tsv <- function(path, which = c("both", "hard", "easy")) {
  which <- match.arg(which)

  # Texts may contain literal tabs, so fread tab-splitting is unreliable.
  # Parse each line with a regex anchored on the known doc_id patterns.
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(lines)]
  lines <- lines[lines != "doc_id_hard\thard_text\tdoc_id_easy\teasy_text"]

  # Greedy .* before \tviki_ finds the rightmost tab boundary.
  pat <- "^(wiki_[^\t]+)\t(.*)\t(viki_[^\t]+)\t(.*)$"
  m   <- regmatches(lines, regexec(pat, lines, perl = TRUE))
  ok  <- lengths(m) == 5L
  if (any(!ok))
    warning(sprintf("%d line(s) did not match the expected format and were skipped", sum(!ok)))
  m <- do.call(rbind, lapply(m[ok], `[`, -1L))

  dt <- data.table(
    doc_id_hard = m[, 1L], hard_text = m[, 2L],
    doc_id_easy = m[, 3L], easy_text = m[, 4L]
  )
  if (which == "hard") return(dt[, .(doc_id = doc_id_hard, text = hard_text)])
  if (which == "easy") return(dt[, .(doc_id = doc_id_easy, text = easy_text)])
  rbind(dt[, .(doc_id = doc_id_hard, text = hard_text)],
        dt[, .(doc_id = doc_id_easy, text = easy_text)])
}

n_cores <- max(1L, parallel::detectCores() - 1L)


# ── helpers ───────────────────────────────────────────────────────────────────

parse_or_load <- function(cache_path, which) {
  if (file.exists(cache_path)) {
    message(sprintf("Loading cached parsed corpus (%s)...", which))
    return(readRDS(cache_path))
  }
  message(sprintf("Parsing %s texts with UDPipe (parser = 'none')...", which))
  dt_txt <- read_wikiviki_tsv(TSV_PATH, which = which)
  message(sprintf("  %d documents", nrow(dt_txt)))
  dt <- parse_text(dt_txt, n_cores = n_cores, parser = "none")
  saveRDS(dt, cache_path)
  message(sprintf("  Cached to %s", cache_path))
  dt
}

fit_norms <- function(dt, label) {
  message(sprintf("\nFitting norms from %s...", label))
  beta  <- compute_burstiness_beta(dt,  min_occurrences = 10L)
  adapt <- compute_adaptation_score(dt, min_docs        =  5L)
  message(sprintf("  beta:       %d words, %.0f%% valid",
                  nrow(beta),  100 * mean(!is.na(beta$beta))))
  message(sprintf("  adaptation: %d words, %.0f%% valid",
                  nrow(adapt), 100 * mean(!is.na(adapt$adaptation))))
  list(beta = beta, adaptation = adapt)
}


# ── 1. Parse each half ────────────────────────────────────────────────────────

dt_hard <- parse_or_load("norms/wikiviki_parsed_hard.Rds", "hard")
dt_easy <- parse_or_load("norms/wikiviki_parsed_easy.Rds", "easy")


# ── 2. Fit and save norm sets ─────────────────────────────────────────────────

norms_wiki    <- fit_norms(dt_hard,                  "wiki (hard texts)")
norms_viki    <- fit_norms(dt_easy,                  "viki (easy texts)")
norms_wikiviki <- fit_norms(rbind(dt_hard, dt_easy), "wikiviki (both)")

saveRDS(norms_wiki,     "norms/burstiness_wiki.Rds")
saveRDS(norms_viki,     "norms/burstiness_viki.Rds")
saveRDS(norms_wikiviki, "norms/burstiness_wikiviki.Rds")

message("\nArtifacts written:")
message("  norms/burstiness_wiki.Rds")
message("  norms/burstiness_viki.Rds")
message("  norms/burstiness_wikiviki.Rds")


# ── 3. Quick sanity check (wikiviki combined) ─────────────────────────────────

message("\nTop 10 most bursty words (wikiviki, β closest to 0):")
print(norms_wikiviki$beta[!is.na(beta), head(.SD, 10L),
                           .SDcols = c("word", "n_occ", "mean_gap", "beta")])

message("\nTop 10 most uniform words (wikiviki, β closest to 1):")
print(norms_wikiviki$beta[!is.na(beta), tail(.SD, 10L),
                           .SDcols = c("word", "n_occ", "mean_gap", "beta")])

message("\nTop 10 highest adaptation scores (wikiviki):")
print(norms_wikiviki$adaptation[!is.na(adaptation), head(.SD, 10L),
                                 .SDcols = c("word", "n_docs", "adaptation")])
