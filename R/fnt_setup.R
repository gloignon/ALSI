# fnt_setup.R — lexical-database acquisition & lazy-load layer
#
# ALSI never bundles the restricted lexical databases (NC / research-only /
# unclear licenses). The user fetches their own copy from the original
# provider via the standalone R/artefact_builders/fetch_*.R scripts, and this
# file is the convenience layer on top:
#
#   alsi_setup_databases()  — run the right fetcher(s), showing each license
#                             first and asking before downloading NC resources.
#   alsi_list_databases()   — status table (present/missing, license, command).
#   alsi_db_dir()           — where the .Rds artefacts live.
#   .alsi_load_db(name)     — lazy loader used as a default arg by DB-backed
#                             feature functions; errors with an actionable
#                             message instead of an opaque "file not found".
#   load_demo_corpus()      — one-call demo entry point: unzips the bundled
#                             viki-wiki demo corpus if needed and returns it
#                             as a corpus data.table (via build_corpus()).
#   ensure_viki_wiki_demo_corpus() — lower-level variant returning the corpus
#                             directory, for scripts that need the raw .txt paths.
#
# Design doc: docs/lexical-db-fetch-plan.md

# -- Registry of known databases ----------------------------------------------
# Each entry maps a short key to: the .Rds basename the fetcher produces, the
# fetcher/builder script that produces it, the resource license, whether
# commercial use is permitted, and an optional acquisition note. Output
# basenames are NOT uniform (dt_flp_words.Rds, dt_morpholex_fr.Rds), so they
# are recorded explicitly rather than derived from the key.
.alsi_db_registry <- list(
  manulex   = list(rds = "dt_manulex.Rds",      script = "fetch_manulex.R",
                   license = "CC BY-NC-SA 3.0",                    commercial = FALSE),
  flelex    = list(rds = "dt_flelex.Rds",       script = "fetch_flelex.R",
                   license = "CC BY-NC-SA 4.0",                    commercial = FALSE),
  eqol      = list(rds = "dt_eqol.Rds",         script = "fetch_eqol.R",
                   license = "All rights reserved (research only)", commercial = FALSE),
  lexique3  = list(rds = "dt_lexique3.Rds",     script = "fetch_lexique3.R",
                   license = "CC BY-SA 4.0",                       commercial = TRUE),
  flp       = list(rds = "dt_flp_words.Rds",    script = "build_flp_words.R",
                   license = "CC BY-SA 4.0",                       commercial = TRUE),
  morpholex = list(rds = "dt_morpholex_fr.Rds", script = "build_morpholex_fr.R",
                   license = "CC BY-NC-SA 4.0",                    commercial = FALSE),
  lexconn   = list(rds = "dt_lexconn.Rds",      script = "fetch_lexconn.R",
                   license = "No confirmed public license",        commercial = FALSE,
                   disabled = TRUE)
)

# -- Path resolution ----------------------------------------------------------

#' Directory holding the lexical-database .Rds artefacts.
#'
#' Resolution order:
#'   1. \code{getOption("alsi.db_dir")} if set.
#'   2. Dev fallback: a project-local \code{lexical_dbs/} in the working
#'      directory (the existing ALSI_dev workflow — fetchers write here).
#'   3. \code{tools::R_user_dir("alsi", "data")} for an installed package.
#'
#' @return Character scalar path (not guaranteed to exist yet).
#' @export
alsi_db_dir <- function() {
  opt <- getOption("alsi.db_dir")
  if (!is.null(opt)) return(opt)
  if (dir.exists("lexical_dbs")) return("lexical_dbs")
  return(tools::R_user_dir("alsi", which = "data"))
}

# Directory holding the standalone fetcher/builder scripts (dev default).
.alsi_src_dir <- function() {
  return(getOption("alsi.src_dir", "R/artefact_builders"))
}

# -- Lazy loader (the "magic" default argument) -------------------------------

#' Load a registered lexical database, or stop with an actionable message.
#'
#' Intended as a lazy default for DB-backed feature functions, e.g.
#' \code{add_morpholex_features(dt_corpus, dt_morpholex = .alsi_load_db("morpholex"))}.
#' Because R evaluates default arguments lazily, the database is only read when
#' the caller does not supply one — never at source/package-load time.
#'
#' @param name Registry key (e.g. "manulex", "morpholex").
#' @return A \code{data.table}.
#' @keywords internal
.alsi_load_db <- function(name) {
  reg <- .alsi_db_registry[[name]]
  if (is.null(reg))
    stop("Unknown database '", name, "'. Known databases: ",
         paste(names(.alsi_db_registry), collapse = ", "), call. = FALSE)

  path <- file.path(alsi_db_dir(), reg$rds)
  if (!file.exists(path))
    stop(sprintf(
      "Database '%s' not found at:\n  %s\nRun alsi_setup_databases(\"%s\") to download it.",
      name, path, name), call. = FALSE)

  obj <- readRDS(path)
  data.table::setDT(obj)
  return(obj)
}

# -- Status table -------------------------------------------------------------

#' List known lexical databases with present/missing status.
#'
#' @return (invisibly) a \code{data.frame} of the registry with a \code{present}
#'   column. Also prints a readable summary.
#' @export
alsi_list_databases <- function() {
  dir <- alsi_db_dir()
  rows <- lapply(names(.alsi_db_registry), function(name) {
    reg <- .alsi_db_registry[[name]]
    data.frame(
      database   = name,
      present    = file.exists(file.path(dir, reg$rds)),
      license    = reg$license,
      commercial = ifelse(isTRUE(reg$commercial), "yes", "no"),
      command    = sprintf("alsi_setup_databases(\"%s\")", name),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message("Lexical databases — directory: ", dir)
  print(out, row.names = FALSE)
  return(invisible(out))
}

# -- Fetch / setup ------------------------------------------------------------

# Run one fetcher script (standalone, as documented) and place its output in
# alsi_db_dir(). The scripts write to a project-relative lexical_dbs/; in dev
# that is already alsi_db_dir() and the copy is a no-op, in package mode the
# produced file is copied into the user data directory.
.alsi_run_fetcher <- function(reg) {
  script <- file.path(.alsi_src_dir(), reg$script)
  if (!file.exists(script))
    stop("Fetcher script not found: ", script,
         "\nSet options(alsi.src_dir=) to point at R/artefact_builders/.",
         call. = FALSE)

  status <- system2("Rscript", shQuote(script))
  if (!identical(status, 0L))
    stop("Fetcher '", reg$script, "' exited with status ", status, call. = FALSE)

  produced <- file.path("lexical_dbs", reg$rds)
  target   <- file.path(alsi_db_dir(), reg$rds)
  if (!file.exists(produced))
    stop("Fetcher '", reg$script, "' ran but produced no ", produced, call. = FALSE)
  if (normalizePath(dirname(produced), mustWork = FALSE) !=
      normalizePath(alsi_db_dir(),     mustWork = FALSE)) {
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    file.copy(produced, target, overwrite = TRUE)
  }
  return(invisible(TRUE))
}

#' Download and preprocess one or more lexical databases.
#'
#' Resources are fetched from their original providers by the standalone
#' \code{R/artefact_builders/} scripts; ALSI never redistributes them. NC and
#' research-only resources display their license and (in interactive sessions
#' with \code{ask = TRUE}) require confirmation before download. LexConn has no
#' confirmed public license and fetching is disabled pending a response from
#' the author.
#'
#' @param dbs Character vector of registry keys, or "all".
#' @param ask Logical; show license and confirm before downloading restricted
#'   (non-commercial / research-only) resources. Defaults to \code{interactive()}.
#' @param overwrite Logical; re-fetch even if the .Rds already exists.
#'   Default \code{FALSE} (already-present databases are skipped).
#' @return (invisibly) character vector of the database keys actually fetched.
#' @export
alsi_setup_databases <- function(dbs = "all", ask = interactive(),
                                 overwrite = FALSE) {
  if (identical(dbs, "all")) dbs <- names(.alsi_db_registry)
  unknown <- setdiff(dbs, names(.alsi_db_registry))
  if (length(unknown))
    stop("Unknown database(s): ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(.alsi_db_registry), collapse = ", "),
         call. = FALSE)

  dir <- alsi_db_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fetched <- character(0)

  for (name in dbs) {
    reg  <- .alsi_db_registry[[name]]
    path <- file.path(dir, reg$rds)

    if (file.exists(path) && !overwrite) {
      message("[", name, "] already present — skipping (overwrite = TRUE to re-fetch).")
      next
    }

    # LexConn: fetching disabled pending a license answer from the author —
    # see docs/licensing_and_resource_distribution.md §4.5.
    if (isTRUE(reg$disabled)) {
      warning("[", name, "] fetching is disabled (", reg$license,
              "). Pending confirmation from the author.",
              call. = FALSE)
      next
    }

    # License display + acknowledgment for restricted resources.
    if (!isTRUE(reg$commercial)) {
      message("[", name, "] license: ", reg$license,
              " — non-commercial / restricted. Commercial use requires the ",
              "author's permission.")
      if (isTRUE(ask) && interactive()) {
        ans <- readline(sprintf("Download '%s' under these terms? [y/N] ", name))
        if (!tolower(trimws(ans)) %in% c("y", "yes")) {
          message("[", name, "] skipped by user.")
          next
        }
      }
    }

    message("[", name, "] fetching via ", reg$script, " …")
    .alsi_run_fetcher(reg)
    fetched <- c(fetched, name)
    message("[", name, "] done -> ", path)
  }

  return(invisible(fetched))
}

# -- Demo corpus ---------------------------------------------------------------

#' Ensure the viki-wiki demo corpus is unzipped and return its directory.
#'
#' The paired Vikidia/Wikipedia demo corpus ships as
#' \code{demo_corpora/viki_wiki.zip} (CC BY-SA; bundled deliberately — see
#' docs/licensing_and_resource_distribution.md §4.6). The extracted \code{.txt}
#' directory is gitignored, so it is rebuilt from the zip on first use. Demos
#' call this before \code{build_corpus()}.
#'
#' @param corpus_dir Directory the .txt files are expected in / extracted to.
#' @param zip_path Bundled zip to extract when the directory is missing.
#' @return Character scalar: \code{corpus_dir}.
#' @export
ensure_viki_wiki_demo_corpus <- function(
  corpus_dir = file.path("demo_corpora", "viki_wiki"),
  zip_path = file.path("demo_corpora", "viki_wiki.zip")
) {
  txt_files <- list.files(corpus_dir, pattern = "\\.txt$", full.names = TRUE)
  if (dir.exists(corpus_dir) && length(txt_files) > 0L) {
    return(corpus_dir)
  }

  if (!file.exists(zip_path)) {
    stop(
      "Viki-Wiki demo corpus not found. Expected either:\n",
      "  - ", corpus_dir, "/\n",
      "  - ", zip_path
    )
  }

  message("Unzipping Viki-Wiki demo corpus from ", zip_path)
  unzip(zip_path, exdir = dirname(corpus_dir))

  txt_files <- list.files(corpus_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(txt_files) == 0L) {
    stop("Unzipped Viki-Wiki corpus, but no .txt files were found in ", corpus_dir)
  }

  return(corpus_dir)
}

#' Ensure the bundled ALECTOR demo corpus is unzipped; return its directory.
#'
#' The ALECTOR corpus (Gala et al., 2020) ships as
#' \code{demo_corpora/alector.zip}: unmodified source \code{.txt} files,
#' redistributed under the CC BY-NC-ND 4.0 "Share" grant (see
#' docs/licensing_and_resource_distribution.md §4.0). The extracted directory
#' is gitignored and rebuilt from the zip on first use. Derived artefacts
#' (CSVs, parses) are built locally and never redistributed.
#'
#' @param corpus_dir Directory the .txt files are expected in / extracted to.
#' @param zip_path Bundled zip to extract when the directory is missing.
#' @return Character scalar: \code{corpus_dir}.
#' @export
ensure_alector_demo_corpus <- function(
  corpus_dir = file.path("demo_corpora", "alector"),
  zip_path = file.path("demo_corpora", "alector.zip")
) {
  txt_files <- list.files(corpus_dir, pattern = "\\.txt$", full.names = TRUE)
  if (dir.exists(corpus_dir) && length(txt_files) > 0L) {
    return(corpus_dir)
  }

  if (!file.exists(zip_path)) {
    stop(
      "ALECTOR demo corpus not found. Expected either:\n",
      "  - ", corpus_dir, "/\n",
      "  - ", zip_path
    )
  }

  message(
    "Unzipping ALECTOR demo corpus from ", zip_path,
    " (Gala et al., 2020; CC BY-NC-ND 4.0 — non-commercial use only)"
  )
  # Unlike viki_wiki.zip, the .txt files sit at the zip root, so extract
  # straight into the target directory.
  unzip(zip_path, exdir = corpus_dir)

  txt_files <- list.files(corpus_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(txt_files) == 0L) {
    stop("Unzipped ALECTOR corpus, but no .txt files were found in ", corpus_dir)
  }

  return(corpus_dir)
}

#' Load the bundled demo corpus in one call.
#'
#' Convenience entry point for the demos: determines internally whether the
#' bundled zip still needs unzipping, then reads the texts with
#' \code{build_corpus()}. Requires \code{R/fnt_corpus.R} to be sourced (or the
#' package to be loaded).
#'
#' @param ... Passed on to \code{build_corpus()} (e.g. \code{verbose},
#'   \code{encoding}).
#' @return A corpus \code{data.table} with one row per document.
#' @export
load_demo_corpus <- function(...) {
  if (!exists("build_corpus")) {
    stop(
      "load_demo_corpus() needs build_corpus(). ",
      "Run source(\"R/fnt_corpus.R\", encoding = \"UTF-8\") first.",
      call. = FALSE
    )
  }
  corpus_dir <- ensure_viki_wiki_demo_corpus()
  return(build_corpus(corpus_dir, ...))
}
