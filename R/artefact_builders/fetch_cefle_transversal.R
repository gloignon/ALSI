# Fetch the CEFLE transversal corpus from Lund University's project site.
#
# Source:
#   Corpus Ecrit de Francais Langue Etrangere (CEFLE)
#   https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/
#
# Output:
#   demo_corpora/cefle_corpus_texts.csv
#
# Run from the repository root:
#   Rscript R/artefact_builders/fetch_cefle_transversal.R

library(data.table)
library(rvest)
library(xml2)

OUT_CSV  <- "demo_corpora/cefle_corpus_texts.csv"

stages <- data.table(
  stage_order = 1:5,
  stage = c("a", "b", "c", "d", "control"),
  stage_label = c("Stade A", "Stade B", "Stade C", "Stade D", "Controle"),
  level = c("initial", "post_initial", "intermediate", "low_advanced", "native_control"),
  expected_n = c(16L, 30L, 30L, 30L, 30L),
  page_url = c(
    "https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/stade-a/",
    "https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/stade-b/",
    "https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/stade-c/",
    "https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/stade-d/",
    "https://projekt.ht.lu.se/cefle/textes/le-sous-corpus-transversal/controle/"
  )
)


clean_text <- function(x) {
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("[ \t]+\n", "\n", x, perl = TRUE)
  trimws(x)
}


clean_preview_text <- function(x) {
  x <- clean_text(x)
  # TYPO3's upload-list markup can leak the next linked filename into a preview.
  gsub("[[:space:]]+[^[:space:]]+\\.txt$", "", x, perl = TRUE, ignore.case = TRUE)
}


normalize_unicode <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trans_nfc(x)
  } else {
    x
  }
}


fetch_stage_index <- function(stage_row) {
  message("Indexing ", stage_row$stage_label, ": ", stage_row$page_url)

  doc <- read_html(stage_row$page_url, encoding = "UTF-8")
  nodes <- html_elements(doc, "article li")
  nodes <- nodes[grepl("\\.txt$", html_attr(html_element(nodes, "a"), "href"), ignore.case = TRUE)]

  if (!length(nodes)) {
    stop("No .txt links found on stage page: ", stage_row$page_url)
  }

  dt <- rbindlist(lapply(seq_along(nodes), function(i) {
    link <- html_element(nodes[[i]], "a")
    href <- html_attr(link, "href")
    file_name <- html_text2(html_element(nodes[[i]], ".ce-uploads-fileName"))
    preview <- html_text2(html_element(nodes[[i]], ".ce-uploads-description"))

    if (is.na(file_name) || !nzchar(file_name)) {
      file_name <- utils::URLdecode(basename(href))
    }

    pseudonym <- sub("\\.txt$", "", file_name, ignore.case = TRUE)

    data.table(
      stage = stage_row$stage,
      stage_order = stage_row$stage_order,
      stage_label = stage_row$stage_label,
      level = stage_row$level,
      expected_n = stage_row$expected_n,
      within_stage_id = i,
      pseudonym = pseudonym,
      html_preview = clean_preview_text(preview)
    )
  }), use.names = TRUE)

  if (nrow(dt) != stage_row$expected_n) {
    stop(
      sprintf(
        "%s expected %d texts but found %d. The CEFLE page structure may have changed.",
        stage_row$stage_label, stage_row$expected_n, nrow(dt)
      )
    )
  }

  dt
}


index <- rbindlist(lapply(seq_len(nrow(stages)), function(i) {
  fetch_stage_index(stages[i])
}), use.names = TRUE)

dt_cefle_transversal <- copy(index)
dt_cefle_transversal[, text := html_preview]
dt_cefle_transversal[, corpus := "cefle"]
dt_cefle_transversal[, expected_n := NULL]

setcolorder(
  dt_cefle_transversal,
  c(
    "corpus", "stage", "stage_order", "stage_label", "level",
    "within_stage_id", "pseudonym", "text", "html_preview"
  )
)
setorder(dt_cefle_transversal, stage_order, within_stage_id)

dt_csv <- dt_cefle_transversal[, .(
  corpus,
  level = fifelse(stage == "control", "E", toupper(stage)),
  doc_id = normalize_unicode(pseudonym),
  text = normalize_unicode(text)
)]

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(dt_csv, OUT_CSV, row.names = FALSE, fileEncoding = "UTF-8")

message("\nCEFLE transversal summary:")
print(dt_cefle_transversal[, .(
  n_texts = .N
), by = .(stage, stage_label, level)])

message("\nArtifacts written:")
message("  ", OUT_CSV)
