# Demo: Ollama API — generate, summarize, and verify from summaries
#
# Three-pass workflow:
#   Pass 1: For each wiki text, ask a local LLM to produce 4 true statements.
#   Pass 2: For each wiki text, produce a short summary.
#   Pass 3: For each statement, we verify it against the summary (not the full text).
#
# The point: can the model recover the truth of statements from lossy summaries?
#
# Each row is a stateless query (no conversation memory between rows).
# Results are saved incrementally to CSV; re-running resumes where it left off.
#
# Prerequisite:
#   - Ollama running locally (https://ollama.com)
#   - Model pulled: run `ollama pull gemma3:4b` in a terminal.

# 0) Setup ----

library(data.table)

source("R/fnt_corpus.R", encoding = "UTF-8")
source("R/fnt_ollama.R", encoding = "UTF-8")


# 1) Smoke test — verify Ollama is responding ----

dt_test <- data.table(question = "Quelle est la capitale de la France ?")
dt_test <- ollama_generate(
  data                 = dt_test,
  user_prompt_template = "{question}",
  system_prompt        = "Réponds en une phrase.",
  model                = "gemma3:4b",
  temperature          = 0,
  num_ctx              = 256,
  force_restart        = TRUE
)
cat("\n--- Smoke test ---\n")
cat("Q:", dt_test$question[1], "\n")
cat("A:", dt_test$ollama_response[1], "\n\n")
if (is.na(dt_test$ollama_response[1]) || !nzchar(dt_test$ollama_response[1])) {
  stop("Smoke test failed: no response from Ollama. Check that the model is running.")
}


# 2) Build a small data frame from wiki texts ----

N_DOCS <- 5

dt_corpus <- build_corpus("demo_corpus")
dt_wiki <- dt_corpus[grepl("^wiki_", doc_id)][1:N_DOCS]

cat(sprintf("Demo corpus: %d documents\n", nrow(dt_wiki)))
print(dt_wiki[, .(doc_id, nchar = nchar(text))])


# 3) Pass 1 — Generate statements from full texts ----

system_prompt <- paste(
  "Voici un texte encyclopédique en français.",
  "Tu dois produire exactement 4 affirmations vraies à propos du texte.",
  "Numérote-les de 1 à 4. Ne produis rien d'autre."
)

dt_statements <- ollama_generate(
  data                 = dt_wiki,
  user_prompt_template = "Voici le texte :\n\n{text}",
  system_prompt        = system_prompt,
  model                = "gemma3:4b",
  temperature          = 0.3,
  num_ctx              = 4096,
  output_file          = "out/demo_ollama_statements.csv",
  force_restart        = TRUE
)

# Parse statements into 4 columns
parse_statements <- function(response) {
  if (is.na(response)) return(rep(NA_character_, 4))
  lines <- unlist(strsplit(response, "\n"))
  vapply(1:4, function(k) {
    pattern <- sprintf("^\\s*\\*{0,2}%d[.):]+\\*{0,2} *", k)
    hit <- grep(pattern, lines, value = TRUE)
    if (length(hit) > 0) trimws(sub(pattern, "", hit[1])) else NA_character_
  }, character(1))
}

parsed <- t(sapply(dt_statements$ollama_response, parse_statements, USE.NAMES = FALSE))
dt_statements[, paste0("statement_", 1:4) := as.data.table(parsed)]

cat("\n--- Parsed statements ---\n")
print(dt_statements[, .(doc_id, statement_1, statement_2, statement_3, statement_4)])


# 4) Pass 2 — Summarize each text ----

dt_summ <- ollama_generate(
  data                 = dt_wiki,
  user_prompt_template = "Voici le texte :\n\n{text}",
  system_prompt        = paste(
    "Tu es un assistant spécialisé en résumé de texte.",
    "On te donne un texte encyclopédique en français.",
    "Produis un résumé en 2 à 3 phrases. Ne produis rien d'autre."
  ),
  model                = "gemma3:4b",
  temperature          = 0.3,
  num_ctx              = 4096,
  output_file          = "out/demo_ollama_summaries.csv",
  force_restart        = TRUE
)

cat("\n--- Summaries ---\n\n")
for (i in seq_len(nrow(dt_summ))) {
  resp <- dt_summ$ollama_response[i]
  if (is.na(resp) || !nzchar(resp)) resp <- "[no response]"
  cat(sprintf("=== %s ===\n%s\n\n", dt_summ$doc_id[i], resp))
}


# 5) Pass 3 — Evaluate statements against summaries ----
# The key test: statements were generated from full texts, but verified against
# summaries only. This measures how well the summary preserves factual content.

# Reshape to one row per statement and join summaries
dt_eval <- melt(
  dt_statements[, .(doc_id, statement_1, statement_2, statement_3, statement_4)],
  id.vars    = "doc_id",
  measure.vars = paste0("statement_", 1:4),
  variable.name = "statement_num",
  value.name    = "statement"
)[!is.na(statement)]

dt_eval[, statement_num := as.integer(gsub("statement_", "", statement_num))]

# Attach summaries (not full texts)
dt_eval <- merge(dt_eval, dt_summ[, .(doc_id, summary = ollama_response)], by = "doc_id")

dt_eval <- ollama_generate(
  data                 = dt_eval,
  user_prompt_template = paste0(
    "Résumé du texte source :\n{summary}\n\n",
    "Affirmation :\n{statement}"
  ),
  system_prompt        = paste(
    "Tu es un vérificateur d'affirmations.",
    "On te donne le résumé d'un texte source et une affirmation.",
    "Tu dois répondre UNIQUEMENT par VRAI ou FAUX.",
    "VRAI si l'affirmation est cohérente avec le résumé, FAUX sinon.",
    "Ne produis rien d'autre qu'un seul mot : VRAI ou FAUX."
  ),
  model                = "gemma3:4b",
  temperature          = 0.0,
  num_ctx              = 4096,
  output_file          = "out/demo_ollama_eval.csv",
  force_restart        = TRUE
)


# 6) Parse verdicts and report ----

dt_eval[, verdict := fcase(
  grepl("VRAI", ollama_response, ignore.case = TRUE), "VRAI",
  grepl("FAUX", ollama_response, ignore.case = TRUE), "FAUX",
  default = "UNCLEAR"
)]

cat("\n--- Evaluation results (statements vs summaries) ---\n\n")
print(dt_eval[, .(doc_id, statement_num, statement, verdict)])

n_expected  <- nrow(dt_statements) * 4
n_parsed    <- sum(!is.na(unlist(dt_statements[, paste0("statement_", 1:4), with = FALSE])))
n_vrai      <- sum(dt_eval$verdict == "VRAI")
n_faux      <- sum(dt_eval$verdict == "FAUX")
n_unclear   <- sum(dt_eval$verdict == "UNCLEAR")
n_evaluated <- nrow(dt_eval)

cat("\n--- Summary ---\n\n")
cat(sprintf("Statements parsed:     %d/%d (%.0f%%)\n",
            n_parsed, n_expected, 100 * n_parsed / n_expected))
cat(sprintf("Recovered from summary (VRAI):  %d/%d (%.0f%%)\n",
            n_vrai, n_evaluated, 100 * n_vrai / n_evaluated))
cat(sprintf("Lost in summary (FAUX):         %d/%d\n", n_faux, n_evaluated))
cat(sprintf("Unclear:                        %d/%d\n", n_unclear, n_evaluated))
