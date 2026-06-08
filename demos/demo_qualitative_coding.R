# ALSI Demo: LLM-assisted qualitative coding with gold-standard validation
#
# Implements the chain-of-thought (CoT) per-code workflow from:
#   Dunivin (2025). Scaling hermeneutics. EPJ Data Science, 14:28.
#   doi:10.1140/epjds/s13688-025-00548-8
#
# Dataset: 111 NYT passages about W.E.B. Du Bois with human gold-standard
# codes (9 categories, this demo uses 2). From Dunivin's OSF archive: osf.io/k4fg9
#
# What this demo does:
#
# Qualitative coding is the task of labelling text passages with thematic
# categories from a codebook — traditionally done by trained human coders.
# This demo shows ALSI's LLM-assisted alternative and checks whether it
# can approximate human judgment by replicating a slice of Dunivin's
# published benchmark end-to-end:
#
#   2. Loads Dunivin's gold-standard dataset: NYT passages about W.E.B. Du
#      Bois, each hand-coded by humans across 9 thematic categories.
#   3. Defines a codebook for 2 of those categories (Scholar, Activist),
#      using Dunivin's LLM-adapted definitions from the paper appendix.
#   4. Builds an Ollama-backed llm_fn (system + user prompt -> character reply).
#   5. Codes N_PASSAGES passages against both categories via code_corpus(),
#      one independent LLM call per passage x code (the "per-code" approach).
#   6. Computes Cohen's kappa between the LLM codes and the human gold
#      standard via compute_kappa(), to assess intercoder reliability.
#
# Prerequisites:
#   - Ollama installed and running (https://ollama.com)
#   - demos/data/qualitative_coding/*.csv downloaded (see README)


# 1) Setup ----

library(data.table)

source("R/fnt_ollama.R",             encoding = "UTF-8")
source("R/fnt_qualitative_coding.R", encoding = "UTF-8")
source("R/fnt_utility.R",            encoding = "UTF-8")

MODEL      <- "gemma4:31b-cloud" # this will use a freely available cloud model, change this if you have 
                                # a different model
N_PASSAGES <- 20   # set to 111 to replicate Dunivin's full gold standard


# 2) Load data ----

dt_passages <- fread("demos/data/qualitative_coding/passages.csv")
dt_gold_wide <- fread("demos/data/qualitative_coding/gold_standard_coding.csv")

# The gold standard is wide: one column per code, 1 = applies, NA = does not.
# Subset to passages that appear in both files.
code_cols <- c("Scholar", "Activist", "Monumental Memorialization",
               "Mention of Scholarly Work", "Social/Political Advocacy",
               "Coalition Building", "Out of the Mouth of Academics",
               "Out of the Mouth of Activists", "Collective Synecdoche")

# Rename the row index column for clarity
setnames(dt_gold_wide, "id", "passage_id")

# Recode: 1 -> TRUE, NA -> FALSE
for (col in code_cols) {
  set(dt_gold_wide, j = col, value = !is.na(dt_gold_wide[[col]]))
}

# Pivot to long format for compute_kappa()
dt_gold <- melt(
  dt_gold_wide[, c("passage_id", "passage", code_cols), with = FALSE],
  id.vars      = c("passage_id", "passage"),
  measure.vars = code_cols,
  variable.name = "code",
  value.name    = "applies"
)

# Use only the first N_PASSAGES from the gold standard
passage_ids <- dt_gold_wide$passage_id[seq_len(N_PASSAGES)]
dt_sample   <- dt_gold_wide[passage_id %in% passage_ids]

message(sprintf("Loaded %d passages with %d codes each.", N_PASSAGES, length(code_cols)))


# 3) Define the codebook ----
#
# These are Dunivin's LLM-adapted definitions from the paper appendix.
# They differ from the original human-coder definitions: they are more
# explicit, avoid ambiguous pronouns, and include prohibitions where needed.

codebook <- data.table(
  code = c("Scholar", "Activist"),
  definition = c(
    paste(
      "Applies when Du Bois is described as a scholar or intellectual,",
      "especially in connection to Black politics, racial identity, or social theory.",
      "When Du Bois is invoked through his ideas on social theory, classify as Scholar,",
      "not Activist, unless it is a call to action, related to his organizing, or other",
      "non-scholarly political activity.",
      "Do not apply when Du Bois is merely the focus of historical and academic study",
      "or his scholarship is only implied by loose connections to other scholars."
    ),
    paste(
      "Apply when Du Bois is explicitly called an 'activist' or 'leader',",
      "or when his political or social activism is either explicitly noted or",
      "clearly implied through context.",
      "Examples: being mentioned in the context of leadership, activism, developing",
      "activist organizations, giving public speeches, participating in meetings with",
      "politicians and organizers, running for office, or promoting a candidate,",
      "organization, or initiative."
    )
  )
)

message("Codebook:")
print(codebook[, .(code, chars = nchar(definition))])


# 4) Build the Ollama llm_fn backend ----
#
# llm_fn must match the interface expected by fnt_qualitative_coding.R:
#   function(system_prompt, user_prompt) -> character
#
# We use temperature = 0 (deterministic) and a generous context window,
# following Dunivin's API settings (Sect. 3).

llm_fn <- function(system_prompt, user_prompt) {
  .ollama_chat(
    model        = MODEL,
    system_prompt = system_prompt,
    user_prompt   = user_prompt,
    options      = list(temperature = 0, num_ctx = 2048),
    endpoint     = "http://localhost:11434"
  )
}

# Smoke test
message("Smoke test ...")
test_resp <- llm_fn("Reply with one word only.", "What colour is the sky?")
message("  Response: ", test_resp)
if (!nzchar(trimws(test_resp))) stop("Ollama not responding. Is it running?")


# 5) Apply codes to passages ----
#
# code_corpus() applies each code to each passage independently (per-code
# approach). Progress is printed to stderr so it can be suppressed.
#
# Output is saved to CSV so the loop can be resumed if interrupted.

out_file <- "out/demo_qualitative_coding_results.csv"
dir.create("out", showWarnings = FALSE)

passages_vec <- setNames(dt_sample$passage, dt_sample$passage_id)

message(sprintf("\nCoding %d passages × %d codes = %d LLM calls ...\n",
                N_PASSAGES, nrow(codebook), N_PASSAGES * nrow(codebook)))

dt_coded <- code_corpus(
  passages = data.table(id = dt_sample$passage_id, text = dt_sample$passage),
  codebook = codebook,
  llm_fn   = llm_fn,
  verbose  = TRUE
)

fwrite(dt_coded, out_file)
message("\nResults saved to: ", out_file)


# 6) Validate against gold standard ----
#
# compute_kappa() compares LLM applies to human gold standard.
# Cohen's κ > 0.60 = substantial agreement, > 0.75 = excellent.

dt_gold_sample <- dt_gold[
  passage_id %in% passage_ids & code %in% codebook$code
]

dt_kappa <- compute_kappa(dt_coded, dt_gold_sample[, .(passage_id, code, applies)])

message("\nIntercoder reliability (LLM vs. human gold standard):")
print(dt_kappa)
