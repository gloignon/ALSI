# Demo: MetaTaal LEGO brick ordering estimate
#
# In this demo you will:
# 1) run the brick tally on a set of French sentences (a therapy session's
#    worth), producing a per-sentence tally and a total "how many bricks of
#    each kind to order" table;
# 2) draw the ASCII brick layout for every sentence.
#
# Background:
#   MetaTaal (Zwitserlood et al. 2015) is a Dutch syntax-therapy method that
#   represents word classes with colored/sized LEGO bricks: green 1x2
#   determiners, blue 2x4 nouns, red 2x3 verbs, brown 2x2 adjectives, yellow
#   2x2 prepositions, and a grey "bridge" brick for complementizers/relative
#   markers (e.g. French "qui", "que", "dont"). Full brick spec, sourcing,
#   and flagged gaps: vignettes/specs/metataal-lego.Rmd.
#
#   Grey bridges (n_bridge) are counted per physical brick, not per marker
#   word: a center-embedded relative clause (e.g. "Le chien que le garçon
#   nourrit dort.", where the main clause resumes at "dort") needs TWO grey
#   bricks — one to drop into the embedded clause and one to climb back up —
#   whereas a right-branching clause that ends the sentence needs only one
#   (down, no return). See the bridge-counting rule in the spec.
#
#   This demo answers a physical-shopping-list question — given a set of
#   sentences chosen for a therapy session, how many bricks of each color
#   need to be on hand? — and then draws each sentence's ASCII brick layout
#   (which bricks sit on which row).
#
#   Scope: French sentences only (French UD model). count_metataal_bricks()
#   itself is language-agnostic (it only reads upos/dep_rel/feats), but this
#   demo and its toy examples are French-only per current use case.
#
# Parser choice — Trankit throughout:
#   This demo uses ALSI's Trankit backend for every section, not UDPipe. The
#   sentences here include center-embedded relative clauses (e.g. "Le chien
#   que le garçon nourrit dort."), which all three bundled French UDPipe
#   models mis-parse: they make the subject noun the sentence root and tag
#   the trailing matrix verb "dort" as ADJ/xcomp instead of VERB/root. That
#   silently ships one red verb brick as a brown adjective brick -- the
#   n_total happens to match, but the individual brick order is wrong.
#   Trankit parses these correctly (VERB root, PronType=Rel relative pronoun,
#   acl:relcl clause), so the brick counts come out right.
#
# Prerequisites:
# - ALSI's Trankit backend at models/trankit_fr_v1/ (all sections). The setup
#   block below fetches it on first run (~34 MB, one-time).
#
# Last update: 2026-07-07

# 1) Setup ----

library(tidyverse)
library(data.table)

# draw_metataal_bricks() drops embedded clauses one row down, detected via
# split_into_clauses() (fnt_clause_split.R), which itself needs
# add_predicate_flag()/.subj_rels from fnt_syntactic_complexity.R -- source
# order matters here.
source("R/fnt_corpus.R",               encoding = "UTF-8")  # parse_text() multi-backend dispatch
source("R/fnt_setup.R",                encoding = "UTF-8")
source("R/fnt_syntactic_complexity.R", encoding = "UTF-8")
source("R/fnt_clause_split.R",         encoding = "UTF-8")
source("R/fnt_metataal.R",             encoding = "UTF-8")

# Fetch the French Trankit model on first run (see the parser-choice note in
# the header for why Trankit rather than UDPipe). One-time, ~34 MB.
trankit_model_path <- "models/trankit_fr_v1"
if (!dir.exists(file.path(trankit_model_path, "xlm-roberta-base", "customized-mwt"))) {
  message("French Trankit model not found — downloading from GitHub (one-time, ~34 MB)...")
  source("R/artefact_builders/fetch_trankit_models.R")
}

# All example sentences for this demo, in one named vector. Names become
# doc_id, so each sentence can be pulled back out by name later. Swap in the
# real therapy-session set here and every downstream section picks them up.
# sentences <- c(
#   simple      = "Le chat mange la souris.",
#   qui_rel     = "Le renard qui court est rapide.",
#   que_rel     = "Le chien que le garçon nourrit dort.",
#   pomme1      = "Il mange sa pomme dans le jardin",
#   pomme2      = "Il mange la pomme qu'il a cueillie dans le jardin",
#   fille_pp    = "La petite fille regarde le grand chien sous la table.",
#   garcon_ditr = "Le garçon donne le livre à la fille qui lit.",
#   edge1      = "Le livre que Paul a lu plaît.",
#   edge2      = "Marie veut manger la pomme rouge.",
#   edge3      = "L’homme qui parle dort profondément.",
#   virgule1   = "Le chat, qui est noir, dort.",
#   virgule2   = "Les jeunes, qui seront les futurs adultes, sont la solution."
# )

sentences <- c(
  que_rel     = "Le gros chien que le garçon nourrit dort profondément.",
  virgule2   = "Les jeunes, qui seront les futurs adultes, sont la solution.",
  jeunes     = "Les jeunes organisent une fête sensationnelle, mais ils n'ont pas l'autorisation."
) 

# Parse every example sentence in one Trankit call. parse_text() takes the
# named character vector (names become doc_id) and returns a data.table already
# carrying paragraph_id/sentence_id, the columns draw_metataal_bricks() needs
# for embedded-clause detection. Downstream sections select their sentence(s)
# by doc_id from this single parsed table.
parsed <- parse_text(sentences,
                     backend = "trankit", trankit_model = trankit_model_path)


# 2) Per-sentence brick tally + total order ----
#
# count_metataal_bricks() returns one row per doc_id: the per-sentence tally.
# A final summarise gives the pooled "how many bricks of each kind to order"
# table for the whole set.

per_sentence <- count_metataal_bricks(parsed)
message("--- Per-sentence brick tally ---")
print(per_sentence)

message("--- Total bricks to order for this sentence set ---")
brick_order <- per_sentence |>
  summarise(across(starts_with("n_"), sum))
print(brick_order)


# 3) ASCII brick layouts for every sentence ----
#
# draw_metataal_bricks() prints the ASCII layout as a side effect (and returns
# it invisibly), so a bare call per doc_id draws them all in one pass. The
# class-code key (D/N/V/A/P/B/-) is identical for every sentence, so show it
# once up front and pass show_legend = FALSE for the rest of the loop.

for (i in seq_along(sentences)) {
  message("\n--- ASCII brick layout for sentence: ", names(sentences)[i], " ---")
  draw_metataal_bricks(parsed, doc_id = names(sentences)[i],
                       show_legend = FALSE)
  # print empty line
  message("\n")
}
