# ALSI Demo: Visualising Dependency Tree Features with Arc Diagrams
#
# Dependency parsing assigns each word a syntactic "head" — the word it
# grammatically depends on.  plot_dependency_arcs() draws these head-dependent
# links as semicircular arcs above the sentence, making structural properties
# directly visible:
#
#   - Arc HEIGHT reflects span (how far apart a word is from its head).
#   - Arc DIRECTION (left vs right) reveals head order:
#       head to the RIGHT of the dependent → head-final arc (e.g. det → noun)
#       head to the LEFT  of the dependent → head-initial arc (e.g. verb → obj)
#   - ARROWHEADS point toward the head token.
#   - A GREEN TRIANGLE marks the ROOT (the verb that heads the whole clause).
#   - RED arcs are CROSSING arcs — they overlap at least one other arc,
#     indicating an "incomplete dependency" in Gibson's Dependency Locality
#     Theory (DLT).
#
# After each plot, sentence_graph_stats() prints the same structure as a
# table of numbers so you can see exactly how the visual maps to the metrics
# that ALSI computes at scale.
#
# Prerequisites: models/french_gsd-remix_3.udpipe
# (run demo_parse_tag.R first only if you want the corpus sections)


# 1) Setup ----

library(data.table)
library(tidyverse)
library(udpipe)

source("R/fnt_heights.R", encoding = "UTF-8")

udmodel <- udpipe_load_model(file = "models/french_gsd-remix_3.udpipe")

# Helper: parse one sentence and return the raw UDPipe data.table
parse_sent <- function(text) {
  as.data.table(udpipe(x = text, object = udmodel))
}


# 2) Reading the arc diagram ----
#
# We start with the simplest possible French sentence.
# Before looking at the numbers, spend a moment reading the plot:
#
#   - Every word sits on the horizontal axis.
#   - Each arc connects a DEPENDENT (tail of the arrowhead) to its HEAD
#     (tip of the arrowhead).
#   - The ROOT verb is marked with a green triangle — it has no arc of its own
#     because it heads the whole clause.
#   - Taller arcs signal longer-range dependencies (farther head-dependent pairs).
#   - The label on each arc is the UD dependency relation.

dt_simple <- parse_sent("Le chat mange le poisson.")

message("Sentence: 'Le chat mange le poisson.'")
print(dt_simple[, .(token, upos, dep_rel, head_token_id)])

par(mfrow = c(1, 1))
plot_dependency_arcs(
  dt_simple,
  title = "Arc diagram — 'Le chat mange le poisson.'",
  show_deprel = TRUE,
  cex_text = 0.85
)
message("(Close or scroll past the plot to continue.)")

# The corresponding numeric summary for this sentence.
# max_path = the longest root-to-leaf path (tree depth).
# avg_dependency_depth = mean depth of all tokens.
# branching_factor = average number of dependents per non-leaf node.
stats_simple <- sentence_graph_stats(dt_simple, verbose = TRUE)


# 3) Shallow tree vs deep tree ----
#
# Adding a relative clause ("qui est entrée par le trou") embeds a second
# predicate inside the main clause.  The tree grows taller: more arc layers
# pile up and the numbers rise accordingly.
#
# In a shallow tree (left plot), arcs barely stack.
# In a deep tree (right plot), arcs nest inside one another —
# each new level of embedding adds a ring.

dt_relative <- parse_sent(
  "Le chat mange la petite souris verte qui est entrée par le trou."
)

# The doubly-embedded relative clause pushes depth further.
dt_embedded <- parse_sent(
  "Le livre que l'auteur dont tout le monde parle a écrit est fascinant."
)

par(mfrow = c(1, 2), mar = c(4, 0.5, 3, 0.5))
plot_dependency_arcs(
  dt_simple,
  title = "Shallow tree (simple clause)",
  show_deprel = FALSE,
  cex_text = 0.75
)
plot_dependency_arcs(
  dt_embedded,
  title = "Deep tree (doubly embedded relative)",
  show_deprel = FALSE,
  cex_text = 0.65
)
par(mfrow = c(1, 1))

# Numeric comparison: watch max_path and avg_dependency_depth climb.
message("--- shallow sentence ---")
sentence_graph_stats(dt_simple,    verbose = TRUE)
message("--- doubly-embedded relative clause ---")
sentence_graph_stats(dt_embedded,  verbose = TRUE)


# 4) Head direction: head-final vs head-initial arcs ----
#
# In the UD annotation scheme used here, an arc whose arrowhead points to the
# RIGHT of the dependent is called HEAD-FINAL (the head follows its dependent).
# An arc pointing to the LEFT is HEAD-INITIAL (the head precedes its dependent).
#
# French is a mixed language:
#   - Determiners and adjectives precede their noun head → head-final arcs
#     (e.g. "la" → "maison", "grande" → "maison")
#   - Objects follow their verb head → head-initial arcs
#     (e.g. "mange" → "poisson")
#
# The ratio of head-final arcs to all arcs is a proxy for how right-branching
# (English-like) vs left-branching (Japanese-like) a text is.
# head_final_initial() gives this ratio per token, per sentence.

dt_adj <- parse_sent(
  "La grande maison rouge appartient à un vieux médecin."
)

plot_dependency_arcs(
  dt_adj,
  title = "Head direction mix in a nominal sentence",
  show_deprel = TRUE,
  cex_text = 0.75
)
# Arcs pointing right (e.g. det/amod → noun): head-FINAL.
# Arcs pointing left (e.g. noun → oblique complement): head-INITIAL.

message("Token-level head direction for 'La grande maison rouge ...':")
print(
  head_final_initial(dt_adj)[,
    .(token_id, head_final, head_initial, head_distance, integration_cost)
  ]
)
# head_final    = TRUE when the head comes AFTER the dependent (arc → right)
# head_distance = |position_head - position_dependent|  (arc span in tokens)
# integration_cost (Gibson DLT): sum of completed dependencies at this position


# 5) Crossing arcs and Gibson DLT ----
#
# A crossing occurs when arc A spans a range that partially overlaps arc B's
# range — they cannot be drawn without intersecting.  These are shown in RED.
#
# In Gibson's Dependency Locality Theory, a crossing means that while reading
# the sentence the parser is holding TWO unresolved dependencies in memory
# simultaneously — a greater working-memory load.
#
# max_incomplete_deps counts the worst such overlap in a sentence.
# avg_incomplete_deps averages it across all positions.
#
# Sentences with centre-embedding typically produce more crossings than
# right-branching sentences of the same length.

# Centre-embedded relative clause ("Le livre que ... a écrit est fascinant"):
# the relative clause interrupts the main clause, creating overlap.
dt_centre <- parse_sent(
  "Le livre que l'auteur a écrit est fascinant."
)

# Right-branching paraphrase of roughly the same content:
dt_right_branch <- parse_sent(
  "L'auteur a écrit un livre fascinant."
)

par(mfrow = c(1, 2), mar = c(4, 0.5, 3, 0.5))
plot_dependency_arcs(
  dt_centre,
  title = "Centre-embedding\n(red = crossing arcs)",
  show_deprel = FALSE,
  cex_text = 0.7
)
plot_dependency_arcs(
  dt_right_branch,
  title = "Right-branching paraphrase\n(no crossings expected)",
  show_deprel = FALSE,
  cex_text = 0.75
)
par(mfrow = c(1, 1))

message("--- centre-embedded relative ---")
s_centre      <- sentence_graph_stats(dt_centre,      verbose = TRUE)
message("--- right-branching paraphrase ---")
s_right_branch <- sentence_graph_stats(dt_right_branch, verbose = TRUE)

message("max_incomplete_deps: centre=", s_centre$max_incomplete_deps,
        "  right-branch=", s_right_branch$max_incomplete_deps)


# 6) Yngve depth: branching load ----
#
# Yngve (1960) proposed a different complexity measure based on how deeply a
# word is buried in the left-branching structure of the phrase.  Each word
# inherits its head's Yngve score and adds the number of siblings to its right.
# Words in long left-branching chains (e.g. stacked pre-nominal modifiers)
# accumulate high Yngve scores.
#
# mean_yngve / max_yngve are returned alongside depth by sentence_graph_stats().
# The arc plot does not colour arcs by Yngve score directly, but you can read
# it off: the more arcs fan out to the LEFT of a given node, the higher its
# right-sibling count, and thus the higher the Yngve load for that subtree.
#
# Compare a stacked modifier phrase (high Yngve) with a simple clause (low):

dt_stacked <- parse_sent(
  "Le très grand et très beau château médiéval est impressionnant."
)
dt_minimal  <- parse_sent(
  "Le château est impressionnant."
)

par(mfrow = c(1, 2), mar = c(4, 0.5, 3, 0.5))
plot_dependency_arcs(
  dt_stacked,
  title = "Stacked modifiers (high Yngve load)",
  show_deprel = FALSE,
  cex_text = 0.65
)
plot_dependency_arcs(
  dt_minimal,
  title = "Minimal clause (low Yngve load)",
  show_deprel = FALSE,
  cex_text = 0.75
)
par(mfrow = c(1, 1))

message("--- stacked modifiers ---")
sentence_graph_stats(dt_stacked,  verbose = TRUE)
message("--- minimal clause ---")
sentence_graph_stats(dt_minimal,  verbose = TRUE)


# 7) Side-by-side feature summary across all examples ----
#
# Collect all six example sentences into a small corpus so that batch_graph_stats()
# can summarise them in a single table.  This is a preview of the workflow used
# on full corpora in demo_dep_stats.R.

# Reparse as a named corpus so each sentence gets a unique doc_id
sentences <- c(
  simple      = "Le chat mange le poisson.",
  relative    = "Le chat mange la petite souris verte qui est entrée par le trou.",
  embedded    = "Le livre que l'auteur dont tout le monde parle a écrit est fascinant.",
  nominal     = "La grande maison rouge appartient à un vieux médecin.",
  centre      = "Le livre que l'auteur a écrit est fascinant.",
  right_branch = "L'auteur a écrit un livre fascinant."
)

dt_mini_corpus <- rbindlist(
  mapply(function(txt, nm) {
    dt <- parse_sent(txt)
    dt[, doc_id := nm]
    dt
  }, sentences, names(sentences), SIMPLIFY = FALSE),
  use.names = TRUE
)

dt_summary <- batch_graph_stats(dt_mini_corpus) |>
  as_tibble() |>
  select(doc_id, sentence_id,
         max_path, avg_dependency_depth, mean_yngve,
         avg_incomplete_deps, branching_factor) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

message("Feature summary across all example sentences:")
print(dt_summary)
