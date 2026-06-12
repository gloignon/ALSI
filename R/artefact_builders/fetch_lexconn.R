# Fetch and pre-process LEXCONN (French discourse connectives lexicon)
#
# Source: Roze, Danlos & Muller (2012)
# "LexConn: A French Lexicon of Discourse Connectives"
# Discours: Revue de Linguistique, Psycholinguistique et Informatique
#
# Output: lexical_dbs/dt_lexconn.Rds
#
# DISABLED: LexConn has no confirmed public license (see
# docs/licensing_and_resource_distribution.md §4.5). Fetching is paused
# until we hear back from the author or find an alternative resource.
stop("fetch_lexconn.R is disabled: LexConn has no confirmed public license. ",
     "See docs/licensing_and_resource_distribution.md §4.5.")

library(data.table)
library(xml2)

url <- "http://www.linguist.univ-paris-diderot.fr/~croze/D/Lexconn.xml"
local_xml <- tempfile(fileext = ".xml")

message("Downloading LEXCONN from ", url)
download.file(url, local_xml, mode = "wb", quiet = TRUE)

doc <- read_xml(local_xml)
connectors <- xml_find_all(doc, "//connecteur")

message("Parsing ", length(connectors), " connective entries...")

rows <- rbindlist(lapply(connectors, function(node) {
  id        <- xml_attr(node, "id")
  cat       <- xml_attr(node, "cat")       # prep, csu, cco, adv
  relations <- xml_attr(node, "relations")  # comma-separated, may have * suffix
  type      <- xml_attr(node, "type")       # coord or sub

  # Extract all <forme> children (a connective can have multiple surface forms)
  formes <- xml_text(xml_find_all(node, "forme"))
  formes <- trimws(formes)

  data.table(
    id       = id,
    forme    = formes,
    cat      = cat,
    type     = type,
    relations = relations
  )
}))

# Clean up relations: remove * suffix (marks non-prototypical usage)
rows[, relations_clean := gsub("\\*", "", relations)]

# Split comma-separated relations into one row per relation
dt_lexconn <- rows[, .(relation = unlist(strsplit(relations_clean, ","))),
                   by = .(id, forme, cat, type, relations)]

# Lowercase the forme for matching against parsed tokens
dt_lexconn[, forme_lower := tolower(forme)]

# Count tokens per connective (for multi-word matching)
dt_lexconn[, n_tokens := lengths(strsplit(forme, "\\s+"))]

# Relation categories (for grouped features)
causal    <- c("result", "consequence", "explanation", "goal", "evidence")
temporal  <- c("narration", "flashback", "background", "background-inverse", "temploc")
adversative <- c("concession", "contrast", "violation")
additive  <- c("continuation", "parallel", "alternation", "elaboration")
other     <- c("summary", "detachment", "digression", "rephrasing",
               "condition", "unknown")

dt_lexconn[, relation_group := fcase(
  relation %in% causal,      "causal",
  relation %in% temporal,    "temporal",
  relation %in% adversative, "adversative",
  relation %in% additive,    "additive",
  default = "other"
)]

# Summary
message("\nLEXCONN summary:")
message("  Unique connective forms: ", uniqueN(dt_lexconn$forme_lower))
message("  Unique connective IDs:   ", uniqueN(dt_lexconn$id))
message("  Rows (form × relation):  ", nrow(dt_lexconn))
message("\n  By syntactic category:")
print(dt_lexconn[, .(n_forms = uniqueN(forme_lower)), by = cat][order(-n_forms)])
message("\n  By relation group:")
print(dt_lexconn[, .(n_forms = uniqueN(forme_lower)), by = relation_group][order(-n_forms)])
message("\n  Multi-word connectives: ", dt_lexconn[n_tokens > 1, uniqueN(forme_lower)])

out_path <- "lexical_dbs/dt_lexconn.Rds"
saveRDS(dt_lexconn, out_path)
message("\nSaved to ", out_path)

unlink(local_xml)
