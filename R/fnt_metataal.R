#' Count MetaTaal LEGO Bricks Needed for a Corpus
#'
#' Tallies, per document, how many of each MetaTaal grammar brick a UD-parsed
#' text would require, for physical brick-ordering purposes (not spatial
#' layout). One brick per counted token; tokens with no corresponding brick
#' (coordinating conjunctions, punctuation, numerals, non-relative pronouns,
#' etc.) are not tallied under any brick.
#'
#' Note: an adverb brick (\code{n_adv}) is an ALSI extension beyond the source
#' paper, which has no adverb brick; see \code{vignettes/specs/metataal-lego.Rmd}.
#'
#' @param dt A parsed data.table (parser output after
#'   \code{post_process_lexicon}) containing at minimum: \code{doc_id},
#'   \code{token_id}, \code{head_token_id}, \code{upos}, \code{dep_rel}, and
#'   \code{feats}.
#'
#' @returns A tibble with one row per \code{doc_id} and columns:
#'   \describe{
#'     \item{n_det}{Green 1x2 determiner bricks (\code{upos == "DET"}).}
#'     \item{n_noun}{Blue 2x4 noun bricks (\code{upos \%in\% c("NOUN", "PROPN")}).}
#'     \item{n_verb}{Red 2x3 verb bricks (\code{upos \%in\% c("VERB", "AUX")}).}
#'     \item{n_adj}{Brown 2x2 adjective bricks (\code{upos == "ADJ"}).}
#'     \item{n_adp}{Yellow 2x2 preposition bricks (\code{upos == "ADP"}).}
#'     \item{n_adv}{Pink 2x2 adverb bricks (\code{upos == "ADV"}). ALSI
#'       extension beyond the source paper (see Details).}
#'     \item{n_bridge}{Grey bridge bricks. NOT a per-token count: one grey
#'       brick per row transition between the main clause and an embedded
#'       finite clause (down on entry, up when the main clause resumes). See
#'       Details.}
#'     \item{n_total}{Sum of the seven brick counts above.}
#'   }
#'
#' @details
#' Full specification, brick colors/sizes, clause-layout rules, and
#' provenance notes: \code{vignettes/specs/metataal-lego.Rmd}.
#'
#' The six word-class bricks (\code{n_det}, \code{n_noun}, \code{n_verb},
#' \code{n_adj}, \code{n_adp}, \code{n_adv}) are a plain per-token tally by
#' \code{upos}.
#'
#' \code{n_bridge} is different: a grey bridge is one physical LEGO brick per
#' row transition between the main-clause row and an embedded finite-clause
#' row, so a \strong{center-embedded} relative clause (the main clause resumes
#' after it) costs \strong{two} grey bricks -- one down bridge on entry and one
#' up (comma) bridge on resume -- while a \strong{right-branching} clause that
#' ends the sentence costs \strong{one} (down only, nothing resumes). This
#' reuses the same finite-embedded-clause row assignment
#' \code{draw_metataal_bricks()} uses (\code{split_into_clauses(...,
#' require_finite = TRUE)} over \code{ccomp}/\code{advcl}/\code{acl}/
#' \code{acl:relcl}/\code{csubj}/\code{csubj:pass}/\code{xcomp}), via the shared
#' helper \code{.metataal_bridge_counts()}, so the counted and drawn bridges
#' never drift. A relative marker (\code{PronType=Rel}, or a \code{mark}
#' dependent of a \code{ccomp}/\code{advcl}/\code{acl}/\code{acl:relcl} head)
#' still gets its own grey word-class brick in the \emph{drawing}, but the
#' \code{n_bridge} \emph{count} is transition-based, not marker-based.
#'
#' UD multiword-token range rows (\code{token_id} like \code{"5-6"}) and
#' empty nodes (\code{token_id} like \code{"8.1"}) carry no \code{upos} of
#' their own in ALSI's parsed output and are excluded, consistent with
#' \code{post_process_lexicon()} in \code{fnt_corpus.R} and
#' \code{split_into_clauses()} in \code{fnt_clause_split.R}.
#'
#' @references
#'   Zwitserlood, R., Wijnen, F., van Weerdenburg, M., & Verhoeven, L.
#'   (2015). "MetaTaal": Enhancing complex syntax in children with specific
#'   language impairment -- a metalinguistic and multimodal approach.
#'   \emph{International Journal of Language & Communication Disorders},
#'   50(3), 273--297. \doi{10.1111/1460-6984.12131}
#'
#' @citation_type "adapted" (brick/word-class mapping is an ALSI
#'   operationalization of the paper's crib sheet; see
#'   vignettes/specs/metataal-lego.Rmd for the full mapping and flagged
#'   gaps)
#'
#' @export
count_metataal_bricks <- function(dt) {
  dt_corpus <- assign_metataal_bricks(dt)

  # Word-class brick counts (green/blue/red/brown/yellow/pink) are a plain
  # per-token tally. Grey bridges are NOT: n_bridge is a physical-brick count
  # over row transitions (one grey brick per down/up move between the main
  # clause and an embedded finite clause), shared with the renderer so the
  # drawn and counted bridges never drift. See .metataal_bridge_counts() and
  # vignettes/specs/metataal-lego.Rmd (bridge-counting rule).
  word_levels <- c("n_det", "n_noun", "n_verb", "n_adj", "n_adp", "n_adv")

  counts <- dt_corpus[!is.na(brick) & brick != "n_bridge",
                      .N, by = .(doc_id, brick)] |>
    dcast(doc_id ~ brick, value.var = "N", fill = 0L)

  # dcast on a doc with only bridge tokens (or none) yields no word-class
  # columns; ensure the full set exists, and that every doc in dt has a row.
  all_docs <- unique(dt_corpus$doc_id)
  missing_docs <- setdiff(all_docs, counts$doc_id)
  if (length(missing_docs) > 0) {
    counts <- rbind(counts,
                    data.table(doc_id = missing_docs), fill = TRUE)
  }
  missing_cols <- setdiff(word_levels, names(counts))
  for (col in missing_cols) counts[, (col) := 0L]
  for (col in word_levels) counts[is.na(get(col)), (col) := 0L]

  bridge_counts <- .metataal_bridge_counts(dt)
  counts <- merge(counts, bridge_counts, by = "doc_id", all.x = TRUE)
  counts[is.na(n_bridge), n_bridge := 0L]

  brick_levels <- c(word_levels, "n_bridge")
  setcolorder(counts, c("doc_id", brick_levels))
  counts[, n_total := n_det + n_noun + n_verb + n_adj + n_adp + n_adv + n_bridge]

  return(as_tibble(counts))
}

# Internal helper: physical grey-bridge count per doc_id.
#
# A grey bridge is one LEGO brick per row transition between the main-clause
# row (0) and an embedded finite-clause row (-1) -- a down bridge where an
# embedded clause is entered and an up bridge where the main clause resumes.
# A trailing (right-branching) embedded clause that ends the sentence has no
# up bridge (nothing resumes), so it counts one, not two. This mirrors the
# renderer's own row assignment (layout_metataal_bricks()) exactly -- same
# split_into_clauses() call, same row definition -- but counts only the real
# down/up transitions, dropping the closing arch the renderer appends purely
# to draw a trailing clause. Keeping this here (rather than reusing the full
# layout, which also does x-cursor/geometry) lets the count run corpus-wide
# in one split_into_clauses() pass. See vignettes/specs/metataal-lego.Rmd.
#
# @returns data.table(doc_id, n_bridge).
.metataal_bridge_counts <- function(dt) {
  dt_corpus <- setDT(copy(dt))

  # Same wider finite dependent-clause set + finiteness gate the renderer
  # uses, so a bridge is counted exactly where a row is dropped in the
  # drawing (fnt_clause_split.R must be sourced first).
  draw_dc_rels <- c("ccomp", "advcl", "acl", "acl:relcl",
                    "csubj", "csubj:pass", "xcomp")
  # .assign_clause_ids() indexes by character token ids; integer ids (Trankit/
  # spaCy) otherwise become positional lookups. Coerce as the renderer does.
  dt_corpus[, token_id      := as.character(token_id)]
  dt_corpus[, head_token_id := as.character(head_token_id)]
  dt_corpus <- split_into_clauses(dt_corpus, dc_rels = draw_dc_rels,
                                  require_finite = TRUE)

  is_syntactic <- !grepl("[-.]", dt_corpus$token_id)
  syn <- dt_corpus[is_syntactic]

  bridges_for_doc <- function(clause_id, dep_rel, token_id) {
    # Root's clause is the main-clause row; split_into_clauses() does not
    # guarantee clause_id 1 is the root's, so look it up (as the renderer).
    root_cid <- clause_id[dep_rel == "root"][1]
    # No identifiable root (malformed parse): no reliable main-clause row to
    # measure transitions against, so count no bridges rather than treat every
    # clause as embedded.
    if (is.na(root_cid)) return(0L)
    row <- ifelse(is.na(clause_id) | clause_id == root_cid, 0L, -1L)
    ord <- token_id_num_key(token_id)
    row <- row[order(ord)]
    if (length(row) < 2L) return(0L)
    # One grey brick per genuine down/up transition. The renderer appends a
    # closing transition when the sequence ends on row -1 (trailing clause);
    # that closing arch is a drawing flourish, not a physical return brick,
    # so it is NOT counted here.
    return(sum(diff(row) != 0L))
  }

  out <- syn[, .(n_bridge = bridges_for_doc(clause_id, dep_rel, token_id)),
             by = doc_id]
  return(out)
}

#' Assign a MetaTaal Brick to Each Token (Internal Helper)
#'
#' Shared word-class -> brick mapping used by both \code{count_metataal_bricks()}
#' (aggregate tally) and \code{draw_metataal_bricks()} (sequence-preserving
#' plot). Kept as one function so the two never drift apart.
#'
#' @param dt A parsed data.table, see \code{count_metataal_bricks()}.
#' @param include_filler If \code{FALSE} (default), syntactic tokens with no
#'   MetaTaal brick keep \code{brick = NA}, matching the brick tally
#'   (non-relative pronouns, conjunctions, punctuation, etc. order no
#'   brick). If \code{TRUE}, those tokens instead get the neutral filler class
#'   \code{"n_none"} so the drawing can show every entered word in sequence
#'   (\code{draw_metataal_bricks()} uses this); the aggregate tally in
#'   \code{count_metataal_bricks()} never sets it, so the ordered brick counts
#'   are unaffected. MWT range rows and empty nodes stay \code{NA} either way.
#'
#' @returns A copy of \code{dt} as a data.table, row order preserved, with an
#'   added \code{brick} column (one of \code{"n_det"}, \code{"n_noun"},
#'   \code{"n_verb"}, \code{"n_adj"}, \code{"n_adp"}, \code{"n_adv"},
#'   \code{"n_bridge"};
#'   \code{"n_none"} for a bricked-nothing syntactic token when
#'   \code{include_filler = TRUE}; \code{NA} otherwise).
#'
#' @keywords internal
assign_metataal_bricks <- function(dt, include_filler = FALSE) {
  dt_corpus <- setDT(copy(dt))

  is_syntactic <- !grepl("[-.]", dt_corpus$token_id)

  dt_corpus[, is_relative_marker := FALSE]
  dt_corpus[is_syntactic, is_relative_marker :=
              grepl("PronType=Rel", feats) |
              (dep_rel == "mark" &
                 head_token_id %in% token_id[dep_rel %in%
                   c("ccomp", "advcl", "acl", "acl:relcl")]),
            by = doc_id]

  dt_corpus[, brick := NA_character_]
  dt_corpus[is_syntactic & is_relative_marker, brick := "n_bridge"]
  dt_corpus[is_syntactic & is.na(brick) & upos == "DET",              brick := "n_det"]
  dt_corpus[is_syntactic & is.na(brick) & upos %in% c("NOUN", "PROPN"), brick := "n_noun"]
  dt_corpus[is_syntactic & is.na(brick) & upos %in% c("VERB", "AUX"),   brick := "n_verb"]
  dt_corpus[is_syntactic & is.na(brick) & upos == "ADJ",               brick := "n_adj"]
  dt_corpus[is_syntactic & is.na(brick) & upos == "ADP",               brick := "n_adp"]
  dt_corpus[is_syntactic & is.na(brick) & upos == "ADV",               brick := "n_adv"]

  if (include_filler) {
    dt_corpus[is_syntactic & is.na(brick), brick := "n_none"]
  }

  dt_corpus[, is_relative_marker := NULL]

  return(dt_corpus)
}

#' Draw MetaTaal Bricks for a Sentence
#'
#' Renders the MetaTaal brick sequence for one document as fixed-width
#' colored rectangles (a plain visual stand-in for the physical LEGO
#' bricks, deliberately not a LEGO-styled rendering), laid out left to
#' right in token order and dropped down one row for embedded (dependent)
#' clauses, per the paper's clause-level layout rule: a relative/complement clause is built one level lower than
#' the main clause, connected by vertical "bridge" bricks (see
#' \code{vignettes/specs/metataal-lego.Rmd}, "Clause-level layout rules").
#' Every word of the sentence is drawn so it reads intact: tokens that order
#' no brick (punctuation, non-relative pronouns, plain
#' conjunctions, etc.) are shown as neutral off-white "no brick" filler tiles
#' in their sentence position. This is a drawing choice only --
#' \code{count_metataal_bricks()} still orders no brick for those tokens, so
#' the brick tally is unchanged (see Details).
#'
#' @param dt A parsed data.table for a single document (see
#'   \code{count_metataal_bricks()}), additionally requiring
#'   \code{paragraph_id} and \code{sentence_id} (needed by
#'   \code{split_into_clauses()} to detect embedded clauses; see Details). If
#'   \code{dt} contains more than one \code{doc_id}, set \code{doc_id} to
#'   select which one to draw.
#' @param doc_id Optional; the \code{doc_id} to draw when \code{dt} contains
#'   more than one document. Defaults to the first \code{doc_id} in \code{dt}.
#' @param render_as One of \code{"ascii"} (default) or \code{"image"}.
#'   \code{"ascii"} prints a text-mode rendering to the console and returns it
#'   invisibly as a single string (via \code{message()}, so it goes to
#'   stderr, matching the rest of ALSI's console output -- see
#'   \code{demos/} conventions). \code{"image"} returns a ggplot object (not
#'   printed/saved by this function) drawing the bricks as fixed-width
#'   colored rectangles. \code{"ascii"} stays the default so the function is
#'   console-friendly without a graphics device.
#' @param label_tokens If \code{TRUE} (default), the surface token is printed
#'   inside each brick (both render modes). In \code{render_as = "image"}
#'   the brick's word class is already shown via its fill color; in
#'   \code{render_as = "ascii"} (no color available) each box also carries a
#'   one-letter word-class code, e.g. \code{"[dog:N]"} for a noun, matching
#'   the paper's color legend (D=Determiner/green, N=Noun/blue, V=Verb/red,
#'   A=Adjective/brown, P=Preposition/yellow, B=bridge/grey) -- a legend line
#'   decoding these is appended to the ascii output.
#' @param bridge_mode One of \code{"brick_bar"} (default) or \code{"bar"},
#'   controlling how a clause-introducing relative pronoun / complementizer is
#'   drawn at the point where an embedded clause drops one row.
#'   \code{"brick_bar"} keeps the prior behaviour: the relative marker gets its
#'   own grey word-class brick (on the embedded row) \emph{and} the vertical
#'   down-bar connecting the two rows is drawn separately (brick + bar).
#'   \code{"bar"} instead makes the relative marker \emph{be} the down-bar:
#'   the marker's own brick is not drawn, and its surface token labels the
#'   vertical down-bar at the clause-entry point (bar only). Only a relative
#'   marker that is the first bricked token of an embedded clause (i.e. sits
#'   at a downward row transition) is affected; any other grey bridge brick
#'   stays a brick in both modes, and the tally from
#'   \code{count_metataal_bricks()} is unchanged either way.
#' @param show_legend If \code{TRUE} (default), the one-line class-code key
#'   (D/N/V/A/P/B/-) is appended below the ascii layout. Set to \code{FALSE}
#'   to omit it, e.g. when drawing many sentences in a loop where the key only
#'   needs to be shown once. Ignored for \code{render_as = "image"}, whose
#'   fill colors carry their own ggplot legend.
#'
#' @returns With \code{render_as = "image"}: a ggplot object with word-class
#'   bricks on a main-clause row (row 0) and, when the sentence has an
#'   embedded relative/complement clause, a second row (row -1) one level
#'   lower holding that clause's bricks, joined to row 0 by vertical bridge
#'   bricks at the points where the embedding starts and ends. With
#'   \code{render_as = "ascii"}: the same layout as a text string, invisibly
#'   (also printed to the console via \code{message()}).
#'
#' @details
#' All bricks are drawn at one fixed width and height, sized to hold a token
#' label at readable size -- an ALSI rendering choice (per 2026-07-05
#' request from Guillaume), so the crib sheet's per-class stud sizes are
#' deliberately not used for drawing (this includes the determiner brick,
#' drawn the same height as the rest rather than the crib sheet's
#' half-height). Fill colors are still the crib sheet's brick colors.
#'
#' Un-bricked tokens are drawn as neutral \code{n_none} filler tiles (see the
#' description) so the whole sentence appears in sequence; they never start,
#' move, or close a bridge (only bricked clause boundaries do), and are absent
#' from \code{count_metataal_bricks()}.
#'
#' Embedded-clause detection reuses \code{split_into_clauses()} (see
#' \code{fnt_clause_split.R}, which must be sourced before this function is
#' called): every token whose \code{clause_id} differs from the root's clause
#' is drawn one row down (row -1). For drawing, this function passes
#' \code{split_into_clauses()} a wider dependent-clause set than its default
#' -- \code{ccomp}, \code{advcl}, \code{acl}, \code{acl:relcl} plus
#' \code{csubj}, \code{csubj:pass}, and \code{xcomp} -- so a clausal subject or
#' open clausal complement can also drop one row (e.g. the "que tu la savoures"
#' clause of a cleft), but with \code{require_finite = TRUE} so only a
#' \emph{finite} subordinate clause drops. Ordinary noun/prepositional/
#' adjective phrases and non-finite verb complements stay on their finite
#' verb's row: a bare adnominal participle (\code{acl}, "la pomme cueillie")
#' and an infinitival open complement (\code{xcomp}, "va manger") are not
#' finite clauses and are kept on row 0. The question the layout answers is
#' "does this constituent introduce a new finite clause?", not "is there a
#' dependency here?" -- so e.g. "Il mange sa pomme verte dans le jardin." draws
#' entirely on row 0. The default set is deliberately Lu (2010)'s finite
#' dependent-clause definition, kept narrow so clause/T-unit \emph{counts}
#' stay stable (see the note in \code{fnt_syntactic_complexity.R}); the wider
#' set plus the finiteness gate here affect only the drawing and no count
#' feature.
#' The paper's layout rules distinguish
#' right-branching (clause trails at the end) from center-embedded (clause
#' interrupts mid-sentence) relative clauses, but per 2026-07-05 clarification
#' from Guillaume both cases get a down bridge where the embedding starts and
#' an up bridge where it ends -- for a trailing (right-branching) clause the
#' up bridge is simply the last element drawn, with no more row-0 bricks
#' after it, since the main clause has already finished. The source paper
#' does not address nesting beyond one embedded level (see spec): every
#' embedded clause is drawn on the single row -1, whether it is one of several
#' sibling clauses off the main clause or a clause nested inside another
#' embedded clause -- an ALSI simplification, not a paper-specified rule.
#'
#' The vertical bridge itself has no stud dimensions given in the source
#' crib sheet (only described as an arched brick); it is drawn here as a
#' 1-stud-wide vertical segment, distinct from the ordinary word bricks,
#' that always spans the exact same fixed height between row 0's bottom
#' edge and row -1's top edge -- an ALSI rendering choice, not a sourced
#' size. A down-bridge and an up-bridge are therefore never different
#' heights or offsets; the only difference is which side of the transition
#' they sit on.
#'
#' @references
#'   Zwitserlood, R., Wijnen, F., van Weerdenburg, M., & Verhoeven, L.
#'   (2015). "MetaTaal": Enhancing complex syntax in children with specific
#'   language impairment -- a metalinguistic and multimodal approach.
#'   \emph{International Journal of Language & Communication Disorders},
#'   50(3), 273--297. \doi{10.1111/1460-6984.12131}
#'
#' @citation_type "adapted" (brick/word-class mapping as in
#'   \code{count_metataal_bricks()}; the row-lowering/bridge layout follows
#'   the paper's clause-layout rules, per vignettes/specs/metataal-lego.Rmd;
#'   the rectangle rendering itself is an ALSI visualization convenience, not
#'   part of the source paper's method)
#'
#' @export
draw_metataal_bricks <- function(dt, doc_id = NULL,
                                 render_as = c("ascii", "image"),
                                 label_tokens = TRUE,
                                 bridge_mode = c("brick_bar", "bar"),
                                 show_legend = TRUE) {
  render_as <- match.arg(render_as)
  bridge_mode <- match.arg(bridge_mode)
  layout <- layout_metataal_bricks(dt, doc_id = doc_id, bridge_mode = bridge_mode)

  if (render_as == "image") {
    return(render_metataal_image(layout, label_tokens = label_tokens))
  }
  return(render_metataal_ascii(layout, label_tokens = label_tokens,
                               show_legend = show_legend))
}

# Internal helper: shared row/x-position/bridge layout computation used by
# both the ascii and image renderers, so the two never drift apart on the
# actual clause-layout logic (only how a brick is drawn differs between
# them).
#
# @param bridge_mode "brick_bar" (default; relative marker drawn as its own
#   grey brick AND a separate down-bar) or "bar" (the clause-introducing
#   relative marker is NOT drawn as a brick; its token instead labels the
#   down-bar). See draw_metataal_bricks() for the full description.
# @returns A list with `bricks` (one row per bricked token: brick, label,
#   token, row, width, height, x_start, x_end) and `bridges` (one row per
#   row-transition connector: x_start, x_end, going_down, y_base, y_top,
#   token -- the last being the relative-marker label carried onto a down-bar
#   in bridge_mode = "bar", NA otherwise; NULL if the sentence has no embedded
#   clause), both data.tables.
layout_metataal_bricks <- function(dt, doc_id = NULL,
                                   bridge_mode = c("brick_bar", "bar")) {
  bridge_mode <- match.arg(bridge_mode)
  # include_filler = TRUE so every syntactic token is drawn in sequence (a
  # neutral n_none tile), even those the paper orders no brick for -- the
  # drawing shows the whole sentence, while count_metataal_bricks() (which
  # calls assign_metataal_bricks() without this flag) is unchanged.
  dt_corpus <- assign_metataal_bricks(dt, include_filler = TRUE)

  # split_into_clauses()'s internal clause-id lookup (.assign_clause_ids() in
  # fnt_clause_split.R) indexes a named vector with `tok` directly; when
  # token_id/head_token_id are integer (Trankit and spaCy output, unlike
  # UDPipe's character ids with "5-6"-style MWT ranges) that becomes a
  # positional `[[` lookup instead of a name lookup and errors out. Coercing
  # to character here works around it without touching fnt_clause_split.R
  # (a separate, already-tested feature, out of scope for this fix).
  dt_corpus[, token_id      := as.character(token_id)]
  dt_corpus[, head_token_id := as.character(head_token_id)]

  # Broader dependent-clause set than split_into_clauses()'s default. The
  # default (ccomp/advcl/acl/acl:relcl) is deliberately Lu (2010)'s FINITE
  # dependent-clause definition, so clause/T-unit COUNTS stay stable (see the
  # @note in fnt_syntactic_complexity.R on why csubj/xcomp were removed).
  # For the DRAWING we want every subordinate clause to drop to its own row,
  # including clausal subjects (csubj/csubj:pass) and open clausal complements
  # (xcomp) -- e.g. "que tu la savoures" in a cleft. Passing the wider set
  # here only affects this layout; no count feature is touched.
  draw_dc_rels <- c("ccomp", "advcl", "acl", "acl:relcl",
                    "csubj", "csubj:pass", "xcomp")
  # require_finite = TRUE: a constituent drops to the lower row only if it
  # forms a FINITE subordinate clause. Ordinary noun/prepositional/adjective
  # phrases and non-finite verb complements (a bare acl participle
  # "la pomme cueillie", an xcomp infinitive "va manger") stay on the finite
  # verb's row. Without this, the UD-relation set above would drop any acl/
  # xcomp — including non-finite modifiers — even though they open no clause.
  dt_corpus <- split_into_clauses(dt_corpus, dc_rels = draw_dc_rels,
                                  require_finite = TRUE)

  target_doc <- if (is.null(doc_id)) dt_corpus$doc_id[1] else doc_id
  sent_dt <- dt_corpus[dt_corpus$doc_id == target_doc, ]

  if (nrow(sent_dt) == 0) {
    stop("No tokens found for doc_id '", target_doc, "'.")
  }

  # split_into_clauses() assigns clause_id in the row order clause heads
  # appear in dt, NOT guaranteed to give the root's clause id 1 (that only
  # holds when the root happens to be the first clause head encountered) --
  # so the root's own clause_id is looked up directly rather than assumed.
  # Any other clause_id is an embedded (dependent) clause, drawn one row
  # down. NA clause_id (MWT range rows, empty nodes) never carries a brick,
  # so it is dropped along with other un-bricked tokens below.
  root_clause_id <- sent_dt$clause_id[sent_dt$dep_rel == "root"][1]
  sent_dt[, row := ifelse(is.na(clause_id) | clause_id == root_clause_id, 0L, -1L)]

  bricks <- sent_dt[!is.na(sent_dt$brick), ]
  if (nrow(bricks) == 0) {
    stop("No bricked tokens found for doc_id '", target_doc, "'.")
  }
  bricks[, token_order := token_id_num_key(token_id)]
  setorder(bricks, token_order)

  # All bricks share one fixed drawing width and height -- wide/tall enough
  # for a token label -- per 2026-07-05 request from Guillaume: the image
  # need not look like LEGO, so the crib sheet's per-class stud sizes are
  # not used for drawing. Colors are still the crib sheet's brick colors
  # (vignettes/specs/metataal-lego.Rmd).
  # n_none is the neutral filler tile for a syntactic token that orders
  # no brick (non-relative pronoun, conjunction, punctuation, ...);
  # drawn in a plain off-white so it reads as "word, but not a MetaTaal brick"
  # and never mistaken for one of the six coloured classes.
  brick_legend <- data.table(
    brick  = c("n_det", "n_noun", "n_verb", "n_adj", "n_adp", "n_adv",
               "n_bridge", "n_none"),
    label  = c("Determiner", "Noun", "Verb", "Adjective", "Preposition",
               "Adverb", "Complementizer / relative marker",
               "Other word (no brick)"),
    color  = c("#4CAF50", "#2196F3", "#E53935", "#8D6E63", "#FDD835", "#EC407A",
               "#9E9E9E", "#F5F5F5"),
    width  = 3,
    height = 2
  )

  bricks <- merge(bricks, brick_legend, by = "brick", sort = FALSE)
  setorder(bricks, token_order)

  # bridge_mode = "bar": a relative marker that introduces an embedded clause
  # (an n_bridge brick that is the FIRST bricked token of an embedded run,
  # i.e. sits immediately after a downward row transition) is not drawn as its
  # own brick -- its surface token instead labels the down-bar connecting the
  # two rows. Detect those entry bricks on the FULL sequence first, record
  # their tokens in top-to-bottom (transition) order, then drop them so the
  # transition/cursor computation below runs on the reduced sequence. Every
  # other grey bridge brick (e.g. a complementizer not at a row drop) stays a
  # brick, matching count_metataal_bricks() (which is unchanged in both modes).
  bar_labels <- character(0)
  if (bridge_mode == "bar") {
    enters_down <- c(FALSE, diff(bricks$row) < 0)
    is_entry_marker <- bricks$brick == "n_bridge" & bricks$row == -1L & enters_down
    if (any(is_entry_marker)) {
      bar_labels <- bricks$token[is_entry_marker]
      bricks <- bricks[!is_entry_marker, ]
    }
  }

  # x-position: each row keeps its own running cursor, so row 0 pauses while
  # an embedded clause is being drawn on row -1 and resumes at the same x
  # where it left off. A bridge connector is inserted into the sequence at
  # every row transition, and it advances the CURRENT row's cursor by its
  # own width so later bricks on that row clear it -- this is what keeps the
  # word-class bridge brick (e.g. "that") from being drawn on top of the
  # vertical connector.
  row_seq <- bricks$row
  transitions <- which(diff(row_seq) != 0)
  # A trailing embedded clause (nothing follows on row 0) still gets a
  # closing up-bridge per 2026-07-05 clarification from Guillaume, so a
  # final transition back to row 0 is appended when the sequence ends on
  # row -1.
  if (row_seq[length(row_seq)] != 0L) {
    transitions <- c(transitions, length(row_seq))
  }

  cursor <- c(`0` = 0, `-1` = 0)
  x_start <- numeric(nrow(bricks))
  bridge_rows <- list()
  next_transition <- 1L
  for (i in seq_len(nrow(bricks))) {
    r <- as.character(bricks$row[i])
    x_start[i] <- cursor[[r]]
    cursor[[r]] <- cursor[[r]] + bricks$width[i]

    if (next_transition <= length(transitions) && transitions[next_transition] == i) {
      # The appended final transition (sequence ends on row -1) is always
      # the closing up-bridge back to the main-clause row.
      going_down <- if (i < nrow(bricks)) row_seq[i + 1] < row_seq[i] else FALSE
      bridge_x <- max(cursor[["0"]], cursor[["-1"]])
      cursor[["0"]]  <- bridge_x + 1
      cursor[["-1"]] <- bridge_x + 1
      bridge_rows[[length(bridge_rows) + 1]] <- data.table(
        x_start = bridge_x, going_down = going_down
      )
      next_transition <- next_transition + 1L
    }
  }
  # row_gap is the vertical space between the main-clause row's bottom (y=0)
  # and the embedded-clause row's top -- fixed so every bridge connector
  # spans exactly this distance regardless of direction (see below).
  row_gap <- 3
  bricks[, x_start := x_start]
  bricks[, x_end := x_start + width]
  bricks[, y_base := ifelse(row == 0L, 0, -(brick_legend$height[1] + row_gap))]
  bricks[, label := factor(label, levels = brick_legend$label)]

  # Each bridge connector is 1 stud wide and spans the full row_gap, flush
  # against row 0's bottom (y=0) and row -1's top (y=-row_gap) on both ends
  # -- a down-bridge (entering the embedded clause) and an up-bridge
  # (returning to the main clause, or closing a trailing embedded clause)
  # are therefore identical in height, differing only in which token they
  # sit next to, per 2026-07-05 clarification -- see @details.
  bridges <- NULL
  if (length(bridge_rows) > 0) {
    bridges <- rbindlist(bridge_rows)
    bridges[, x_end := x_start + 1]
    bridges[, y_base := -row_gap]
    bridges[, y_top  := 0]
    # In bridge_mode = "bar", each extracted relative-marker token labels the
    # down-bar it introduced. Down-bars appear in the same top-to-bottom order
    # the markers were collected, so they zip together directly; up-bars and
    # (in brick_bar mode) all bars carry no token label.
    bridges[, token := NA_character_]
    if (length(bar_labels) > 0) {
      bridges[going_down == TRUE, token := bar_labels[seq_len(.N)]]
    }
  }

  return(list(bricks = bricks, bridges = bridges, brick_legend = brick_legend))
}

# Internal helper: renders a layout_metataal_bricks() result as a ggplot
# object -- plain colored rectangles, deliberately not LEGO-styled (no
# studs or arch shapes; see the brick_legend comment in
# layout_metataal_bricks() for the fixed-width rendering choice).
render_metataal_image <- function(layout, label_tokens = TRUE) {
  bricks <- copy(layout$bricks)
  bridges <- layout$bridges
  brick_legend <- layout$brick_legend

  # show.legend = TRUE forces a filled swatch for every legend key: with
  # drop = FALSE, brick classes absent from this sentence would otherwise
  # get an empty (invisible) key.
  plt <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bricks,
      ggplot2::aes(xmin = x_start, xmax = x_end,
                   ymin = y_base, ymax = y_base + height, fill = label),
      color = "black", linewidth = 0.4, show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
      name = "Brick",
      values = stats::setNames(brick_legend$color, brick_legend$label),
      drop = FALSE
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(bridges)) {
    plt <- plt +
      ggplot2::geom_rect(
        data = bridges,
        ggplot2::aes(xmin = x_start, xmax = x_end, ymin = y_base, ymax = y_top),
        fill = "#9E9E9E", color = "black", linewidth = 0.4
      )
    # bridge_mode = "bar": a down-bar carrying a relative-marker token label
    # (see layout_metataal_bricks()) prints that token inside the bar, rotated
    # 90 degrees so it fits the 1-stud-wide vertical connector.
    labelled_bars <- bridges[!is.na(bridges$token), ]
    if (label_tokens && nrow(labelled_bars) > 0) {
      plt <- plt +
        ggplot2::geom_text(
          data = labelled_bars,
          ggplot2::aes(x = (x_start + x_end) / 2, y = (y_base + y_top) / 2,
                       label = token),
          angle = 90, color = "black", size = 3
        )
    }
  }

  if (label_tokens) {
    # Long tokens shrink so they stay inside the fixed brick width.
    bricks[, label_size := pmin(3.2, 6.8 * width / nchar(token))]
    plt <- plt +
      ggplot2::geom_text(
        data = bricks,
        ggplot2::aes(x = (x_start + x_end) / 2, y = y_base + height / 2,
                     label = token, size = label_size),
        color = "black"
      ) +
      ggplot2::scale_size_identity()
  }

  return(plt)
}

# One-letter word-class code per brick, standing in for the color legend in
# ascii mode (Determiner=green, Noun=blue, Verb=red, Adjective=brown,
# Preposition=yellow, Complementizer/relative-marker bridge=grey -- see the
# brick_legend colors in layout_metataal_bricks()).
metataal_class_code <- c(n_det = "D", n_noun = "N", n_verb = "V",
                          n_adj = "A", n_adp = "P", n_adv = "R",
                          n_bridge = "B", n_none = "-")

# Internal helper: text for exactly one ASCII brick box, e.g. "[dog:N]" (a
# noun brick) or, with label_tokens = FALSE, "[N]". Vectorised over
# token/brick so it can be applied to a whole bricks data.table column at
# once. Kept as its own function so box styling (the "[...]" wrapper and the
# class-code suffix) has one place to change.
metataal_ascii_box <- function(token, brick, label_tokens = TRUE) {
  code <- unname(metataal_class_code[brick])
  inner <- if (label_tokens) paste0(token, ":", code) else code
  return(paste0("[", inner, "]"))
}

# Internal helper: renders a layout_metataal_bricks() result as a text-mode
# grid. Character-space positions are recomputed independently of the
# image renderer's stud-unit positions (studs are far too narrow to hold a
# word), but reuse the same token order / row / bridge-transition sequence
# from layout_metataal_bricks(), so the two renderers can never disagree on
# what is on which row or where a bridge goes -- only pixel/character sizing
# differs. Each brick is drawn as a `[token]` box on its row; each row
# transition is a vertical bar spanning the 3 blank text-rows between row 0
# and row -1, positioned at the x where that particular bridge sits.
render_metataal_ascii <- function(layout, label_tokens = TRUE,
                                  show_legend = TRUE) {
  bricks <- layout$bricks
  bridges <- layout$bridges

  # Width is the box's own character length, not the fixed drawing width
  # used by the image renderer -- positions are recomputed here in
  # characters, laid out left to right per row with a 1-space gap between
  # adjacent boxes on the same row.
  bricks[, box_text := metataal_ascii_box(token, brick, label_tokens)]
  bricks[, box_width := nchar(box_text)]

  # In bridge_mode = "bar", layout_metataal_bricks() carries the relative
  # marker's token onto its down-bar (bridges$token) and drops the marker's
  # own brick; a labelled down-bar is rendered here as a small "|token|" box so
  # the marker still reads at the clause-entry point, and it reserves that much
  # character width so following bricks clear it. Unlabelled bars stay a single
  # "|" column, as before. Bridges are in transition order, matching the
  # bridge_positions collected below, so token/label zip together by index.
  bridge_tokens <- if (!is.null(bridges) && "token" %in% names(bridges)) {
    bridges$token
  } else {
    rep(NA_character_, if (is.null(bridges)) 0L else nrow(bridges))
  }

  cursor <- c(`0` = 0L, `-1` = 0L)
  char_start <- integer(nrow(bricks))
  bridge_positions <- integer(0)
  bridge_labels <- character(0)
  bridge_widths <- integer(0)
  row_seq <- bricks$row

  # Bridge insertion points are wherever layout_metataal_bricks() detected a
  # row transition; recover them here by walking the same row sequence (a
  # trailing embedded clause still closes with an up-bridge, matching the
  # image renderer -- see layout_metataal_bricks()).
  transitions <- which(diff(row_seq) != 0)
  if (row_seq[length(row_seq)] != 0L) transitions <- c(transitions, length(row_seq))

  next_transition <- 1L
  bridge_index <- 0L
  for (i in seq_len(nrow(bricks))) {
    r <- as.character(row_seq[i])
    char_start[i] <- cursor[[r]]
    cursor[[r]] <- cursor[[r]] + bricks$box_width[i] + 1L

    if (next_transition <= length(transitions) && transitions[next_transition] == i) {
      bridge_index <- bridge_index + 1L
      tok <- if (bridge_index <= length(bridge_tokens)) bridge_tokens[bridge_index] else NA_character_
      # A labelled down-bar draws "|token|" (width nchar(token)+2); a plain bar
      # is a single "|" column (width 1).
      bar_width <- if (!is.na(tok) && label_tokens) nchar(tok) + 2L else 1L
      bridge_x <- max(cursor[["0"]], cursor[["-1"]])
      cursor[["0"]]  <- bridge_x + bar_width + 1L
      cursor[["-1"]] <- bridge_x + bar_width + 1L
      bridge_positions <- c(bridge_positions, bridge_x)
      bridge_labels <- c(bridge_labels, tok)
      bridge_widths <- c(bridge_widths, bar_width)
      next_transition <- next_transition + 1L
    }
  }
  bricks[, char_start := char_start]

  line_width <- max(bricks$char_start + bricks$box_width,
                     if (length(bridge_positions) > 0) bridge_positions + bridge_widths else 0L)

  build_row <- function(row_id) {
    line <- strrep(" ", line_width)
    row_bricks <- bricks[bricks$row == row_id, ]
    for (i in seq_len(nrow(row_bricks))) {
      pos <- row_bricks$char_start[i] + 1L
      substr(line, pos, pos + row_bricks$box_width[i] - 1L) <- row_bricks$box_text[i]
    }
    return(line)
  }

  # An unlabelled bar is a single "|" column; a labelled down-bar (bridge_mode
  # = "bar") is a "|token|" box drawn on both bar-lines so the relative marker
  # reads at the clause-entry point.
  bridge_line <- function() {
    line <- strrep(" ", line_width)
    for (k in seq_along(bridge_positions)) {
      pos <- bridge_positions[k] + 1L
      seg <- if (!is.na(bridge_labels[k]) && label_tokens) {
        paste0("|", bridge_labels[k], "|")
      } else {
        "|"
      }
      substr(line, pos, pos + nchar(seg) - 1L) <- seg
    }
    return(line)
  }

  lines <- c(build_row(0L))
  if (length(bridge_positions) > 0) {
    lines <- c(lines, bridge_line(), bridge_line(), build_row(-1L))
  }

  # One-line legend decoding the class codes used in each box (D/N/V/A/P/B),
  # stated in terms of the color each stands for in render_as = "image", so
  # the two render modes are legible against the same key. Suppressed when
  # show_legend = FALSE (e.g. drawing many sentences in one loop).
  if (show_legend) {
    legend <- paste(
      "D=Determiner(green) N=Noun(blue) V=Verb(red) A=Adjective(brown)",
      "P=Preposition(yellow) R=Adverb(pink)",
      "B=Complementizer/relative marker(grey) -=Other word (no brick)"
    )
    lines <- c(lines, "", legend)
  }

  ascii_art <- paste(lines, collapse = "\n")
  message(ascii_art)
  return(invisible(ascii_art))
}

# Internal helper: numeric sort key for token_id, so token order is correct
# even for MWT range rows ("5-6") mixed with ordinary integer ids -- takes
# the first number in the id (e.g. "5-6" -> 5, "8.1" -> 8, "12" -> 12).
token_id_num_key <- function(token_id) {
  return(as.numeric(sub("[-.].*$", "", token_id)))
}
