# ALSI Feature Inventory

Scope: features produced by the current pipeline in `R/main.R`.

Scripts covered:
- `R/fnt_tunits.R`
- `R/fnt_counters.R`
- `R/fnt_lexical.R`
- `R/fnt_burstiness.R`
- `R/fnt_heights.R`
- `R/fnt_syntactic_complexity.R`
- `R/fnt_pos_surprisal.R`
- `R/fnt_deprel_surprisal.R`
- `R/fnt_top_predictions.R`
- `R/fnt_cohesion.R`
- `R/fnt_embeddings.R`
- `R/fnt_ollama.R`
- `R/fnt_mwe.R`
- `R/main.R`

## 1) `features$simple_counts$doc_level_counts`
Produced in: `R/fnt_counters.R` (`simple_count_features`)

| Feature name | Short description | Level |
|---|---|---|
| `word_count` | Number of counted non-punctuation tokens. | document |
| `unique_word_count` | Number of unique counted tokens. | document |
| `content_word_count` | Number of content words (`NOUN`, `VERB`, `ADJ`, `ADV`, `PRON` by default). | document |
| `unique_content_word_count` | Number of unique content-word tokens. | document |
| `sentence_count` | Number of sentences. | document |
| `paragraph_count` | Number of paragraphs (if `paragraph_id` exists). | document |
| `avg_word_length` | Mean characters per word (final value recomputed as `char_count / word_count`). | document |
| `char_count` | Total character count across counted non-punctuation/non-PART tokens. | document |
| `char_count_content` | Total character count across content words. | document |
| `avg_sentence_length` | Mean words per sentence (`word_count / sentence_count`). | document |
| `avg_content_word_length` | Mean characters per content word (currently computed as `char_count / content_word_count`). | document |

## 2) `features$simple_counts$upos_counts`
Produced in: `R/fnt_counters.R` (`simple_count_features`)

| Feature name | Short description | Level |
|---|---|---|
| `count_ADJ` | Count of `ADJ` tokens. | document |
| `count_ADP` | Count of `ADP` tokens. | document |
| `count_ADV` | Count of `ADV` tokens. | document |
| `count_AUX` | Count of `AUX` tokens. | document |
| `count_CCONJ` | Count of `CCONJ` tokens. | document |
| `count_DET` | Count of `DET` tokens. | document |
| `count_INTJ` | Count of `INTJ` tokens. | document |
| `count_NOUN` | Count of `NOUN` tokens. | document |
| `count_NUM` | Count of `NUM` tokens. | document |
| `count_PART` | Count of `PART` tokens. | document |
| `count_PRON` | Count of `PRON` tokens. | document |
| `count_PROPN` | Count of `PROPN` tokens. | document |
| `count_PUNCT` | Count of `PUNCT` tokens. | document |
| `count_SCONJ` | Count of `SCONJ` tokens. | document |
| `count_SYM` | Count of `SYM` tokens. | document |
| `count_VERB` | Count of `VERB` tokens. | document |
| `count_X` | Count of `X` tokens. | document |
| `prop_ADJ` | Proportion of `ADJ` tokens within document. | document |
| `prop_ADP` | Proportion of `ADP` tokens within document. | document |
| `prop_ADV` | Proportion of `ADV` tokens within document. | document |
| `prop_AUX` | Proportion of `AUX` tokens within document. | document |
| `prop_CCONJ` | Proportion of `CCONJ` tokens within document. | document |
| `prop_DET` | Proportion of `DET` tokens within document. | document |
| `prop_INTJ` | Proportion of `INTJ` tokens within document. | document |
| `prop_NOUN` | Proportion of `NOUN` tokens within document. | document |
| `prop_NUM` | Proportion of `NUM` tokens within document. | document |
| `prop_PART` | Proportion of `PART` tokens within document. | document |
| `prop_PRON` | Proportion of `PRON` tokens within document. | document |
| `prop_PROPN` | Proportion of `PROPN` tokens within document. | document |
| `prop_PUNCT` | Proportion of `PUNCT` tokens within document. | document |
| `prop_SCONJ` | Proportion of `SCONJ` tokens within document. | document |
| `prop_SYM` | Proportion of `SYM` tokens within document. | document |
| `prop_VERB` | Proportion of `VERB` tokens within document. | document |
| `prop_X` | Proportion of `X` tokens within document. | document |

## 3) `features$verb_tenses$counts`
Produced in: `R/fnt_counters.R` (`verb_tense_features`)

| Feature name | Short description | Level |
|---|---|---|
| `present_count` | Count of tokens with `Tense=Pres`. | document |
| `past_count` | Count of tokens with `Tense=Past`. | document |
| `future_count` | Count of tokens with `Tense=Fut`. | document |
| `conditional_count` | Count of tokens with `Mood=Cnd`. | document |
| `subjunctive_count` | Count of tokens with `Mood=Sub`. | document |
| `indicative_count` | Count of tokens with `Mood=Ind`. | document |
| `imperative_count` | Count of tokens with `Mood=Imp`. | document |
| `infinitive_count` | Count of tokens with `VerbForm=Inf`. | document |
| `past_participle_count` | Count of past participles. | document |
| `present_participle_count` | Count of present participles. | document |
| `past_simple_count` | Count of finite past indicative forms. | document |
| `present_indicative_finite_count` | Countable VERB/AUX tokens jointly tagged `Mood=Ind`, `Tense=Pres`, and `VerbForm=Fin`. | document |

## 4) `features$verb_tenses$proportions`
Produced in: `R/fnt_counters.R` (`verb_tense_features`)

| Feature name | Short description | Level |
|---|---|---|
| `present_prop` | `present_count / word_count`. | document |
| `past_prop` | `past_count / word_count`. | document |
| `future_prop` | `future_count / word_count`. | document |
| `conditional_prop` | `conditional_count / word_count`. | document |
| `subjunctive_prop` | `subjunctive_count / word_count`. | document |
| `indicative_prop` | `indicative_count / word_count`. | document |
| `imperative_prop` | `imperative_count / word_count`. | document |
| `infinitive_prop` | `infinitive_count / word_count`. | document |
| `past_participle_prop` | `past_participle_count / word_count`. | document |
| `present_participle_prop` | `present_participle_count / word_count`. | document |
| `past_simple_prop` | `past_simple_count / word_count`. | document |
| `present_indicative_finite_prop` | `present_indicative_finite_count / word_count`. | document |

## 4b) `features$verb_tenses$summaries`
Produced in: `R/fnt_counters.R` (`verb_tense_features`)

For each of the twelve indicators above, `*_n` counts eligible token
observations, `*_mean` gives the share marked by that indicator, and `*_sd`
gives the sample standard deviation of its binary observations. These
statistics are computed directly across tokens in each document, without
sentence aggregation. Only countable `VERB`/`AUX` tokens are eligible.
Present/past/future require a `Tense` tag; conditional/subjunctive/indicative/
imperative require `Mood`; infinitive requires `VerbForm`. Participles require
`Tense` and `VerbForm`; past simple and present indicative finite require all
three. A missing required tag excludes that token from the corresponding
population. Empty populations have `n = 0` and missing mean/SD; singleton
populations have missing SD; constant populations of two or more have SD zero.
These derived UD-tag summaries are local descriptions of parser observations;
they do not reproduce FABRA's unresolved compound/passive verb grouping.

| Feature family | Meaning | Level |
|---|---|---|
| `<indicator>_n` | Number of eligible verb-token observations. | document |
| `<indicator>_mean` | Indicator proportion among eligible verb tokens. | document |
| `<indicator>_sd` | Sample SD of the token indicator. | document |

Indicators: `present`, `past`, `future`, `conditional`, `subjunctive`,
`indicative`, `imperative`, `infinitive`, `past_participle`,
`present_participle`, `past_simple`, and `present_indicative_finite`.

## 5) Lexical database token-level features
Produced in: `R/fnt_lexical.R` (`fuzzy_match_lexical_db`, `add_lexical_freq_with_imputation`)

Notes:
- These outputs keep token-level parsed corpus columns and append lexical features.
- `*_imp` variants add one imputed frequency feature (`<prefix>_freq_u_imputed` in current `main.R`).

### 5.1) `features$lexical_db$eqol`
| Feature name | Short description | Level |
|---|---|---|
| `eqol_freq_u` | EQOL frequency per million (matched). | word |
| `eqol_syll` | EQOL syllable information. | word |
| `eqol_upos_brut` | EQOL raw POS label. | word |
| `eqol_age` | EQOL age/grade acquisition field. | word |
| `eqol_freq_sfi` | EQOL SFI-like frequency field. | word |

### 5.2) `features$lexical_db$eqol_imp`
| Feature name | Short description | Level |
|---|---|---|
| `eqol_freq_u_imputed` | Good-Turing style imputed EQOL frequency for missing matches. | word |

### 5.3) `features$lexical_db$franqus`
| Feature name | Short description | Level |
|---|---|---|
| `franqus_grade` | Franqus grade field. | word |
| `franqus_freq_raw` | Franqus raw frequency. | word |
| `franqus_freq_u` | Franqus frequency per million. | word |
| `franqus_freq_sfi` | Franqus SFI-like frequency field. | word |
| `franqus_phonetique` | Franqus phonetic field. | word |
| `franqus_classe_de_mot` | Franqus lexical class. | word |
| `franqus_genre` | Franqus grammatical gender. | word |
| `franqus_nombre` | Franqus grammatical number. | word |
| `franqus_forme_au_singulier` | Franqus singular form field. | word |
| `franqus_age` | Franqus age/grade field. | word |

### 5.4) `features$lexical_db$franqus_imp`
| Feature name | Short description | Level |
|---|---|---|
| `franqus_freq_u_imputed` | Imputed Franqus frequency for missing matches. | word |

### 5.5) `features$lexical_db$manulex`
| Feature name | Short description | Level |
|---|---|---|
| `manulex_upos_brut` | Manulex raw POS label. | word |
| `manulex_freq_u` | Manulex frequency per million. | word |
| `manulex_freq_sfi` | Manulex SFI-like frequency field. | word |
| `manulex_freq_f` | Manulex alternate frequency field. | word |
| `manulex_grade` | Manulex grade field. | word |
| `manulex_age` | Manulex age/grade field. | word |

### 5.6) `features$lexical_db$manulex_imp`
| Feature name | Short description | Level |
|---|---|---|
| `manulex_freq_u_imputed` | Imputed Manulex frequency for missing matches. | word |

### 5.7) `features$lexical_db$flelex`
| Feature name | Short description | Level |
|---|---|---|
| `flelex_freq_u` | FLELex frequency per million. | word |
| `flelex_freq_sfi` | FLELex SFI-like frequency field. | word |
| `flelex_grade` | FLELex grade field. | word |
| `flelex_num_grade` | FLELex numeric grade field. | word |
| `flelex_freq_a1` through `flelex_freq_c2` | Source frequency by CEFR level (A1, A2, B1, B2, C1, C2). | word |

### 5.8) `features$lexical_db$flelex_doc`
Produced by `flelex_level_freq_p20()` using FLELex (François et al., 2014).
Each value is the type-7 20th percentile of matched, countable lemma/UPOS
**token occurrences** in a document. Unmatched tokens are omitted; matched
zero frequencies remain. No sentence average or word-count denominator is used.
The default match method is exact lemma/UPOS; `match_method = "fuzzy"` allows
the existing fallback priorities. These are ALSI document summaries, not
claimed FABRA equivalents.

| Feature name | Short description | Level |
|---|---|---|
| `flelex_n_matched` | Number of matched token occurrences. | document |
| `flelex_freq_a1_p20` through `flelex_freq_c2_p20` | Per-level frequency p20, ordered A1, A2, B1, B2, C1, C2. | document |

### 5.9) `features$lexical_db$flelex_imp`
| Feature name | Short description | Level |
|---|---|---|
| `flelex_freq_u_imputed` | Imputed FLELex frequency for missing matches. | word |

## 6) `features$lexical_diversity`
Produced in: `R/fnt_lexical.R` (`lexical_diversity_general`)

| Feature name | Short description | Level |
|---|---|---|
| `TTR` | Type-token ratio. | document |
| `RTTR` | Root TTR: distinct case-sensitive surface tokens divided by the square root of eligible surface-token count (Vajjala & Meurers 2012, Section 4.1, p. 166). | document |
| `maas` | Maas lexical diversity index. | document |
| `MATTR` | Moving-average type-token ratio. | document |
| `simpsons_D` | Simpson-style lexical concentration/diversity metric. | document |
| `TTR_content` | TTR on content words only. | document |
| `RTTR_content` | Root TTR on selected content surface tokens; both numerator and denominator use the content stream (Vajjala & Meurers 2012, Section 4.1, p. 166; ALSI content selection). | document |
| `maas_content` | Maas index on content words only. | document |
| `MATTR_content` | MATTR on content words only. | document |
| `simpsons_D_content` | Simpson metric on content words only. | document |
| `maas_verb` | Maas index on verbs only. | document |
| `simpsons_D_verb` | Simpson metric on verbs only. | document |

Root TTR follows the existing `lexical_diversity_general()` filter: remove
rows with missing `start` when available; use `compte == TRUE` when available,
otherwise exclude `PUNCT` when UPOS exists. The content default is NOUN,
VERB, ADJ, ADV (no PROPN); `content_upos` can change it. Types use `token`
exactly as stored, with case preserved. A document with no selected content
tokens has `NA` for `RTTR_content`; a document with no eligible tokens has no
output row. These conventions differ from the lowercase surface and
content-lemma, all-token-denominator proxies in
`docs/TCFLE8_FABRA_FEATURE_ADDITIONS.md`.

## 7) `features$heights`
Produced in: `R/fnt_heights.R` (`docwise_graph_stats`)

| Feature name | Short description | Level | References |
|---|---|---|---|
| `avg_sent_height` | Mean of per-sentence maximum root-to-token dependency depth. | document | Chen et al. (2024) [adapted: formula matches `Height` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `sd_sent_height` | Standard deviation of sentence max depth: `sd(max_path)` across sentences, or spread of per-sentence *heights* (cross-sentence consistency of how deep sentences go). | document | [derived: cross-sentence variability of tree height; not the same statistic as Chen et al.'s within-tree `depthVar`] |
| `avg_sd_depth` | Mean within-sentence SD of dependency depth: `mean(sd(node_depths))` computed per sentence over all token depths-from-root, then averaged across sentences -- spread of depths *within* a sentence's tree (tree shape/balance), not across sentences. | document | Chen et al. (2024) [adapted: same underlying statistic as `depthVar`, reported as SD instead of variance] |
| `avg_depth_var` | Mean within-sentence variance of dependency depth: same statistic as `avg_sd_depth` but reported as variance, matching Chen's units exactly. | document | Chen et al. (2024) [adapted: formula matches `depthVar` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `avg_branching_factor` | Mean node degree (dependent count) over all metric tokens, including leaves with degree 0. | document | Chen et al. (2024) [adapted: formula matches `degreeMean` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `avg_tree_degree` | Mean (across sentences) of each sentence's maximum node degree -- ALSI reports the mean of per-sentence maxima rather than a single corpus-wide max. | document | Chen et al. (2024) [adapted: `treeDegree` is per-sentence max degree; ALSI averages this across sentences instead of taking a single document-wide max] |
| `avg_degree_var` | Mean (across sentences) of each sentence's node-degree variance. | document | Chen et al. (2024) [adapted: formula matches `degreeVar` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `avg_leaf_count` | Mean (across sentences) of each sentence's leaf count (tokens with degree 0). | document | Chen et al. (2024) [adapted: `#Leaves` is summed corpus-wide in Chen; ALSI averages the per-sentence count instead] |
| `prop_leaves` | Mean (across sentences) of each sentence's leaf proportion: `leaf_count / sentence_length`. Length-normalized companion to `avg_leaf_count`. | document | [derived: `#Leaves` normalized by sentence length, no normalized analogue in Chen et al. (2024)] |
| `avg_max_incomplete_deps` | Mean (across sentences) of each sentence's peak memory cost: at every token position, sum `M(n) = n` over all currently-open dependencies (`n` = discourse referents processed since each one opened), keep the max over positions. This is Gibson's own predictor: he argues sentence acceptability tracks the *peak* memory load reached during parsing, not the average. | document | Gibson (1998) [adapted: Formula 10, his stated complexity predictor; fixed UD-upos set approximates his discourse-referent criterion] |
| `avg_incomplete_deps` | Mean (across sentences) of each sentence's *average* memory cost over positions, using the same per-position cost as `avg_max_incomplete_deps`. ALSI's own complementary aggregation -- Gibson does not propose the average as a predictor himself, only the peak. | document | Gibson (1998) [adapted, secondary aggregation: same Formula 10 cost, mean instead of max] |
| `avg_integration_cost` | Mean Gibson DLT integration cost: count of NOUN/PROPN/VERB tokens strictly between dependent and head (both endpoints excluded), per dependency arc. | document | Gibson (1998) [adapted: matches Formula 9's distance-based integration cost I(n) = n; fixed UD-upos set approximates his discourse-referent criterion] |
| `n` | Total counted sentence tokens (aggregation helper, retained in output; not an independent feature). | document | -- |
| `s` | Number of sentences (aggregation helper, retained in output; not an independent feature). | document | -- |
| `total_paths` | Sum of dependency path lengths (aggregation helper, retained in output; not an independent feature). | document | -- |
| `avg_dependency_depth` | Document-level `sum(token_depths) / sum(sentence_length)`: sentence-length-weighted mean dependency depth. | document | Chen et al. (2024) [computationally identical to `depthMean`, pooled across sentences] |
| `avg_head_distance_adj` | Mean Normalized Dependency Distance (NDD) per sentence, averaged over the document: `\|ln(MDD / sqrt(root_position * sentence_length))\|`, where `root_position` is the root token's position renumbered over non-punctuation tokens only (same scale as `sentence_length`, which also excludes punctuation). Empirically reduces (but does not eliminate) correlation with sentence length relative to `avg_head_distance` (r = 0.73 -> 0.47 on a 400-doc validation corpus); naive linear-division normalization was tried first and rejected for *increasing* the length confound. | document | Lei & Jockers (2020) [computationally identical to NDD] |
| `avg_root_position` | Mean root-token position per sentence, averaged over the document (`droot` in Chen et al.): the index of the word connecting to the pseudo-root, renumbered over non-punctuation tokens. Previously computed internally for `avg_head_distance_adj`'s normalization but not exposed as its own feature. | document | Chen et al. (2024); Lei & Jockers (2020) [computationally identical to `droot`] |
| `prop_hf` | Proportion of head-final dependencies: `count(head_final) / count(real dependencies)`, where a real dependency excludes root and punctuation tokens (root has no head, so it cannot be head-final or head-initial). `prop_hf + prop_hi == 1`. | document | Liu (2010) [computationally identical] |
| `prop_hi` | Proportion of head-initial dependencies: `count(head_initial) / count(real dependencies)`, same denominator as `prop_hf`. | document | Liu (2010) [computationally identical] |
| `avg_head_distance` | Mean absolute dependency distance: `mean(\|head_position - dependent_position\|)` over real dependencies (root, punctuation excluded). | document | Liu (2008) [computationally identical to Formula 2, sample-level MDD] |
| `max_head_distance` | Maximum dependency distance. | document | Liu (2008) [inspired: Liu discusses per-sentence max DD as a candidate syntactic complexity measure.] |
| `avg_yngve` | Mean Yngve depth per sentence, averaged over the document. Yngve depth of a word = number of right-sibling co-dependents at each ancestor level; reflects mean left-embedding processing load. | document | Yngve (1960) [adapted: branch-numbering/depth-sum formula identical, applied to dependency trees instead of his original phrase-structure trees] |
| `avg_max_yngve` | Mean of per-sentence maximum Yngve depth; captures peak embedding load. | document | Yngve (1960) [adapted: same dependency-tree adaptation as `avg_yngve`] |
| `avg_sd_yngve` | Mean of per-sentence SD of Yngve depths; captures heterogeneity (mix of shallow and deeply embedded words within sentences). | document | Yngve (1960) [adapted: same dependency-tree adaptation as `avg_yngve`] |

## 8) `features$syntactic`
Produced in: `R/fnt_syntactic_complexity.R` (`extra_syntactic_features`)

Operationalized following Lu (2010, *International Journal of Corpus Linguistics*, 15(4), 474–496), with the Penn Treebank tregex patterns *adapted* to Universal Dependencies (see caveats below). Dependency distance follows Liu (2008, *Journal of Cognitive Science*, 9(2), 159–191).

Source split: clause length, clausal density, and the dependent-clause / complex-nominal ratios are Lu (2010)-adapted. Mean dependency distance is computationally identical to Liu (2008, Formula 2), with positions renumbered over non-punctuation tokens (Hudson's "intervening words" definition, quoted by Liu). Complex-verb measures are ALSI-derived (not a Lu index). `avg_dep_count` is a derived graph statistic with no Lu/Liu source. Words before the main verb is *adapted* from Coh-Metrix's left-embeddedness index SYNLE (Graesser, McNamara & Kulikowich 2011, *Educational Researcher*, 40(5), 223–234, doi:10.3102/0013189X11413260), a working-memory load index counting words before the main verb of the main clause in a constituency parse, here approximated by the UD sentence root. (An earlier revision of this table misattributed the feature to Vajjala & Meurers 2012, which contains the Lu-14 battery and parse-tree height but no such feature.)

| Feature name | Short description | Level |
|---|---|---|
| `clausal_density` | Mean clauses per sentence (Lu C/S). Clause count = T-units plus dependent-clause heads identified by the UD relations `ccomp`, `advcl`, `acl`, `acl:relcl` (a UD approximation of Lu's finite dependent-clause set — see caveat). | document |
| `avg_clause_length` | Mean tokens per clause (Lu MLC): total counted tokens / total clauses. | document |
| `dc_per_clause` | Dependent clauses per clause (Lu DC/C): **total** dependent clauses / **total** clauses across the document (a ratio of sums, matching Lu — not a mean of per-sentence ratios). | document |
| `complex_nom_per_sent` | Mean complex nominals per sentence, using Lu (2010) CN1-style detection adapted to UD. A nominal head (NOUN, PROPN, or PRON — Lu's CN1 tregex matches NP nodes, which also head proper nouns and pronouns) with at least one child bearing `amod`, `nmod`, `nmod:poss`, `acl`, `acl:relcl`, `nummod`, `appos`, `compound`, or `det:nummod`. Covers Lu's CN1 only; CN2/CN3 clausal nominals are not included here. | document |
| `cn_per_clause` | Complex nominals per clause (Lu CN/C): total complex nominals / total clauses across the document. | document |
| `complex_verb_per_sent` | Mean complex verbs per sentence. A VERB head with at least one `aux`, `aux:pass`, `aux:tense`, or `aux:caus` child. Not one of Lu's 14 indices; added by ALSI. | document |
| `cv_per_clause` | Complex verbs per clause: total complex verbs / total clauses (ALSI analogue of Lu CN/C). Not one of Lu's 14 indices; added by ALSI. | document |
| `avg_dep_dist` | Mean Dependency Distance (MDD, Liu 2008, Formula 2): total \|position(head) − position(dependent)\| over all real (non-root, non-PUNCT) dependencies, divided by the total number of real dependencies (pooled across sentences). Positions are renumbered over non-punctuation tokens (intervening punctuation adds no distance), the same scale as `avg_head_distance` in `fnt_heights.R`. | document |
| `avg_dep_count` | Mean out-degree per token, pooled across the document: total countable children / total countable tokens, leaves counted as 0. Derived graph statistic. | document |
| `avg_words_before_root` | Mean number of tokens preceding the root (usually the main verb): for each sentence, count the tokens whose surface position is before the `root` token, then average across sentences (Coh-Metrix left embeddedness SYNLE, Graesser et al. 2011, adapted — pre-verbal working memory load). | document |
| `avg_words_after_root` | Mean number of countable tokens following the root, averaged over sentences with a root. Mirror of `avg_words_before_root`. Derived descriptive (same-name FABRA `SYNdevAFT` is a cross-reference only). | document |
| `sd_dep_count` | Pooled sample standard deviation (n − 1) of per-token out-degree; companion of `avg_dep_count`. Derived descriptive (cf. FABRA `SYNdevSUB`, cross-reference only). | document |
| `advcl_per_clause`, `ccomp_per_clause`, `xcomp_per_clause`, `csubj_per_clause`, `relcl_per_clause`, `advmod_per_clause`, `cc_per_clause` | Tokens with the UD relation (base relation; `acl:relcl` for relative clauses; for `cc`, only coordinating conjunctions whose head is a predicate, i.e. clausal coordination) / total clauses (ALSI clause count: T-units + dependent-clause heads). Adapted from TAASSC per-clause indices (Kyle & Crossley 2018; Wen, Zeng & Yu 2026). Not numerically comparable to TAASSC, whose clause is a finite verb with a subject. | document |
| `deps_per_predicate` | Countable children of predicate tokens (VERB/AUX or copular heads) / number of predicate tokens. Analogue of TAASSC `cl_av_deps`. | document |

**Caveat (clause finiteness).** The default `dc_rels` set (`ccomp`, `advcl`, `acl`, `acl:relcl`) is a UD *approximation* of Lu's finite dependent-clause definition, not an exact match: these relations are not conditioned on finiteness, so a non-finite `acl`/`advcl` head (e.g. a participial adjunct) is still counted as a dependent clause. The non-finite `xcomp` is excluded by default; `csubj`/`csubj:pass` are excluded because they are essentially absent in parsed French. Pass a custom `dc_rels` to change this.

Optional clause-counting mode: `extra_syntactic_features(..., count_parataxis = TRUE)` also follows UD `parataxis` arcs when detecting independent clauses, which can recover asyndetic main-clause coordination. The default is `FALSE` because `parataxis` also covers comment and quotative clauses that Hunt/Lu-style T-unit logic would often keep inside one unit.

### 8b) `ud_relation_proportions()` and `referential_features()`

**Derived** descriptive counts of existing parser output (`dep_rel`, `upos`, `feats`); no external citation backs them yet. FABRA (`SYNdep*`, `DISref*`) has same-named features, listed only as a cross-reference. Spec, gaps and worked example: `vignettes/specs/fabra-cheap-ud-features.Rmd`. "Words" are `compte == TRUE` tokens; a zero denominator gives `NA`.

| Feature | Definition | Level |
|---|---|---|
| `prop_dep_<rel>` | Words whose base UD relation (subtype removed) is `<rel>`, over all words. Default relations: `amod`, `nmod`, `acl`, `appos`, `conj`, `case`. | document |
| `ref_pron_per_noun` | PRON words / NOUN words (PROPN excluded). | document |
| `ref_pron_per_word` | PRON words / words. | document |
| `ref_personal_pron_per_word` | PRON with `PronType=Prs` and not `Poss=Yes` / words. | document |
| `ref_possessive_per_word` | Words with `Poss=Yes` / words. | document |
| `ref_def_art_per_noun` | DET with `Definite=Def` / NOUN words. | document |
| `ref_def_art_per_word` | DET with `Definite=Def` / words. | document |

### 8c) `features$nominals` -- `nominal_dependent_features()`

Produced in: `R/fnt_syntactic_complexity.R`. **Adapted** from the TAASSC fine-grained phrasal indices (Kyle & Crossley 2018), with index names and grade-level evidence from Wen, Zeng & Yu (2026) (who only name the indices) and behaviour read from the TAASSC 2.0.0.58 release (Kyle et al. 2021); Kyle & Crossley (2018) and Kyle (2016), where the formal definitions are, were **not read**. Several columns (`np_nmod`, `np_acl`, `np_compound`, `np_pobj_deps`, `cc_per_clause`, `deps_per_predicate`) are ALSI mappings of TAASSC concepts onto UD, closer to *inspired* than *identical*; UD relation mappings are judgement calls (spec: `vignettes/specs/kyle-nominal-clausal-ratios.Rmd`). A nominal is a `NOUN` (no pronouns or proper nouns); dependents are its children excluding `case` (UD makes the preposition a dependent of its object; in TAASSC's scheme it is the head), punctuation and multi-word-token range rows. Ratios are sums over the document; `NA` when the document has no nominal. Not implemented (definition unavailable): dependents per nominal complement, `conj_per_cl`, `ncomp_per_cl`.

| Feature | Dependents counted (UD) | TAASSC concept | Level |
|---|---|---|---|
| `np_deps_per_nominal` | all | dependents per nominal | document |
| `np_det_per_nominal` | `det` without `Poss=Yes` | determiners per nominal | document |
| `np_poss_per_nominal` | `det` with `Poss=Yes`, `nmod:poss` | possessives per nominal | document |
| `np_amod_per_nominal` | `amod` | adjectival modifiers per nominal | document |
| `np_nmod_per_nominal` | `nmod` | prepositions per nominal (mapping) | document |
| `np_relcl_per_nominal` | `acl:relcl` | relative-clause modifiers per nominal | document |
| `np_acl_per_nominal` | `acl` (non-relative) | verbal modifiers per nominal (mapping) | document |
| `np_compound_per_nominal` | `compound` | nouns as nominal dependent (mapping; ~0 in French) | document |
| `np_advmod_per_nominal` | `advmod` | adverbial modifiers per nominal | document |
| `np_deps_sd` | Sample sd (n − 1) of per-nominal dependent counts over the document's nominals (zero-dependent nominals count as 0); `NA` below two nominals. Definition of TAASSC `nominal_deps_stdev` unverified (sample vs population sd). | document |
| `np_nsubj_deps` | Mean dependents of nominals whose relation is `nsubj` (incl. `nsubj:pass`); `NA` if none. | document |
| `np_pobj_deps` | Mean dependents of nominals that carry a `case` child (objects of a preposition); `case` excluded from their count; `NA` if none. Mapping from TAASSC `pobj`. | document |

## 9) `features$pos_surprisal$doc_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

All values in bits (log₂). `pos_surprisal()` accepts `exclude_pos`, `use_sentence_boundaries`, and `backoff_scale` (Stupid Backoff, default `NULL` — see [docs/features/pos-surprisal.md](docs/features/pos-surprisal.md)).

| Feature name | Short description | Level |
|---|---|---|
| `mean_pos_surprisal` | Mean POS trigram surprisal (−log₂ p) over scored tokens. | document |
| `sd_pos_surprisal` | SD of POS surprisal — proxy for Uniform Information Density (Jaeger 2010). | document |
| `mean_pos_entropy` | Mean POS trigram-context entropy. | document |
| `sd_pos_entropy` | SD of POS context entropy. | document |
| `mean_pos_entropy_reduction` | Mean token-to-token entropy change (telescopes to ≈ 0 across a document). | document |
| `sd_pos_entropy_reduction` | SD of entropy change. | document |

## 10) `features$pos_surprisal$sent_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `mean_pos_surprisal` | Mean POS surprisal at sentence level. | sentence |
| `sd_pos_surprisal` | SD of POS surprisal at sentence level. | sentence |
| `mean_pos_entropy` | Mean POS entropy at sentence level. | sentence |
| `sd_pos_entropy` | SD of POS entropy at sentence level. | sentence |
| `mean_pos_entropy_reduction` | Mean entropy reduction at sentence level. | sentence |
| `sd_pos_entropy_reduction` | SD of entropy reduction at sentence level. | sentence |

## 11) `features$pos_surprisal$token_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `pos_surprisal` | Token-level POS surprisal (−log₂ p, bits). | word |
| `pos_entropy` | Token-level POS context entropy (bits); `NA` when Stupid Backoff reaches unigram level. | word |
| `pos_entropy_reduction` | entropy[t−1] − entropy[t] within sentence. | word |

## 11b) `features$deprel_surprisal$doc_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

Dependency-triple surprisal over tree arcs (head UPOS, dependency relation,
dependent UPOS), scoring −log₂ p(dep_pos | head_pos, dep_rel). All values in
bits (log₂). `deprel_surprisal()` accepts `exclude_pos` and `backoff_scale`
(Stupid Backoff) — see [docs/features/deprel-surprisal.md](docs/features/deprel-surprisal.md). Backed by Sidorov et al. (2014), *syntactic n-grams* (adapted: POS+deprel triples rather than syntactic paths).

| Feature name | Short description | Level |
|---|---|---|
| `mean_deprel_surprisal` | Mean dependency-triple surprisal (−log₂ p) over scored arcs. | document |
| `sd_deprel_surprisal` | SD of dependency-triple surprisal — Uniform Information Density proxy (Jaeger 2010). | document |
| `mean_deprel_entropy` | Mean (head_pos, dep_rel)-context entropy. | document |
| `sd_deprel_entropy` | SD of context entropy. | document |
| `mean_deprel_entropy_reduction` | Mean arc-to-arc entropy change in reading order. | document |
| `sd_deprel_entropy_reduction` | SD of entropy change. | document |

## 11c) `features$deprel_surprisal$sent_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `mean_deprel_surprisal` | Mean dependency-triple surprisal at sentence level. | sentence |
| `sd_deprel_surprisal` | SD of dependency-triple surprisal at sentence level. | sentence |
| `mean_deprel_entropy` | Mean context entropy at sentence level. | sentence |
| `sd_deprel_entropy` | SD of context entropy at sentence level. | sentence |
| `mean_deprel_entropy_reduction` | Mean entropy reduction at sentence level. | sentence |
| `sd_deprel_entropy_reduction` | SD of entropy reduction at sentence level. | sentence |

## 11d) `features$deprel_surprisal$token_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `deprel_surprisal` | Arc-level dependency-triple surprisal (−log₂ p, bits). | word |
| `deprel_entropy` | Arc-level (head_pos, dep_rel)-context entropy (bits); `NA` when Stupid Backoff reaches unigram level. | word |
| `deprel_entropy_reduction` | entropy[t−1] − entropy[t] in reading order within sentence. | word |

## 11e) `features$attach_surprisal$doc_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

Dependency-attachment surprisal: same arc triples as the deprel model, flipped
target — scores −log₂ p(dep_rel | head_pos, dep_pos), i.e. how predictable the
relation linking two categories is (attachment-label ambiguity). All values in
bits (log₂). `attach_surprisal()` accepts `exclude_pos` and `backoff_scale` —
see [docs/features/deprel-surprisal.md](docs/features/deprel-surprisal.md). Backed by Sidorov et al. (2014), *syntactic n-grams* (adapted).

| Feature name | Short description | Level |
|---|---|---|
| `mean_attach_surprisal` | Mean attachment-relation surprisal (−log₂ p) over scored arcs. | document |
| `sd_attach_surprisal` | SD of attachment surprisal — Uniform Information Density proxy (Jaeger 2010). | document |
| `mean_attach_entropy` | Mean (head_pos, dep_pos)-context entropy over relations (attachment ambiguity). | document |
| `sd_attach_entropy` | SD of attachment entropy. | document |
| `mean_attach_entropy_reduction` | Mean arc-to-arc entropy change in reading order. | document |
| `sd_attach_entropy_reduction` | SD of entropy change. | document |

## 11f) `features$attach_surprisal$sent_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `mean_attach_surprisal` | Mean attachment surprisal at sentence level. | sentence |
| `sd_attach_surprisal` | SD of attachment surprisal at sentence level. | sentence |
| `mean_attach_entropy` | Mean attachment entropy at sentence level. | sentence |
| `sd_attach_entropy` | SD of attachment entropy at sentence level. | sentence |
| `mean_attach_entropy_reduction` | Mean entropy reduction at sentence level. | sentence |
| `sd_attach_entropy_reduction` | SD of entropy reduction at sentence level. | sentence |

## 11g) `features$attach_surprisal$token_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

| Feature name | Short description | Level |
|---|---|---|
| `attach_surprisal` | Arc-level attachment-relation surprisal (−log₂ p, bits). | word |
| `attach_entropy` | Arc-level (head_pos, dep_pos)-context entropy over relations (bits); `NA` when backoff reaches unigram level. | word |
| `attach_entropy_reduction` | entropy[t−1] − entropy[t] in reading order within sentence. | word |

## 12) `features$lexical_cohesion`
Produced in: `R/fnt_cohesion.R` (`simple_lexical_cohesion`)

Default current context windows in `main.R`: `n_sent_context = c(1, 5)`.

| Feature name | Short description | Level |
|---|---|---|
| `token_sent_overlap_prev1` | Mean token overlap with previous 1 sentence window. | document |
| `token_sent_overlap_prev5` | Mean token overlap with previous 5 sentence window. | document |
| `lemma_sent_overlap_prev1` | Mean lemma overlap with previous 1 sentence window. | document |
| `lemma_sent_overlap_prev5` | Mean lemma overlap with previous 5 sentence window. | document |
| `token_doc_overlap` | Mean normalized token recurrence in remainder of document. | document |
| `content_sent_overlap_prev1` | Mean content-token overlap with previous 1 sentence window. | document |
| `content_sent_overlap_prev5` | Mean content-token overlap with previous 5 sentence window. | document |
| `content_lemma_sent_overlap_prev1` | Mean content-lemma overlap with previous 1 sentence window. | document |
| `content_lemma_sent_overlap_prev5` | Mean content-lemma overlap with previous 5 sentence window. | document |
| `content_doc_overlap` | Mean normalized content-token recurrence in remainder of document. | document |
| `cosine_sent` | Mean cosine similarity between adjacent sentences. | document |
| `cosine_content` | Mean cosine similarity between adjacent content-word sentence vectors. | document |
| `arg_overlap` | Proportion of adjacent sentence pairs sharing ≥1 noun/pronoun lemma (Coh-Metrix style). | document |
| `arg_overlap_content` | Same as `arg_overlap` but computed on content-word-only subset. | document |
| `global_local_gap` | Document-level minus adjacent-sentence token overlap (positive = global coherence without local repetition). | document |
| `content_global_local_gap` | Same as `global_local_gap` for content words only. | document |

## 13) `features$embeddings$dt_sent_embeddings`
Produced in: `R/fnt_embeddings.R` (`encode_embeddings`, `corpus_embeddings`)

| Feature name | Short description | Level |
|---|---|---|
| `dim1` ... `dimN` | Sentence embedding dimensions (N depends on embedding model). | sentence |

## 14) `features$embeddings$dt_doc_embeddings`
Produced in: `R/fnt_embeddings.R` (`corpus_embeddings`)

| Feature name | Short description | Level |
|---|---|---|
| `dim1` ... `dimN` | Document-level mean of sentence embedding dimensions. | document |

## 15) `features$embedding_coherence`
Produced in: `R/fnt_embeddings.R` (`embedding_coherence`)

Input: sentence embeddings from `corpus_embeddings()`. Returns one row per document.

| Feature name | Short description | Level |
|---|---|---|
| `emb_thematic_dispersion` | Mean cosine distance from each sentence to the document centroid. | document |
| `emb_centroid_distance_sd` | SD of cosine distances to centroid. | document |
| `emb_sequential_similarity` | Mean cosine similarity between consecutive sentences. | document |
| `emb_mean_semantic_gap` | Mean cosine distance between consecutive sentences. | document |
| `emb_max_semantic_gap` | Largest cosine distance between consecutive sentences. | document |
| `emb_topic_drift` | Mean cosine distance between consecutive 3-sentence block centroids. | document |
| `emb_mean_novelty` | Mean cosine distance of each sentence to the running centroid of all previous sentences. | document |
| `emb_n_topics` | Optimal number of sentence clusters via silhouette method (k = 1..min(5, n/3)). | document |
| `emb_convexity` | Conceptual convexity (Gärdenfors): mean nearest-neighbour cosine support for sampled sentence-pair midpoints, excluding the pair endpoints. 1 = strongly supported. | document |
| `emb_blob_convexity` | Proportion of sampled sentence-pair midpoints whose nearest non-endpoint sentence is within the document's median nearest-neighbour support radius. | document |
| `emb_segment_support` | Local-scale segment support: mean exp(-0.5 × normalized midpoint distance²), where normalized distance uses the geometric mean of endpoint k-NN radii. | document |
| `emb_segment_occupancy` | Proportion of sampled sentence-pair midpoints within the endpoint k-NN local support scale. | document |
| `emb_local_convexity` | Same midpoint-support score but only on consecutive sentence pairs; measures local semantic continuity. | document |
| `emb_local_blob_convexity` | Local consecutive-pair version of blob convexity. | document |
| `emb_local_segment_support` | Consecutive-pair version of local-scale segment support. | document |
| `emb_local_segment_occupancy` | Consecutive-pair version of local-scale segment occupancy. | document |

## 16) `features$surprisal$mlm` and `features$surprisal$ar`
Produced in: `R/fnt_surprisal.R` (`llm_surprisal_entropy`)

| Feature name | Short description | Level |
|---|---|---|
| `llm_surprisal` | Token-level language-model surprisal (MLM or AR, depending on call). | word |
| `llm_entropy` | Token-level predictive entropy from language model. | word |
| `llm_subword_n` | Number of subword pieces used for the token. | word |

### 16b) MLM top-k prediction inspection
Produced in: `R/fnt_top_predictions.R` (`llm_top_predictions`,
`llm_word_predictions`, `llm_beam_word_predictions`).

Inspection helpers that expose *what the masked language model expected* at
each position, alongside the surprisal of what was actually written. Not part
of the `features$` bundle — called directly on a sentence for qualitative
analysis, error inspection, and demos. Each returns a `data.table`;
`print_word_predictions()` / `print_top_predictions()` render them as
fixed-width tables.

| Function | What it returns | Level |
|---|---|---|
| `llm_top_predictions` | Per-subword top-k fill-ins (word *completions*, siblings visible), `surprisal`, `entropy`, `actual_in_topk`. | subword |
| `llm_word_predictions` | Per-word whole-word alternatives (single `[MASK]` over the word's span; single-piece candidates only), `word_prob` (`NA` for multi-piece words). | word |
| `llm_beam_word_predictions` | Per-word alternatives incl. multi-piece candidates via beam search; `word_prob` defined for every word. | word |

Citation backing: **adapted** — the pseudo-log-likelihood scorer of Salazar
et al. (2020), extended to return top-k fill-ins / whole-word completions.

## 17) Ollama LLM querying
Provided by: `R/fnt_ollama.R` (`ollama_generate`)

Not a fixed feature set — this is a general-purpose utility for querying a local LLM
(via [Ollama](https://ollama.com)) row-by-row from a data frame. Each row becomes one
stateless API call with a templated prompt; the response is stored in an
`ollama_response` column.

Typical uses: generating annotations, classifications, paraphrases, or verification
labels that can then be parsed into whatever columns are needed downstream.

Requires Ollama running locally (or on the network). See `demos/demo_ollama.R` for a
worked example that generates statements about texts and then verifies them.

## 18) `features$burstiness`
Produced in: `R/fnt_burstiness.R` (`burstiness_doc_features`)

Word-level burstiness norms (Weibull β, Negative Binomial adaptation) are derived
from a reference corpus — by default the target corpus itself, or an external
reference passed via `ref_corpus` (recommended for group comparisons).  Each
document receives a lookup-and-average over the reference-derived word properties.

| Feature name | Short description | Level |
|---|---|---|
| `mean_beta` | Mean Weibull β over all tokens with a valid estimate (β < 1 = bursty, β ≈ 1 = Poisson). | document |
| `median_beta` | Median Weibull β. | document |
| `prop_bursty` | Proportion of tokens with β < 0.6 (clearly bursty threshold, Altmann et al. 2009). | document |
| `mean_adaptation` | Mean NB adaptation ratio Pr(x≥2\|x≥1)/Pr(x≥1) over all tokens. | document |
| `median_adaptation` | Median adaptation ratio. | document |
| `mean_beta_content` | Mean β restricted to content words (NOUN, VERB, ADJ, ADV). | document |
| `prop_bursty_content` | Proportion of content-word tokens with β < 0.6. | document |
| `mean_adaptation_content` | Mean adaptation ratio restricted to content words. | document |

Also available (not in main pipeline yet): `burstiness_within_doc()` computes the
Goh–Barabási B parameter within each document independently (document-blind, no
reference corpus needed); best suited to long texts.

References: Altmann, Pierrehumbert & Motter (2009); Church & Gale (1995);
Goh & Barabási (2008). See [docs/features/burstiness.md](docs/features/burstiness.md).

## 19) MWE connective density features
Produced in: `R/fnt_mwe.R` (`connective_density_features`)

Requires a match table from `match_multiword_sequences()` against a
user-supplied connective lexicon. Output columns depend on the
`relation_group` and `cat` values present in the lexicon — the feature set is
dynamic. All raw counts are also provided as `*_per100w` density variants.
(ALSI does not bundle a connectives lexicon; the demo builds a small toy one.)

| Feature name | Short description | Level |
|---|---|---|
| `n_connectives` | Total number of connective matches. | document |
| `<relation_group>` | Count of matches per relation group in the lexicon (e.g. `contrast`, `goal`, `cause`). | document |
| `cat_<cat>` | Count of matches per grammatical category in the lexicon (e.g. `cat_cco`, `cat_adv`). | document |
| `*_per100w` | Per-100-word density variant of every count column above. | document |

See `demos/demo_mwe_matching.R` for a worked example with a hand-built toy
lexicon on the Vikidia/Wikipedia and ALECTOR corpora.

## 20) `features$tunits`
Produced in: `R/fnt_tunits.R` (`tunit_features`)

Lu's (2010) 14 L2SCA syntactic complexity indices, translated from
phrase-structure patterns to Universal Dependency style trees, plus two retained 
count helpers (`n_tunits`, `n_sentences`) and one ALSI coordination derivative
(`prop_coord_sent`). The T-unit construct has its historical origin in Hunt
(1965), but all 14 ratios below — including `mlt` and `c_t` — are sourced
from Lu (2010).

T-unit boundaries are detected from UD dependency parses: the sentence root
anchors the first T-unit; each predicate (VERB, AUX, or any token with a
`cop` child) reachable from the root via a chain of `conj` arcs **and
carrying its own overt subject** (`nsubj`, `nsubj:pass`, `nsubj:caus`,
`csubj`, `csubj:pass`, `expl`, `expl:subj`) starts an additional T-unit;
shared-subject coordination ("Marie chante et danse") stays in one T-unit
per Hunt (1965, p. 20–21). Complex nominal detection reuses
`add_complex_nominal_flag()` from `fnt_syntactic_complexity.R`, ensuring
identical operationalization across feature sets.

| Feature name | Short description | Level |
|---|---|---|
| `n_tunits` | Total T-units in the document. | document |
| `n_sentences` | Total orthographic sentences. | document |
| `mls` | Mean Length of Sentence in tokens, PUNCT excluded (Lu 2010 MLS). | document |
| `mlt` | Mean Length of T-unit in tokens, PUNCT excluded (Lu 2010 MLT). | document |
| `mlc` | Mean Length of Clause in tokens, PUNCT excluded (Lu 2010 MLC). Denominator = T-units + dependent clauses. | document |
| `c_s` | Clauses per sentence (Lu 2010 C/S). | document |
| `t_s` | T-units per sentence (Lu 2010 T/S), equivalent to Hunt's (1966) main clause coordination index. Values > 1 indicate sentences with multiple coordinated main clauses. | document |
| `c_t` | Clauses per T-unit (Lu 2010 C/T). Always ≥ 1; increases with subordination depth. | document |
| `dc_c` | Dependent clauses per clause (Lu 2010 DC/C). Finite subordinate heads (`ccomp`, `advcl`, `acl`, `acl:relcl`) / total clauses. | document |
| `dc_t` | Dependent clauses per T-unit (Lu 2010 DC/T). Same DC definition; denominator is T-units. | document |
| `ct_t` | Complex T-unit ratio (Lu 2010 CT/T): proportion of T-units containing ≥ 1 dependent clause. Computed as a sentence-level proxy — `n_dc` is counted per sentence, so when a sentence contains multiple coordinated T-units this under-resolves which specific T-unit holds the dependent clause. | document |
| `vp_t` | Verb phrases per T-unit (Lu 2010 VP/T): one verb phrase per predicate head — VERB tokens, plus AUX tokens that are not `aux`/`aux:pass`/`aux:tense`/`aux:caus` dependents of another verb — divided by n\_tunits. Auxiliary chains are folded into their governing verb so periphrastic forms (e.g. French passé composé "a mangé") count as one VP, not two. Shared-subject `conj` predicates ("chante et danse") also fold in: one coordinated VP (plus one CP), matching Lu's "VP > S" pattern. Non-finite `acl` participles are counted although Lu's pattern would skip reduced participial modifiers. | document |
| `cp_t` | Coordinate phrases per T-unit (Lu 2010 CP/T, tregex `ADJP\|ADVP\|NP\|VP < CC`): counted once per coordinated head token (the phrase), not once per `conj` arc — "A, B et C" is one coordinate phrase. Eligible head upos: NOUN/PROPN/PRON/NUM (NP), ADJ (ADJP), ADV (ADVP), VERB/AUX (VP). Conjuncts that are predicates with their own subject are clausal coordination and are excluded (they surface in `t_s`/`prop_coord_sent` instead when anchored at the root). | document |
| `cp_c` | Coordinate phrases per clause (Lu 2010 CP/C). Same CP definition as `cp_t`; denominator is total clauses. | document |
| `cn_t` | Complex nominals per T-unit (Lu 2010 CN/T): nominal tokens (NOUN, PROPN, PRON) with ≥ 1 substantive modifier child. Uses same relation set as `add_complex_nominal_flag()` in `fnt_syntactic_complexity.R`. Lu's CN1 only; CN2 (nominal clauses) and CN3 (gerund/infinitive subjects) are not counted. | document |
| `cn_c` | Complex nominals per clause (Lu 2010 CN/C). Same CN definition; denominator is total clauses. | document |
| `prop_coord_sent` | Proportion of sentences containing more than one T-unit (ALSI addition). | document |

References: Hunt (1965) NCTE Research Report No. 3 (T-unit construct); Lu
(2010) *IJCL* 15(4) (the 14 indices); Hunt (1966), *Elementary English* 43(7)
(main clause coordination index, `t_s`; reprinted in Lester, Ed. (1970),
*Readings in Applied Transformational Grammar*, p. 189).

## 21) MetaTaal LEGO brick counter
Produced in: `R/fnt_metataal.R` (`count_metataal_bricks`;
`draw_metataal_bricks` renders one document's brick sequence as ascii art or
a ggplot diagram of fixed-width colored rectangles, with embedded clauses
dropped one row down per the paper's clause-layout rule)

Tallies, per document, how many of each MetaTaal grammar LEGO brick a
UD-parsed text requires — a physical brick-ordering estimate, not a spatial
layout. One brick per counted token; a token with no corresponding brick
(adverbs, coordinating conjunctions, punctuation, numerals, non-relative
pronouns, etc.) contributes to no brick and is excluded from `n_total`.
Full brick spec, color/size mapping, and provenance notes/flagged gaps:
`vignettes/specs/metataal-lego.Rmd`.

| Feature name | Short description | Level |
|---|---|---|
| `n_det` | Green 1x2 determiner bricks (`upos == "DET"`). | document |
| `n_noun` | Blue 2x4 noun bricks (`upos` in NOUN/PROPN). | document |
| `n_verb` | Red 2x3 verb bricks (`upos` in VERB/AUX). Periphrastic/auxiliary chains count one brick per token (flagged over-count risk; see spec). | document |
| `n_adj` | Brown 2x2 adjective bricks (`upos == "ADJ"`). | document |
| `n_adp` | Yellow 2x2 preposition bricks (`upos == "ADP"`). | document |
| `n_bridge` | Grey complementizer/relative-marker bridge bricks: tokens with `PronType=Rel` in `feats`, or a `mark` dependent of a `ccomp`/`advcl`/`acl`/`acl:relcl` head. | document |
| `n_total` | Sum of the six brick counts above (ALSI convenience total). | document |

See `demos/demo_metataal_bricks.R` for a worked example on a French toy
sentence set, including the per-sentence and pooled brick-order tallies.

Reference: Zwitserlood, Wijnen, van Weerdenburg & Verhoeven (2015).
"MetaTaal": Enhancing complex syntax in children with specific language
impairment. *International Journal of Language & Communication Disorders*,
50(3), 273–297. doi:10.1111/1460-6984.12131.

## Maintenance notes
- If `n_sent_context` changes in `simple_lexical_cohesion`, overlap feature names change accordingly (`...prevK`).
- If lexical DB schemas change, prefixed lexical feature names change automatically.
- If embedding model changes, number of `dim*` features changes.
- UPOS pivot features (`count_<UPOS>`, `prop_<UPOS>`) depend on observed tags in the processed corpus.
