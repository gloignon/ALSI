# ALSI Feature Inventory

Scope: features produced by the current pipeline in `R/main.R`.

Scripts covered:
- `R/fnt_tunits.R`
- `R/fnt_counters.R`
- `R/fnt_lexical.R`
- `R/fnt_burstiness.R`
- `R/fnt_heights.R`
- `R/fnt_extra_syntax.R`
- `R/fnt_pos_surprisal.R`
- `R/fnt_deprel_surprisal.R`
- `R/fnt_cohesion.R`
- `R/fnt_embeddings.R`
- `R/fnt_ollama.R`
- `R/fnt_mwe.R`
- `R/main.R`

## 1) `features$simple_counts$doc_level_counts`
Produced in: `R/fnt_counters.R` (`simple_count_features`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `word_count` | `R/fnt_counters.R` | Number of counted non-punctuation tokens. | document |
| `unique_word_count` | `R/fnt_counters.R` | Number of unique counted tokens. | document |
| `content_word_count` | `R/fnt_counters.R` | Number of content words (`NOUN`, `VERB`, `ADJ`, `ADV`, `PRON` by default). | document |
| `unique_content_word_count` | `R/fnt_counters.R` | Number of unique content-word tokens. | document |
| `sentence_count` | `R/fnt_counters.R` | Number of sentences. | document |
| `paragraph_count` | `R/fnt_counters.R` | Number of paragraphs (if `paragraph_id` exists). | document |
| `avg_word_length` | `R/fnt_counters.R` | Mean characters per word (final value recomputed as `char_count / word_count`). | document |
| `char_count` | `R/fnt_counters.R` | Total character count across counted non-punctuation/non-PART tokens. | document |
| `char_count_content` | `R/fnt_counters.R` | Total character count across content words. | document |
| `avg_sentence_length` | `R/fnt_counters.R` | Mean words per sentence (`word_count / sentence_count`). | document |
| `avg_content_word_length` | `R/fnt_counters.R` | Mean characters per content word (currently computed as `char_count / content_word_count`). | document |

## 2) `features$simple_counts$upos_counts`
Produced in: `R/fnt_counters.R` (`simple_count_features`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `count_ADJ` | `R/fnt_counters.R` | Count of `ADJ` tokens. | document |
| `count_ADP` | `R/fnt_counters.R` | Count of `ADP` tokens. | document |
| `count_ADV` | `R/fnt_counters.R` | Count of `ADV` tokens. | document |
| `count_AUX` | `R/fnt_counters.R` | Count of `AUX` tokens. | document |
| `count_CCONJ` | `R/fnt_counters.R` | Count of `CCONJ` tokens. | document |
| `count_DET` | `R/fnt_counters.R` | Count of `DET` tokens. | document |
| `count_INTJ` | `R/fnt_counters.R` | Count of `INTJ` tokens. | document |
| `count_NOUN` | `R/fnt_counters.R` | Count of `NOUN` tokens. | document |
| `count_NUM` | `R/fnt_counters.R` | Count of `NUM` tokens. | document |
| `count_PART` | `R/fnt_counters.R` | Count of `PART` tokens. | document |
| `count_PRON` | `R/fnt_counters.R` | Count of `PRON` tokens. | document |
| `count_PROPN` | `R/fnt_counters.R` | Count of `PROPN` tokens. | document |
| `count_PUNCT` | `R/fnt_counters.R` | Count of `PUNCT` tokens. | document |
| `count_SCONJ` | `R/fnt_counters.R` | Count of `SCONJ` tokens. | document |
| `count_SYM` | `R/fnt_counters.R` | Count of `SYM` tokens. | document |
| `count_VERB` | `R/fnt_counters.R` | Count of `VERB` tokens. | document |
| `count_X` | `R/fnt_counters.R` | Count of `X` tokens. | document |
| `prop_ADJ` | `R/fnt_counters.R` | Proportion of `ADJ` tokens within document. | document |
| `prop_ADP` | `R/fnt_counters.R` | Proportion of `ADP` tokens within document. | document |
| `prop_ADV` | `R/fnt_counters.R` | Proportion of `ADV` tokens within document. | document |
| `prop_AUX` | `R/fnt_counters.R` | Proportion of `AUX` tokens within document. | document |
| `prop_CCONJ` | `R/fnt_counters.R` | Proportion of `CCONJ` tokens within document. | document |
| `prop_DET` | `R/fnt_counters.R` | Proportion of `DET` tokens within document. | document |
| `prop_INTJ` | `R/fnt_counters.R` | Proportion of `INTJ` tokens within document. | document |
| `prop_NOUN` | `R/fnt_counters.R` | Proportion of `NOUN` tokens within document. | document |
| `prop_NUM` | `R/fnt_counters.R` | Proportion of `NUM` tokens within document. | document |
| `prop_PART` | `R/fnt_counters.R` | Proportion of `PART` tokens within document. | document |
| `prop_PRON` | `R/fnt_counters.R` | Proportion of `PRON` tokens within document. | document |
| `prop_PROPN` | `R/fnt_counters.R` | Proportion of `PROPN` tokens within document. | document |
| `prop_PUNCT` | `R/fnt_counters.R` | Proportion of `PUNCT` tokens within document. | document |
| `prop_SCONJ` | `R/fnt_counters.R` | Proportion of `SCONJ` tokens within document. | document |
| `prop_SYM` | `R/fnt_counters.R` | Proportion of `SYM` tokens within document. | document |
| `prop_VERB` | `R/fnt_counters.R` | Proportion of `VERB` tokens within document. | document |
| `prop_X` | `R/fnt_counters.R` | Proportion of `X` tokens within document. | document |

## 3) `features$verb_tenses$counts`
Produced in: `R/fnt_counters.R` (`verb_tense_features`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `present_count` | `R/fnt_counters.R` | Count of tokens with `Tense=Pres`. | document |
| `past_count` | `R/fnt_counters.R` | Count of tokens with `Tense=Past`. | document |
| `future_count` | `R/fnt_counters.R` | Count of tokens with `Tense=Fut`. | document |
| `conditional_count` | `R/fnt_counters.R` | Count of tokens with `Mood=Cnd`. | document |
| `subjunctive_count` | `R/fnt_counters.R` | Count of tokens with `Mood=Sub`. | document |
| `indicative_count` | `R/fnt_counters.R` | Count of tokens with `Mood=Ind`. | document |
| `imperative_count` | `R/fnt_counters.R` | Count of tokens with `Mood=Imp`. | document |
| `infinitive_count` | `R/fnt_counters.R` | Count of tokens with `VerbForm=Inf`. | document |
| `past_participle_count` | `R/fnt_counters.R` | Count of past participles. | document |
| `present_participle_count` | `R/fnt_counters.R` | Count of present participles. | document |
| `past_simple_count` | `R/fnt_counters.R` | Count of finite past indicative forms. | document |

## 4) `features$verb_tenses$proportions`
Produced in: `R/fnt_counters.R` (`verb_tense_features`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `present_prop` | `R/fnt_counters.R` | `present_count / word_count`. | document |
| `past_prop` | `R/fnt_counters.R` | `past_count / word_count`. | document |
| `future_prop` | `R/fnt_counters.R` | `future_count / word_count`. | document |
| `conditional_prop` | `R/fnt_counters.R` | `conditional_count / word_count`. | document |
| `subjunctive_prop` | `R/fnt_counters.R` | `subjunctive_count / word_count`. | document |
| `indicative_prop` | `R/fnt_counters.R` | `indicative_count / word_count`. | document |
| `imperative_prop` | `R/fnt_counters.R` | `imperative_count / word_count`. | document |
| `infinitive_prop` | `R/fnt_counters.R` | `infinitive_count / word_count`. | document |
| `past_participle_prop` | `R/fnt_counters.R` | `past_participle_count / word_count`. | document |
| `present_participle_prop` | `R/fnt_counters.R` | `present_participle_count / word_count`. | document |
| `past_simple_prop` | `R/fnt_counters.R` | `past_simple_count / word_count`. | document |

## 5) Lexical database token-level features
Produced in: `R/fnt_lexical.R` (`fuzzy_match_lexical_db`, `add_lexical_freq_with_imputation`)

Notes:
- These outputs keep token-level parsed corpus columns and append lexical features.
- `*_imp` variants add one imputed frequency feature (`<prefix>_freq_u_imputed` in current `main.R`).

### 5.1) `features$lexical_db$eqol`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `eqol_freq_u` | `R/fnt_lexical.R` | EQOL frequency per million (matched). | word |
| `eqol_syll` | `R/fnt_lexical.R` | EQOL syllable information. | word |
| `eqol_upos_brut` | `R/fnt_lexical.R` | EQOL raw POS label. | word |
| `eqol_age` | `R/fnt_lexical.R` | EQOL age/grade acquisition field. | word |
| `eqol_freq_sfi` | `R/fnt_lexical.R` | EQOL SFI-like frequency field. | word |

### 5.2) `features$lexical_db$eqol_imp`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `eqol_freq_u_imputed` | `R/fnt_lexical.R` | Good-Turing style imputed EQOL frequency for missing matches. | word |

### 5.3) `features$lexical_db$franqus`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `franqus_grade` | `R/fnt_lexical.R` | Franqus grade field. | word |
| `franqus_freq_raw` | `R/fnt_lexical.R` | Franqus raw frequency. | word |
| `franqus_freq_u` | `R/fnt_lexical.R` | Franqus frequency per million. | word |
| `franqus_freq_sfi` | `R/fnt_lexical.R` | Franqus SFI-like frequency field. | word |
| `franqus_phonetique` | `R/fnt_lexical.R` | Franqus phonetic field. | word |
| `franqus_classe_de_mot` | `R/fnt_lexical.R` | Franqus lexical class. | word |
| `franqus_genre` | `R/fnt_lexical.R` | Franqus grammatical gender. | word |
| `franqus_nombre` | `R/fnt_lexical.R` | Franqus grammatical number. | word |
| `franqus_forme_au_singulier` | `R/fnt_lexical.R` | Franqus singular form field. | word |
| `franqus_age` | `R/fnt_lexical.R` | Franqus age/grade field. | word |

### 5.4) `features$lexical_db$franqus_imp`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `franqus_freq_u_imputed` | `R/fnt_lexical.R` | Imputed Franqus frequency for missing matches. | word |

### 5.5) `features$lexical_db$manulex`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `manulex_upos_brut` | `R/fnt_lexical.R` | Manulex raw POS label. | word |
| `manulex_freq_u` | `R/fnt_lexical.R` | Manulex frequency per million. | word |
| `manulex_freq_sfi` | `R/fnt_lexical.R` | Manulex SFI-like frequency field. | word |
| `manulex_freq_f` | `R/fnt_lexical.R` | Manulex alternate frequency field. | word |
| `manulex_grade` | `R/fnt_lexical.R` | Manulex grade field. | word |
| `manulex_age` | `R/fnt_lexical.R` | Manulex age/grade field. | word |

### 5.6) `features$lexical_db$manulex_imp`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `manulex_freq_u_imputed` | `R/fnt_lexical.R` | Imputed Manulex frequency for missing matches. | word |

### 5.7) `features$lexical_db$flelex`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `flelex_freq_u` | `R/fnt_lexical.R` | FLELex frequency per million. | word |
| `flelex_freq_sfi` | `R/fnt_lexical.R` | FLELex SFI-like frequency field. | word |
| `flelex_grade` | `R/fnt_lexical.R` | FLELex grade field. | word |
| `flelex_num_grade` | `R/fnt_lexical.R` | FLELex numeric grade field. | word |

### 5.8) `features$lexical_db$flelex_imp`
| Feature name | Script | Short description | Level |
|---|---|---|---|
| `flelex_freq_u_imputed` | `R/fnt_lexical.R` | Imputed FLELex frequency for missing matches. | word |

## 6) `features$lexical_diversity`
Produced in: `R/fnt_lexical.R` (`lexical_diversity_general`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `TTR` | `R/fnt_lexical.R` | Type-token ratio. | document |
| `maas` | `R/fnt_lexical.R` | Maas lexical diversity index. | document |
| `MATTR` | `R/fnt_lexical.R` | Moving-average type-token ratio. | document |
| `simpsons_D` | `R/fnt_lexical.R` | Simpson-style lexical concentration/diversity metric. | document |
| `TTR_content` | `R/fnt_lexical.R` | TTR on content words only. | document |
| `maas_content` | `R/fnt_lexical.R` | Maas index on content words only. | document |
| `MATTR_content` | `R/fnt_lexical.R` | MATTR on content words only. | document |
| `simpsons_D_content` | `R/fnt_lexical.R` | Simpson metric on content words only. | document |
| `maas_verb` | `R/fnt_lexical.R` | Maas index on verbs only. | document |
| `simpsons_D_verb` | `R/fnt_lexical.R` | Simpson metric on verbs only. | document |

## 7) `features$heights`
Produced in: `R/fnt_heights.R` (`docwise_graph_stats`)

| Feature name | Script | Short description | Level | References |
|---|---|---|---|---|
| `avg_sent_height` | `R/fnt_heights.R` | Mean of per-sentence maximum root-to-token dependency depth. | document | Chen et al. (2024) [adapted: formula matches `Height` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `sd_sent_height` | `R/fnt_heights.R` | Standard deviation of sentence max depth: `sd(max_path)` across sentences, or spread of per-sentence *heights* (cross-sentence consistency of how deep sentences go). | document | [derived: cross-sentence variability of tree height; not the same statistic as Chen et al.'s within-tree `depthVar`] |
| `avg_sd_depth` | `R/fnt_heights.R` | Mean within-sentence SD of dependency depth: `mean(sd(node_depths))` computed per sentence over all token depths-from-root, then averaged across sentences -- spread of depths *within* a sentence's tree (tree shape/balance), not across sentences. | document | Chen et al. (2024) [adapted: same underlying statistic as `depthVar`, reported as SD instead of variance] |
| `avg_branching_factor` | `R/fnt_heights.R` | Mean node degree (dependent count) over all metric tokens, including leaves with degree 0. | document | Chen et al. (2024) [adapted: formula matches `degreeMean` exactly when punctuation policy aligns, but ALSI includes punctuation by default while Chen's processing excludes it] |
| `avg_max_incomplete_deps` | `R/fnt_heights.R` | Mean (across sentences) of each sentence's peak memory cost: at every token position, sum `M(n) = n` over all currently-open dependencies (`n` = discourse referents processed since each one opened), keep the max over positions. This is Gibson's own predictor: he argues sentence acceptability tracks the *peak* memory load reached during parsing, not the average. | document | Gibson (1998) [adapted: Formula 10, his stated complexity predictor; fixed UD-upos set approximates his discourse-referent criterion] |
| `avg_incomplete_deps` | `R/fnt_heights.R` | Mean (across sentences) of each sentence's *average* memory cost over positions, using the same per-position cost as `avg_max_incomplete_deps`. ALSI's own complementary aggregation -- Gibson does not propose the average as a predictor himself, only the peak. | document | Gibson (1998) [adapted, secondary aggregation: same Formula 10 cost, mean instead of max] |
| `avg_integration_cost` | `R/fnt_heights.R` | Mean Gibson DLT integration cost: count of NOUN/PROPN/VERB tokens strictly between dependent and head (both endpoints excluded), per dependency arc. | document | Gibson (1998) [adapted: matches Formula 9's distance-based integration cost I(n) = n; fixed UD-upos set approximates his discourse-referent criterion] |
| `n` | `R/fnt_heights.R` | Total counted sentence tokens (aggregation helper, retained in output; not an independent feature). | document | -- |
| `s` | `R/fnt_heights.R` | Number of sentences (aggregation helper, retained in output; not an independent feature). | document | -- |
| `total_paths` | `R/fnt_heights.R` | Sum of dependency path lengths (aggregation helper, retained in output; not an independent feature). | document | -- |
| `avg_dependency_depth` | `R/fnt_heights.R` | Document-level `sum(token_depths) / sum(sentence_length)`: sentence-length-weighted mean dependency depth. | document | Chen et al. (2024) [computationally identical to `depthMean`, pooled across sentences] |
| `avg_head_distance_adj` | `R/fnt_heights.R` | Mean Normalized Dependency Distance (NDD) per sentence, averaged over the document: `\|ln(MDD / sqrt(root_position * sentence_length))\|`, where `root_position` is the root token's position renumbered over non-punctuation tokens only (same scale as `sentence_length`, which also excludes punctuation). Empirically reduces (but does not eliminate) correlation with sentence length relative to `avg_head_distance` (r = 0.73 -> 0.47 on a 400-doc validation corpus); naive linear-division normalization was tried first and rejected for *increasing* the length confound. | document | Lei & Jockers (2020) [computationally identical to NDD] |
| `prop_hf` | `R/fnt_heights.R` | Proportion of head-final dependencies: `count(head_final) / count(real dependencies)`, where a real dependency excludes root and punctuation tokens (root has no head, so it cannot be head-final or head-initial). `prop_hf + prop_hi == 1`. | document | Liu (2010) [computationally identical] |
| `prop_hi` | `R/fnt_heights.R` | Proportion of head-initial dependencies: `count(head_initial) / count(real dependencies)`, same denominator as `prop_hf`. | document | Liu (2010) [computationally identical] |
| `avg_head_distance` | `R/fnt_heights.R` | Mean absolute dependency distance: `mean(\|head_position - dependent_position\|)` over real dependencies (root, punctuation excluded). | document | Liu (2008) [computationally identical to Formula 2, sample-level MDD] |
| `max_head_distance` | `R/fnt_heights.R` | Maximum dependency distance. | document | Liu (2008) [inspired: Liu discusses per-sentence max DD as a candidate syntactic complexity measure.] |
| `max_head_distance_adj` | `R/fnt_heights.R` | Maximum normalized dependency distance (`head_distance / (sentence_length - 1)`), bounded in `[0, 1]`. | document | Liu (2008) [inspired: length-normalized version of their max-DD concept] |
| `avg_yngve` | `R/fnt_heights.R` | Mean Yngve depth per sentence, averaged over the document. Yngve depth of a word = number of right-sibling co-dependents at each ancestor level; reflects mean left-embedding processing load. | document | Yngve (1960) [adapted: branch-numbering/depth-sum formula identical, applied to dependency trees instead of his original phrase-structure trees] |
| `avg_max_yngve` | `R/fnt_heights.R` | Mean of per-sentence maximum Yngve depth; captures peak embedding load. | document | Yngve (1960) [adapted: same dependency-tree adaptation as `avg_yngve`] |
| `avg_sd_yngve` | `R/fnt_heights.R` | Mean of per-sentence SD of Yngve depths; captures heterogeneity (mix of shallow and deeply embedded words within sentences). | document | Yngve (1960) [adapted: same dependency-tree adaptation as `avg_yngve`] |

Reference scope: Lu (2010, *IJCL*) is used for the 14 L2SCA syntactic-complexity measures documented under `features$tunits`; in this table Liu (2008, *Journal of Cognitive Science*) covers dependency distance, Liu (2010, *Lingua*) covers head directionality, Gibson (1998) covers DLT-based costs, Yngve (1960) covers Yngve depth, Chen et al. (2024, arXiv:2402.11549, "Syntactic Language Change in English and German: Metrics, Parsers, and Convergences") covers the tree-height/depth/degree family (`Height`, `depthMean`, `depthVar`, `degreeMean`), and Lei & Jockers (2020, *Journal of Quantitative Linguistics*, "Normalized Dependency Distance") covers the length-normalized `avg_head_distance_adj`.

Note: four previously-documented length-normalization features (`avg_sent_height_adj`, `avg_dependency_depth_adj`, `avg_max_incomplete_deps_adj`, `avg_incomplete_deps_adj`) were removed after empirical validation showed naive division by `(sentence_length - 1)` did not decorrelate them from sentence length -- in most cases it introduced a new, larger, sign-flipped correlation. `max_head_distance_adj` was kept (verified to work); `avg_head_distance_adj` was replaced with the principled Lei & Jockers (2020) NDD formula rather than dropped.

Note: `dependency_direction_index` (a signed `prop_hf - prop_hi` combination) was removed -- Liu (2010) never defines a combined head-final/head-initial index; he reports the two percentages separately and treats either one alone as sufficient for typological positioning (since they are complements: `prop_hi = 1 - prop_hf`). `prop_hf`/`prop_hi` remain as the genuine Liu (2010) statistics.

Note: `avg_max_incomplete_deps`/`avg_incomplete_deps` were re-implemented to compute Gibson's actual Formula 10 memory cost (weighted by intervening discourse referents) instead of a naive unweighted count of open dependencies. The unweighted count was the simpler "stacking incompletely parsed rules" precursor idea Gibson (1998) explicitly attributes to Yngve (1960), not his own SPLT memory-cost hypothesis; the weighted version reuses the same discourse-referent-counting mechanism already implemented for `avg_integration_cost`. As a result, locally-adjacent open dependencies (no NOUN/PROPN/VERB intervening) now correctly cost 0, matching Gibson's prediction that memory cost grows with intervening material, not with the raw count of open dependencies.


## 8) `features$syntactic`
Produced in: `R/fnt_extra_syntax.R` (`extra_syntactic_features`)

Operationalized following Lu (2010, *International Journal of Corpus Linguistics*, 15(4), 474–496), translated from Penn Treebank tregex patterns to Universal Dependencies. Dependency distance follows Liu (2008, *Journal of Cognitive Science*, 9(2), 159–191).

Source split: clause length, clausal density, dependent-clause ratios, and complex nominal measures are Lu (2010)-derived. Mean dependency distance is Liu (2008).

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `clausal_density` | `R/fnt_extra_syntax.R` | Mean clauses per sentence (Lu C/S). Clause count = T-units plus finite dependent-clause heads identified by UD relations `ccomp`, `advcl`, `acl`, `acl:relcl`. | document |
| `avg_clause_length` | `R/fnt_extra_syntax.R` | Mean tokens per clause (Lu MLC): total counted tokens / total clauses. | document |
| `dc_per_clause` | `R/fnt_extra_syntax.R` | Mean proportion of clauses that are dependent clauses (Lu DC/C): mean(dependent clauses / total clauses) per sentence. | document |
| `complex_nom_per_sent` | `R/fnt_extra_syntax.R` | Mean complex nominals per sentence, using Lu (2010)-derived complex nominal detection. A NOUN head with at least one child bearing `amod`, `nmod`, `nmod:poss`, `acl`, `acl:relcl`, `nummod`, `appos`, `compound`, or `det:nummod`. | document |
| `cn_per_clause` | `R/fnt_extra_syntax.R` | Mean complex nominals per clause (Lu CN/C). | document |
| `complex_verb_per_sent` | `R/fnt_extra_syntax.R` | Mean complex verbs per sentence. A VERB head with at least one `aux`, `aux:pass`, `aux:tense`, or `aux:caus` child. Not one of Lu's 14 indices; added by ALSI. | document |
| `cv_per_clause` | `R/fnt_extra_syntax.R` | Mean complex verbs per clause. Analogous to Lu CN/C but for complex verbs. Not one of Lu's 14 indices; added by ALSI. | document |
| `avg_dep_dist` | `R/fnt_extra_syntax.R` | Mean Dependency Distance (MDD, Liu 2008): mean \|position(head) − position(dependent)\| over non-PUNCT tokens. | document |
| `avg_dep_count` | `R/fnt_extra_syntax.R` | Mean number of dependents per token (mean out-degree in the dependency tree). | document |

## 9) `features$pos_surprisal$doc_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

All values in bits (log₂). `pos_surprisal()` accepts `exclude_pos`, `use_sentence_boundaries`, and `backoff_scale` (Stupid Backoff, default `NULL` — see [docs/features/pos-surprisal.md](docs/features/pos-surprisal.md)).

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_pos_surprisal` | `R/fnt_pos_surprisal.R` | Mean POS trigram surprisal (−log₂ p) over scored tokens. | document |
| `sd_pos_surprisal` | `R/fnt_pos_surprisal.R` | SD of POS surprisal — proxy for Uniform Information Density (Jaeger 2010). | document |
| `mean_pos_entropy` | `R/fnt_pos_surprisal.R` | Mean POS trigram-context entropy. | document |
| `sd_pos_entropy` | `R/fnt_pos_surprisal.R` | SD of POS context entropy. | document |
| `mean_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | Mean token-to-token entropy change (telescopes to ≈ 0 across a document). | document |
| `sd_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | SD of entropy change. | document |

## 10) `features$pos_surprisal$sent_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_pos_surprisal` | `R/fnt_pos_surprisal.R` | Mean POS surprisal at sentence level. | sentence |
| `sd_pos_surprisal` | `R/fnt_pos_surprisal.R` | SD of POS surprisal at sentence level. | sentence |
| `mean_pos_entropy` | `R/fnt_pos_surprisal.R` | Mean POS entropy at sentence level. | sentence |
| `sd_pos_entropy` | `R/fnt_pos_surprisal.R` | SD of POS entropy at sentence level. | sentence |
| `mean_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | Mean entropy reduction at sentence level. | sentence |
| `sd_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | SD of entropy reduction at sentence level. | sentence |

## 11) `features$pos_surprisal$token_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `pos_surprisal` | `R/fnt_pos_surprisal.R` | Token-level POS surprisal (−log₂ p, bits). | word |
| `pos_entropy` | `R/fnt_pos_surprisal.R` | Token-level POS context entropy (bits); `NA` when Stupid Backoff reaches unigram level. | word |
| `pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | entropy[t−1] − entropy[t] within sentence. | word |

## 11b) `features$deprel_surprisal$doc_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

Dependency-triple surprisal over tree arcs (head UPOS, dependency relation,
dependent UPOS), scoring −log₂ p(dep_pos | head_pos, dep_rel). All values in
bits (log₂). `deprel_surprisal()` accepts `exclude_pos` and `backoff_scale`
(Stupid Backoff) — see [docs/features/deprel-surprisal.md](docs/features/deprel-surprisal.md). Backed by Sidorov et al. (2014), *syntactic n-grams* (adapted: POS+deprel triples rather than syntactic paths).

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_deprel_surprisal` | `R/fnt_deprel_surprisal.R` | Mean dependency-triple surprisal (−log₂ p) over scored arcs. | document |
| `sd_deprel_surprisal` | `R/fnt_deprel_surprisal.R` | SD of dependency-triple surprisal — Uniform Information Density proxy (Jaeger 2010). | document |
| `mean_deprel_entropy` | `R/fnt_deprel_surprisal.R` | Mean (head_pos, dep_rel)-context entropy. | document |
| `sd_deprel_entropy` | `R/fnt_deprel_surprisal.R` | SD of context entropy. | document |
| `mean_deprel_entropy_reduction` | `R/fnt_deprel_surprisal.R` | Mean arc-to-arc entropy change in reading order. | document |
| `sd_deprel_entropy_reduction` | `R/fnt_deprel_surprisal.R` | SD of entropy change. | document |

## 11c) `features$deprel_surprisal$sent_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_deprel_surprisal` | `R/fnt_deprel_surprisal.R` | Mean dependency-triple surprisal at sentence level. | sentence |
| `sd_deprel_surprisal` | `R/fnt_deprel_surprisal.R` | SD of dependency-triple surprisal at sentence level. | sentence |
| `mean_deprel_entropy` | `R/fnt_deprel_surprisal.R` | Mean context entropy at sentence level. | sentence |
| `sd_deprel_entropy` | `R/fnt_deprel_surprisal.R` | SD of context entropy at sentence level. | sentence |
| `mean_deprel_entropy_reduction` | `R/fnt_deprel_surprisal.R` | Mean entropy reduction at sentence level. | sentence |
| `sd_deprel_entropy_reduction` | `R/fnt_deprel_surprisal.R` | SD of entropy reduction at sentence level. | sentence |

## 11d) `features$deprel_surprisal$token_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`deprel_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `deprel_surprisal` | `R/fnt_deprel_surprisal.R` | Arc-level dependency-triple surprisal (−log₂ p, bits). | word |
| `deprel_entropy` | `R/fnt_deprel_surprisal.R` | Arc-level (head_pos, dep_rel)-context entropy (bits); `NA` when Stupid Backoff reaches unigram level. | word |
| `deprel_entropy_reduction` | `R/fnt_deprel_surprisal.R` | entropy[t−1] − entropy[t] in reading order within sentence. | word |

## 11e) `features$attach_surprisal$doc_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

Dependency-attachment surprisal: same arc triples as the deprel model, flipped
target — scores −log₂ p(dep_rel | head_pos, dep_pos), i.e. how predictable the
relation linking two categories is (attachment-label ambiguity). All values in
bits (log₂). `attach_surprisal()` accepts `exclude_pos` and `backoff_scale` —
see [docs/features/deprel-surprisal.md](docs/features/deprel-surprisal.md). Backed by Sidorov et al. (2014), *syntactic n-grams* (adapted).

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_attach_surprisal` | `R/fnt_deprel_surprisal.R` | Mean attachment-relation surprisal (−log₂ p) over scored arcs. | document |
| `sd_attach_surprisal` | `R/fnt_deprel_surprisal.R` | SD of attachment surprisal — Uniform Information Density proxy (Jaeger 2010). | document |
| `mean_attach_entropy` | `R/fnt_deprel_surprisal.R` | Mean (head_pos, dep_pos)-context entropy over relations (attachment ambiguity). | document |
| `sd_attach_entropy` | `R/fnt_deprel_surprisal.R` | SD of attachment entropy. | document |
| `mean_attach_entropy_reduction` | `R/fnt_deprel_surprisal.R` | Mean arc-to-arc entropy change in reading order. | document |
| `sd_attach_entropy_reduction` | `R/fnt_deprel_surprisal.R` | SD of entropy change. | document |

## 11f) `features$attach_surprisal$sent_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_attach_surprisal` | `R/fnt_deprel_surprisal.R` | Mean attachment surprisal at sentence level. | sentence |
| `sd_attach_surprisal` | `R/fnt_deprel_surprisal.R` | SD of attachment surprisal at sentence level. | sentence |
| `mean_attach_entropy` | `R/fnt_deprel_surprisal.R` | Mean attachment entropy at sentence level. | sentence |
| `sd_attach_entropy` | `R/fnt_deprel_surprisal.R` | SD of attachment entropy at sentence level. | sentence |
| `mean_attach_entropy_reduction` | `R/fnt_deprel_surprisal.R` | Mean entropy reduction at sentence level. | sentence |
| `sd_attach_entropy_reduction` | `R/fnt_deprel_surprisal.R` | SD of entropy reduction at sentence level. | sentence |

## 11g) `features$attach_surprisal$token_surprisal`
Produced in: `R/fnt_deprel_surprisal.R` (`attach_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `attach_surprisal` | `R/fnt_deprel_surprisal.R` | Arc-level attachment-relation surprisal (−log₂ p, bits). | word |
| `attach_entropy` | `R/fnt_deprel_surprisal.R` | Arc-level (head_pos, dep_pos)-context entropy over relations (bits); `NA` when backoff reaches unigram level. | word |
| `attach_entropy_reduction` | `R/fnt_deprel_surprisal.R` | entropy[t−1] − entropy[t] in reading order within sentence. | word |

## 12) `features$lexical_cohesion`
Produced in: `R/fnt_cohesion.R` (`simple_lexical_cohesion`)

Default current context windows in `main.R`: `n_sent_context = c(1, 5)`.

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `token_sent_overlap_prev1` | `R/fnt_cohesion.R` | Mean token overlap with previous 1 sentence window. | document |
| `token_sent_overlap_prev5` | `R/fnt_cohesion.R` | Mean token overlap with previous 5 sentence window. | document |
| `lemma_sent_overlap_prev1` | `R/fnt_cohesion.R` | Mean lemma overlap with previous 1 sentence window. | document |
| `lemma_sent_overlap_prev5` | `R/fnt_cohesion.R` | Mean lemma overlap with previous 5 sentence window. | document |
| `token_doc_overlap` | `R/fnt_cohesion.R` | Mean normalized token recurrence in remainder of document. | document |
| `content_sent_overlap_prev1` | `R/fnt_cohesion.R` | Mean content-token overlap with previous 1 sentence window. | document |
| `content_sent_overlap_prev5` | `R/fnt_cohesion.R` | Mean content-token overlap with previous 5 sentence window. | document |
| `content_lemma_sent_overlap_prev1` | `R/fnt_cohesion.R` | Mean content-lemma overlap with previous 1 sentence window. | document |
| `content_lemma_sent_overlap_prev5` | `R/fnt_cohesion.R` | Mean content-lemma overlap with previous 5 sentence window. | document |
| `content_doc_overlap` | `R/fnt_cohesion.R` | Mean normalized content-token recurrence in remainder of document. | document |
| `cosine_sent` | `R/fnt_cohesion.R` | Mean cosine similarity between adjacent sentences. | document |
| `cosine_content` | `R/fnt_cohesion.R` | Mean cosine similarity between adjacent content-word sentence vectors. | document |
| `arg_overlap` | `R/fnt_cohesion.R` | Proportion of adjacent sentence pairs sharing ≥1 noun/pronoun lemma (Coh-Metrix style). | document |
| `arg_overlap_content` | `R/fnt_cohesion.R` | Same as `arg_overlap` but computed on content-word-only subset. | document |
| `global_local_gap` | `R/fnt_cohesion.R` | Document-level minus adjacent-sentence token overlap (positive = global coherence without local repetition). | document |
| `content_global_local_gap` | `R/fnt_cohesion.R` | Same as `global_local_gap` for content words only. | document |

## 13) `features$embeddings$dt_sent_embeddings`
Produced in: `R/fnt_embeddings.R` (`encode_embeddings`, `corpus_embeddings`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `dim1` ... `dimN` | `R/fnt_embeddings.R` | Sentence embedding dimensions (N depends on embedding model). | sentence |

## 14) `features$embeddings$dt_doc_embeddings`
Produced in: `R/fnt_embeddings.R` (`corpus_embeddings`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `dim1` ... `dimN` | `R/fnt_embeddings.R` | Document-level mean of sentence embedding dimensions. | document |

## 15) `features$embedding_coherence`
Produced in: `R/fnt_embeddings.R` (`embedding_coherence`)

Input: sentence embeddings from `corpus_embeddings()`. Returns one row per document.

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `emb_thematic_dispersion` | `R/fnt_embeddings.R` | Mean cosine distance from each sentence to the document centroid. | document |
| `emb_centroid_distance_sd` | `R/fnt_embeddings.R` | SD of cosine distances to centroid. | document |
| `emb_sequential_similarity` | `R/fnt_embeddings.R` | Mean cosine similarity between consecutive sentences. | document |
| `emb_mean_semantic_gap` | `R/fnt_embeddings.R` | Mean cosine distance between consecutive sentences. | document |
| `emb_max_semantic_gap` | `R/fnt_embeddings.R` | Largest cosine distance between consecutive sentences. | document |
| `emb_topic_drift` | `R/fnt_embeddings.R` | Mean cosine distance between consecutive 3-sentence block centroids. | document |
| `emb_mean_novelty` | `R/fnt_embeddings.R` | Mean cosine distance of each sentence to the running centroid of all previous sentences. | document |
| `emb_n_topics` | `R/fnt_embeddings.R` | Optimal number of sentence clusters via silhouette method (k = 1..min(5, n/3)). | document |
| `emb_convexity` | `R/fnt_embeddings.R` | Conceptual convexity (Gärdenfors): mean nearest-neighbour cosine support for sampled sentence-pair midpoints, excluding the pair endpoints. 1 = strongly supported. | document |
| `emb_blob_convexity` | `R/fnt_embeddings.R` | Proportion of sampled sentence-pair midpoints whose nearest non-endpoint sentence is within the document's median nearest-neighbour support radius. | document |
| `emb_segment_support` | `R/fnt_embeddings.R` | Local-scale segment support: mean exp(-0.5 × normalized midpoint distance²), where normalized distance uses the geometric mean of endpoint k-NN radii. | document |
| `emb_segment_occupancy` | `R/fnt_embeddings.R` | Proportion of sampled sentence-pair midpoints within the endpoint k-NN local support scale. | document |
| `emb_local_convexity` | `R/fnt_embeddings.R` | Same midpoint-support score but only on consecutive sentence pairs; measures local semantic continuity. | document |
| `emb_local_blob_convexity` | `R/fnt_embeddings.R` | Local consecutive-pair version of blob convexity. | document |
| `emb_local_segment_support` | `R/fnt_embeddings.R` | Consecutive-pair version of local-scale segment support. | document |
| `emb_local_segment_occupancy` | `R/fnt_embeddings.R` | Consecutive-pair version of local-scale segment occupancy. | document |

## 16) `features$surprisal$mlm` and `features$surprisal$ar`
Produced in: `R/fnt_surprisal.R` (`llm_surprisal_entropy`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `llm_surprisal` | `R/fnt_surprisal.R` | Token-level language-model surprisal (MLM or AR, depending on call). | word |
| `llm_entropy` | `R/fnt_surprisal.R` | Token-level predictive entropy from language model. | word |
| `llm_subword_n` | `R/fnt_surprisal.R` | Number of subword pieces used for the token. | word |

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

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_beta` | `R/fnt_burstiness.R` | Mean Weibull β over all tokens with a valid estimate (β < 1 = bursty, β ≈ 1 = Poisson). | document |
| `median_beta` | `R/fnt_burstiness.R` | Median Weibull β. | document |
| `prop_bursty` | `R/fnt_burstiness.R` | Proportion of tokens with β < 0.6 (clearly bursty threshold, Altmann et al. 2009). | document |
| `mean_adaptation` | `R/fnt_burstiness.R` | Mean NB adaptation ratio Pr(x≥2\|x≥1)/Pr(x≥1) over all tokens. | document |
| `median_adaptation` | `R/fnt_burstiness.R` | Median adaptation ratio. | document |
| `mean_beta_content` | `R/fnt_burstiness.R` | Mean β restricted to content words (NOUN, VERB, ADJ, ADV). | document |
| `prop_bursty_content` | `R/fnt_burstiness.R` | Proportion of content-word tokens with β < 0.6. | document |
| `mean_adaptation_content` | `R/fnt_burstiness.R` | Mean adaptation ratio restricted to content words. | document |

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

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `n_connectives` | `R/fnt_mwe.R` | Total number of connective matches. | document |
| `<relation_group>` | `R/fnt_mwe.R` | Count of matches per relation group in the lexicon (e.g. `contrast`, `goal`, `cause`). | document |
| `cat_<cat>` | `R/fnt_mwe.R` | Count of matches per grammatical category in the lexicon (e.g. `cat_cco`, `cat_adv`). | document |
| `*_per100w` | `R/fnt_mwe.R` | Per-100-word density variant of every count column above. | document |

See `demos/demo_mwe_matching.R` for a worked example with a hand-built toy
lexicon on the Vikidia/Wikipedia and ALECTOR corpora.

## 20) `features$tunits`
Produced in: `R/fnt_tunits.R` (`tunit_features`)

Operationalized following Hunt (1965) and Lu (2010). T-unit boundaries are detected
from UD dependency parses: the sentence root anchors the first T-unit; each predicate
(VERB, AUX, or ADJ-with-cop) reachable from the root via a chain of `conj` arcs
starts an additional T-unit. Complex nominal detection reuses `add_complex_nominal_flag()`
from `fnt_extra_syntax.R`, ensuring identical operationalization across feature sets.

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `n_tunits` | `R/fnt_tunits.R` | Total T-units in the document. | document |
| `n_sentences` | `R/fnt_tunits.R` | Total orthographic sentences. | document |
| `mls` | `R/fnt_tunits.R` | Mean Length of Sentence in tokens, PUNCT excluded (Lu 2010 MLS). | document |
| `mlt` | `R/fnt_tunits.R` | Mean Length of T-unit in tokens, PUNCT excluded (Hunt 1965 / Lu 2010 MLT). | document |
| `mlc` | `R/fnt_tunits.R` | Mean Length of Clause in tokens, PUNCT excluded (Lu 2010 MLC). Denominator = T-units + dependent clauses. | document |
| `c_s` | `R/fnt_tunits.R` | Clauses per sentence (Lu 2010 C/S). | document |
| `t_s` | `R/fnt_tunits.R` | T-units per sentence — coordination index (Lu 2010 T/S; Bardovi-Harlig 1992). Values > 1 indicate sentences with multiple coordinated main clauses. | document |
| `c_t` | `R/fnt_tunits.R` | Clauses per T-unit (Hunt 1965 / Lu 2010 C/T). Always ≥ 1; increases with subordination depth. | document |
| `dc_c` | `R/fnt_tunits.R` | Dependent clauses per clause (Lu 2010 DC/C). Finite subordinate heads (`ccomp`, `advcl`, `acl`, `acl:relcl`) / total clauses. | document |
| `dc_t` | `R/fnt_tunits.R` | Dependent clauses per T-unit (Lu 2010 DC/T). Same DC definition; denominator is T-units. | document |
| `ct_t` | `R/fnt_tunits.R` | Complex T-unit ratio (Lu 2010 CT/T): proportion of T-units containing ≥ 1 dependent clause. | document |
| `vp_t` | `R/fnt_tunits.R` | Verb phrases per T-unit (Lu 2010 VP/T): total VERB + AUX tokens / n\_tunits. | document |
| `cp_t` | `R/fnt_tunits.R` | Coordinate phrases per T-unit (Lu 2010 CP/T): `conj` arcs whose head is NOUN, PROPN, ADJ, or ADV (phrasal coordination within a T-unit, not clausal). | document |
| `cp_c` | `R/fnt_tunits.R` | Coordinate phrases per clause (Lu 2010 CP/C). Same CP definition; denominator is total clauses. | document |
| `cn_t` | `R/fnt_tunits.R` | Complex nominals per T-unit (Lu 2010 CN/T): NOUN tokens with ≥ 1 substantive modifier child. Uses same relation set as `add_complex_nominal_flag()` in `fnt_extra_syntax.R`. | document |
| `cn_c` | `R/fnt_tunits.R` | Complex nominals per clause (Lu 2010 CN/C). Same CN definition; denominator is total clauses. | document |
| `prop_coord_sent` | `R/fnt_tunits.R` | Proportion of sentences containing more than one T-unit (ALSI addition). | document |

References: Hunt (1965) NCTE Research Report No. 3; Lu (2010) *IJCL* 15(4);
Bardovi-Harlig (1992) *TESOL Quarterly* 26(2).

## Maintenance notes
- If `n_sent_context` changes in `simple_lexical_cohesion`, overlap feature names change accordingly (`...prevK`).
- If lexical DB schemas change, prefixed lexical feature names change automatically.
- If embedding model changes, number of `dim*` features changes.
- UPOS pivot features (`count_<UPOS>`, `prop_<UPOS>`) depend on observed tags in the processed corpus.
