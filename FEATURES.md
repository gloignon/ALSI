# ALSI Feature Inventory

Scope: features produced by the current pipeline in `R/main.R`, based on non-`.old` scripts only.

Scripts covered:
- `R/fnt_counters.R`
- `R/fnt_lexical.R`
- `R/fnt_heights.R`
- `R/fnt_extra_syntax.R`
- `R/fnt_pos_surprisal.R`
- `R/fnt_cohesion.R`
- `R/fnt_embeddings.R`
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

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `avg_sent_height` | `R/fnt_heights.R` | Mean max dependency depth per sentence. | document |
| `avg_sent_height_adj` | `R/fnt_heights.R` | Mean sentence max depth normalized by sentence length. | document |
| `sd_sent_height` | `R/fnt_heights.R` | Standard deviation of sentence max depth. | document |
| `avg_dependency_depth_adj` | `R/fnt_heights.R` | Mean adjusted dependency depth over sentences. | document |
| `avg_sd_depth` | `R/fnt_heights.R` | Mean within-sentence SD of dependency depth. | document |
| `avg_branching_factor` | `R/fnt_heights.R` | Mean branching factor (dependents per internal node). | document |
| `n` | `R/fnt_heights.R` | Total counted sentence tokens (aggregation helper, retained). | document |
| `s` | `R/fnt_heights.R` | Number of sentences (aggregation helper, retained). | document |
| `total_paths` | `R/fnt_heights.R` | Sum of dependency path lengths (aggregation helper, retained). | document |
| `avg_dependency_depth` | `R/fnt_heights.R` | Document-level mean dependency depth from path sums. | document |
| `prop_hf` | `R/fnt_heights.R` | Proportion of head-final dependencies. | document |
| `prop_hi` | `R/fnt_heights.R` | Proportion of head-initial dependencies. | document |
| `avg_head_distance` | `R/fnt_heights.R` | Mean absolute dependency distance. | document |
| `avg_head_distance_adj` | `R/fnt_heights.R` | Mean dependency distance normalized by sentence length. | document |
| `max_head_distance` | `R/fnt_heights.R` | Maximum dependency distance. | document |
| `max_head_distance_adj` | `R/fnt_heights.R` | Maximum normalized dependency distance (`head_distance / (sentence_length - 1)`), bounded in `[0, 1]`. | document |
| `dependency_direction_index` | `R/fnt_heights.R` | Mean signed direction of dependencies (left vs right). | document |

Notes:
- `crossing_deps` (sentence level) and `avg_crossing_deps` (document level) were removed from `R/fnt_heights.R`.

## 8) `features$syntactic`
Produced in: `R/fnt_extra_syntax.R` (`extra_syntactic_features`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `avg_clause_length` | `R/fnt_extra_syntax.R` | Mean tokens per clause estimate. | document |
| `complex_nom_per_sent` | `R/fnt_extra_syntax.R` | Mean number of complex nominals per sentence. | document |
| `complex_verb_per_sent` | `R/fnt_extra_syntax.R` | Mean number of complex verbs per sentence. | document |
| `avg_dep_dist` | `R/fnt_extra_syntax.R` | Mean dependency distance. | document |
| `avg_dep_count` | `R/fnt_extra_syntax.R` | Mean dependents per token/head relation proxy. | document |
| `clausal_density` | `R/fnt_extra_syntax.R` | Clauses per sentence. | document |

## 9) `features$pos_surprisal$doc_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `mean_pos_surprisal` | `R/fnt_pos_surprisal.R` | Mean POS surprisal over filtered tokens. | document |
| `sd_pos_surprisal` | `R/fnt_pos_surprisal.R` | SD of POS surprisal. | document |
| `mean_pos_entropy` | `R/fnt_pos_surprisal.R` | Mean POS trigram-context entropy. | document |
| `sd_pos_entropy` | `R/fnt_pos_surprisal.R` | SD of POS entropy. | document |
| `mean_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | Mean token-to-token entropy change. | document |
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
| `sd_pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | SD entropy reduction at sentence level. | sentence |

## 11) `features$pos_surprisal$token_surprisal`
Produced in: `R/fnt_pos_surprisal.R` (`pos_surprisal`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `pos_surprisal` | `R/fnt_pos_surprisal.R` | Token-level POS surprisal (`-log2 p`). | word |
| `pos_entropy` | `R/fnt_pos_surprisal.R` | Token-level POS context entropy. | word |
| `pos_entropy_reduction` | `R/fnt_pos_surprisal.R` | Difference from previous token entropy in sentence. | word |

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
| `emb_convexity` | `R/fnt_embeddings.R` | Conceptual convexity (Gärdenfors): mean NN cosine similarity of points interpolated along all sentence-pair segments (λ = 0.25/0.5/0.75). 1 = perfectly convex. All pairs for n ≤ 30, random sample of 500 pairs above. | document |
| `emb_local_convexity` | `R/fnt_embeddings.R` | Same as convexity but only on consecutive sentence pairs; measures local semantic continuity. | document |

## 16) `features$surprisal$mlm` and `features$surprisal$ar`
Produced in: `R/fnt_surprisal.R` (`llm_surprisal_entropy`)

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `llm_surprisal` | `R/fnt_embeddings.R` | Token-level language-model surprisal (MLM or AR, depending on call). | word |
| `llm_entropy` | `R/fnt_embeddings.R` | Token-level predictive entropy from language model. | word |
| `llm_subword_n` | `R/fnt_embeddings.R` | Number of subword pieces used for the token. | word |

## 17) Optional label currently added in demo pipeline
Produced in: `R/main.R`

| Feature name | Script | Short description | Level |
|---|---|---|---|
| `class` | `R/main.R` | Demo label: `1` for `viki*` docs, `2` otherwise. | document |

## Maintenance notes
- If `n_sent_context` changes in `simple_lexical_cohesion`, overlap feature names change accordingly (`...prevK`).
- If lexical DB schemas change, prefixed lexical feature names change automatically.
- If embedding model changes, number of `dim*` features changes.
- UPOS pivot features (`count_<UPOS>`, `prop_<UPOS>`) depend on observed tags in the processed corpus.
