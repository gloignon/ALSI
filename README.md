# ALSI: Analyseur Lexico-Syntaxique Intégré

ILSA: Integrated Lexico-Syntactic Analyzer

Produces classic readability features, more advanced psycholinguistic features, and LLM-based features such as surprisal, entropy and embeddings.

This is a complete re-write of the pipeline described Loignon (2021). Please cite the 2021 paper if you use ALSI/ILSA (see bibliography at the end of this page)as no other papers have been published yet presenting this toolkit.

[LTRC conference 2026 attendees? The presentation on surprisal is right here](
https://github.com/gloignon/ALSI/blob/main/Loignon%20LTRC2026%20Surprisal%20%20v2.pdf)

## Many features, for real

ALSI extracts many types of features ([see the full feature list](https://github.com/gloignon/ALSI/blob/main/FEATURES.md)):

- **Surface counts** — word, sentence, and character counts; POS-tag counts and proportions; verb tense and mood distributions.
- **Lexical frequency** — word-level frequency and grade-level lookup against four French databases (ÉQOL, Franqus, Manulex, FLELex), with Good-Turing imputation for out-of-vocabulary items.
- **Lexical diversity** — TTR, Maas, MATTR, and Simpson's D, computed on full vocabulary, content words, and verbs separately.
- **Dependency / syntactic complexity** — dependency depth, branching factor, head distance, Gibson (1998) DLT integration cost, and head-final/head-initial ratios.
- **Clausal complexity** — clausal density, mean clause length, complex nominals, and complex verbs, operationalized from Universal Dependency relations following Lu (2010); mean dependency distance following Liu (2008).
- **T-unit complexity** — the full Lu (2010) 14-measure battery (T-unit construct origin: Hunt, 1965; the T-units-per-sentence ratio is Hunt's 1966 main clause coordination index), plus an ALSI coordination-sentence proportion index. Note that t-unit features are adapted to dependency parsing, we don't do s-bar plots.
- **Lexical cohesion** — token and lemma overlap across sentence windows, argument overlap, and cosine similarity between adjacent sentences.
- **Semantic embeddings and coherence** — sentence and document embeddings; thematic dispersion, sequential similarity, topic drift, novelty, and conceptual convexity.
- **POS surprisal** — token-, sentence-, and document-level surprisal and entropy from a UPOS trigram model. Includes Stupid Backoff (Brants et al. 2007) for unseen trigrams, optional sentence-boundary padding, and SD of surprisal as a Uniform Information Density proxy (Jaeger 2010). NEW: our custom llm trained on POS tags allows us to produce llm-surprisal and entropy for POS.
- **Dependency-triple surprisal** — syntactic predictability over dependency-tree arcs (head POS, relation, dependent POS), scoring −log₂ p(dependent_pos | head_pos, relation). A tree-local counterpart to POS surprisal, adapted from syntactic n-grams (Sidorov et al. 2014) with Stupid Backoff.
- **Dependency-attachment surprisal** — the same arc triples with the target flipped: −log₂ p(relation | head_pos, dependent_pos), measuring how ambiguous the attachment label is between two categories. Shares the model artefact with dependency-triple surprisal.
- **LLM surprisal** — token-level surprisal and entropy from masked (MLM) or autoregressive (AR) language models.
- **Word burstiness** — Weibull β scores (Altmann, Pierrehumbert & Motter, 2009) and negative-binomial adaptation scores (Church & Gale, 1995) measuring how clustered each word's occurrences are across documents.
- **Multi-word expression (MWE) matching** — density features for any user-supplied MWE lexicon, broken down by relation group and category.
- **Ollama LLM querying** — general-purpose row-by-row querying of a locally-run LLM (via [Ollama](https://ollama.com/)) for annotation, classification, paraphrase, or any templated task.
- **Lexical database features** (see below)

## Works with several French lexical frequency databases:

-   Manulex (Lété et al, 2004)

-   ÉQOL (Stanké et al, 2019)

-   flelex (François et al., 2014)

-   Quebec's Ministry of education vocabulary list. We also include the yet unpublished frequencies, scraped from the Franqus (USITO) website: <https://franqus.ca/liste_orthographique/outil_de_recherche/>

-   Lexique3 (New et al., 2004)

We do not redistribute these databases. Download your own copy with `alsi_setup_databases()`, which shows each license and asks for confirmation before fetching restricted resources (`alsi_list_databases()` shows what is present). You are responsible for respecting the respective user licenses — several are non-commercial — and for citing the relevant papers if you use them.

## Encoding support

`build_corpus()` reads UTF-8 by default, but also supports Latin-1 (`encoding = "latin1"`) and Windows-1252 (`encoding = "windows-1252"`). Use `encoding = "auto"` to let the function detect each file's encoding automatically — if a directory contains files with different encodings, they will all be read correctly and you will get a warning listing what was found. See `demos/demo_corpus_read.R` for examples.

## Parser/taggers

By default, ALSI uses a Universal Dependency based model, with a custom model of the French language. spaCy and trankit are also available. Our custom French models are trained on the French-GSD treebank, slightly modified so that AUX tags refer only to actual auxiliary verb, as proposed by Duran et al. (2021). They will therefore produce what we consider to be a more sensible tagging and an appropriate use of the AUX tag, e.g.:
-    ALSI/ISLA custom model: "Le (DET) chat (NOUN) est (VERB) gris (ADJ). Il (PRON) est (AUX) parti (VERB)." The copula "est" is tagged as VERB. The auxiliary "est" in the second sentence is also correctly tagged as AUX.
-    Official model: "Le (DET) chat (NOUN) est (AUX) gris (ADJ).  Il (PRON) est (AUX) parti (VERB)." Both "est" are tagged as AUX, which is confusing for languages that have actual auxiliary verbs.

Note that you can swap our models with the official ones or with your own. This enables to use the toolkit for English, for which most features will work natively.

## French model benchmarks

| Model | Sent | Tok | UPOS | Lemma | UAS | LAS |
|---|---:|---:|---:|---:|---:|---:|
| **ALSI Trankit v1** (XLM-RoBERTa) | 100 | 96.29 | 98.32 | 97.01 | 94.68 | 92.19 |
| **ALSI spaCy v1** (CNN tok2vec) | 99.64 | — | 95.64 | 93.99 | 90.52 | 85.94 |
| **ALSI french_gsd-remix_3** (UDPipe) | 95.27 | 98.83 | 96.31 | 97.21 | 87.80 | 84.75 |
| UDPipe 1 official (french-gsd-ud-2.5) | 93.59 | 98.71 | 95.45 | 93.68 | 86.56 | 83.22 |

All metrics are end-to-end from raw text on a slighlty corrected French-GSD UD 2.16 test set (sentences with clear lemmatization or sentence tokenization errors were removed from train, dev and test set). Test on your own data and judge for yourself. For official UDPipe benchmarks see: https://ufal.mff.cuni.cz/udpipe/1/models

# Bibliography

Altmann, E. G., Pierrehumbert, J. B., & Motter, A. E. (2009). Beyond word frequency: Bursts, lulls, and scaling in the temporal distributions of words. PLoS ONE, 4(11), e7678. <https://doi.org/10.1371/journal.pone.0007678>

Brants, T., Popat, A. C., Xu, P., Och, F. J., & Dean, J. (2007). Large language models in machine translation. *Proceedings of EMNLP-CoNLL*, 858–867.

Church, K. W., & Gale, W. A. (1995). Poisson mixtures. Natural Language Engineering, 1(2), 163–190. <https://doi.org/10.1017/S1351324900000139>

Duran, M., Pagano, A., Rassi, A., & Pardo, T. (2021). On auxiliary verb in Universal Dependencies: Untangling the issue and proposing a systematized annotation strategy. In N. Mazziotta & S. Mille (Eds.), Proceedings of the Sixth International Conference on Dependency Linguistics (Depling, SyntaxFest 2021) (pp. 10–21). Association for Computational Linguistics. <https://aclanthology.org/2021.depling-1.2/>

François, T., Gala, N., Watrin, P., & Fairon, C. (2014, May). FLELex: a graded lexical resource for French foreign learners. In International conference on Language Resources and Evaluation (LREC 2014).

Gibson, E. (1998). Linguistic complexity: Locality of syntactic dependencies. Cognition, 68(1), 1–76. <https://doi.org/10.1016/S0010-0277(98)00034-1>

Grave, E., Bojanowski, P., Gupta, P., Joulin, A., & Mikolov, T. (2018). Learning word vectors for 157 languages. In *Proceedings of the Eleventh International Conference on Language Resources and Evaluation (LREC 2018)*. European Language Resources Association. <https://aclanthology.org/L18-1550/>

Hunt, K. W. (1965). Grammatical structures written at three grade levels (NCTE Research Report No. 3). National Council of Teachers of English.

Hunt, K. W. (1966). Recent measures in syntactic development. Elementary English, 43(7), 732–739.

Jaeger, T. F. (2010). Redundancy and reduction: Speakers manage syntactic information density. *Cognitive Psychology*, 61(1), 23–62. <https://doi.org/10.1016/j.cogpsych.2010.02.002>

Lété, B., Sprenger-Charolles, L., & Colé, P. (2004). MANULEX: A grade-level lexical database from French elementary school readers. Behavior Research Methods, Instruments, & Computers, 36(1), 156-166.

Liu, H. (2008). Dependency distance as a metric of language comprehension difficulty. Journal of Cognitive Science, 9(2), 159–191. <https://doi.org/10.17791/jcs.2008.9.2.159>

Loignon, G. (2021). ILSA: an automated language complexity analysis tool for French. Mesure et évaluation en éducation, 44, 61-88. <https://doi.org/10.7202/1095682ar>

Lu, X. (2010). Automatic analysis of syntactic complexity in second language writing. International Journal of Corpus Linguistics, 15(4), 474–496. <https://doi.org/10.1075/ijcl.15.4.02lu>

New, B., Pallier, C., Brysbaert, M., & Ferrand, L. (2004). Lexique 2: A new French lexical database. Behavior Research Methods, Instruments, & Computers, 36(3), 516-524.

Sagot, B. (2010). The Lefff, a freely available and large-coverage morphological and syntactic lexicon for French. In *Proceedings of the Seventh International Conference on Language Resources and Evaluation (LREC 2010)*. European Language Resources Association.

Sidorov, G., Velasquez, F., Stamatatos, E., Gelbukh, A., & Chanona-Hernández, L. (2014). Syntactic n-grams as machine learning features for natural language processing. *Expert Systems with Applications*, 41(3), 853–860. <https://doi.org/10.1016/j.eswa.2013.08.015>

Stanké, B., Le Mené, M., Rezzonico, S., Moreau, A., Dumais, C., Robidoux, J., Dault, C., & Royle, P. (2019). ÉQOL: Une nouvelle base de données québécoise du lexique scolaire du primaire comportant une échelle d’acquisition de l’orthographe lexicale. Corpus, (19). <https://doi.org/10.4000/corpus.3818>
