# ALSI: Analyseur Lexico-Syntaxique Intégré

ILSA: Integrated Lexico-Syntactic Analyzer

Produces classic readability features and more advanced psycholinguistic features, including POS surprisal and dependency-tree based syntactic features.

This is a complete re-write of the pipeline described Loignon (2021). Please cite the 2021 paper if you use ALSI/ILSA (see bibliography at the end of this page).

Included French lexical frequency databases:

-   Manulex (Lété et al, 2004)

-   ÉQOL (Stanké et al, 2019)

-   flelex (François et al., 2014)

-   Quebec's Ministry of education vocabulary list. We also include the yet unpublished frequencies, scraped from the Franqus (USITO) website: <https://franqus.ca/liste_orthographique/outil_de_recherche/>

-   LexConn (Roze, Danlos & Muller, 2012) — a lexicon of French discourse connectives used for multi-word expression matching.

Please cite the relevant papers if you use the lexical databases included in the ALSI/ILSA tool.

## Many features, for real

ALSI extracts many types of features ([see the full feature list](https://github.com/gloignon/ALSI/blob/main/FEATURES.md)):

- **Surface counts** — word, sentence, and character counts; POS-tag counts and proportions; verb tense and mood distributions.
- **Lexical frequency** — word-level frequency and grade-level lookup against four French databases (ÉQOL, Franqus, Manulex, FLELex), with Good-Turing imputation for out-of-vocabulary items.
- **Lexical diversity** — TTR, Maas, MATTR, and Simpson's D, computed on full vocabulary, content words, and verbs separately.
- **Dependency / syntactic complexity** — dependency depth, branching factor, head distance, Gibson (1998) DLT integration cost, and head-final/head-initial ratios.
- **Clausal complexity** — clausal density, mean clause length, complex nominals, and complex verbs, operationalized from Universal Dependency relations following Lu (2010); mean dependency distance following Liu (2008).
- **Lexical cohesion** — token and lemma overlap across sentence windows, argument overlap, and cosine similarity between adjacent sentences.
- **Semantic embeddings and coherence** — sentence and document embeddings; thematic dispersion, sequential similarity, topic drift, novelty, and conceptual convexity.
- **LLM surprisal** — token-level surprisal and entropy from masked (MLM) or autoregressive (AR) language models.
- **Word burstiness** — Weibull β scores (Altmann, Pierrehumbert & Motter, 2009) and negative-binomial adaptation scores (Church & Gale, 1995) measuring how clustered each word's occurrences are across documents.
- **Multi-word expression (MWE) matching** — density features for any user-supplied MWE lexicon, broken down by relation group and category. Demonstrated with LEXCONN (Roze, Danlos & Muller, 2012), a French discourse-connective lexicon.
- **Ollama LLM querying** — general-purpose row-by-row querying of a locally-run LLM (via [Ollama](https://ollama.com/)) for annotation, classification, paraphrase, or any templated task.

## Encoding support

`build_corpus()` reads UTF-8 by default, but also supports Latin-1 (`encoding = "latin1"`) and Windows-1252 (`encoding = "windows-1252"`). Use `encoding = "auto"` to let the function detect each file's encoding automatically — if a directory contains files with different encodings, they will all be read correctly and you will get a warning listing what was found. See `demos/demo_corpus_read.R` for examples.

## Parser/tagger

ALSI uses a Universal Dependency based model, with a custom model of the French language by default. Our French model was trained on the French-GSD treebank, slightly modified so that AUX tags refer only to actual auxiliary verb, as proposed by Duran et al. (2021). It will therefore produce what we consider to be a more sensible tagging and an appropriate use of the AUX tag, e.g.:
-    ALSI/ISLA custom model: "Le (DET) chat (NOUN) est (VERB) gris (ADJ). Il (PRON) est (AUX) parti (VERB)." The copula "est" is tagged as VERB. The auxiliary "est" in the second sentence is also correctly tagged as AUX.
-    UDPipe model: "Le (DET) chat (NOUN) est (AUX) gris (ADJ).  Il (PRON) est (AUX) parti (VERB)." Both "est" are tagged as AUX, which is confusing for languages that have actual auxiliary verbs.
  
# Bibliography

Altmann, E. G., Pierrehumbert, J. B., & Motter, A. E. (2009). Beyond word frequency: Bursts, lulls, and scaling in the temporal distributions of words. PLoS ONE, 4(11), e7678. <https://doi.org/10.1371/journal.pone.0007678>

Church, K. W., & Gale, W. A. (1995). Poisson mixtures. Natural Language Engineering, 1(2), 163–190. <https://doi.org/10.1017/S1351324900000139>

Gibson, E. (1998). Linguistic complexity: Locality of syntactic dependencies. Cognition, 68(1), 1–76. <https://doi.org/10.1016/S0010-0277(98)00034-1>

Duran, M., Pagano, A., Rassi, A., & Pardo, T. (2021). On auxiliary verb in Universal Dependencies: Untangling the issue and proposing a systematized annotation strategy. In N. Mazziotta & S. Mille (Eds.), Proceedings of the Sixth International Conference on Dependency Linguistics (Depling, SyntaxFest 2021) (pp. 10–21). Association for Computational Linguistics. <https://aclanthology.org/2021.depling-1.2/>

François, T., Gala, N., Watrin, P., & Fairon, C. (2014, May). FLELex: a graded lexical resource for French foreign learners. In International conference on Language Resources and Evaluation (LREC 2014).

Loignon, G. (2021). ILSA: an automated language complexity analysis tool for French. Mesure et évaluation en éducation, 44, 61-88. <https://doi.org/10.7202/1095682ar>

Roze, C., Danlos, L., & Muller, P. (2012). LEXCONN: A French lexicon of discourse connectives. Discours, 10. <https://doi.org/10.4000/discours.8645>

Lété, B., Sprenger-Charolles, L., & Colé, P. (2004). MANULEX: A grade-level lexical database from French elementary school readers. Behavior Research Methods, Instruments, & Computers, 36(1), 156-166.

Liu, H. (2008). Dependency distance as a metric of language comprehension difficulty. Journal of Cognitive Science, 9(2), 159–191. <https://doi.org/10.17791/jcs.2008.9.2.159>

Lu, X. (2010). Automatic analysis of syntactic complexity in second language writing. International Journal of Corpus Linguistics, 15(4), 474–496. <https://doi.org/10.1075/ijcl.15.4.02lu>

Stanké, B., Le Mené, M., Rezzonico, S., Moreau, A., Dumais, C., Robidoux, J., Dault, C., & Royle, P. (2019). ÉQOL: Une nouvelle base de données québécoise du lexique scolaire du primaire comportant une échelle d’acquisition de l’orthographe lexicale. Corpus, (19). <https://doi.org/10.4000/corpus.3818>
