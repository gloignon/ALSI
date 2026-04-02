# ALSI: Analyseur Lexico-Syntaxique Intégré

ILSA: Integrated Lexico-Syntactic Analyzer

Produces classic readability features and more advanced psycholinguistic features, including POS surprisal and dependency-tree based syntactic features.

This is a complete re-write of the pipeline described Loignon (2021). Please cite the 2021 paper if you use ALSI/ILSA (see bibliography at the end of this page).

Included lexical frequency databases:

-   Manulex (Lété et al, 2004)

-   ÉQOL (Stanké et al, 2019)

-   flelex (François et al., 2014)

-   Quebec's Ministry of education vocabulary list. We also include the yet unpublished frequencies, scraped from the Franqus (USITO) website: <https://franqus.ca/liste_orthographique/outil_de_recherche/>

-   LexConn (Roze, Danlos & Muller, 2012) — a lexicon of French discourse connectives used for multi-word expression matching.

Please cite the relevant papers if you use the lexical databases included in the ALSI/ILSA tool.

## Many features - for real

ALSI extracts many types of features ([see the current list of features](https://github.com/gloignon/ALSI/blob/main/FEATURES.md)). You can easily output classic NLP-style features, POS-tag features, cohesion features, lexical frequency features, dependency parsing features, and now LLM-derived features such as surprisal, entropy and semantic embeddings. You can even use ALSI to "talk" with your locally-run LLM through [Ollama](https://ollama.com/) and run experiments in AI summarization, back translation, question and answer, or anything you can think of.

## Encoding support

`build_corpus()` reads UTF-8 by default, but also supports Latin-1 (`encoding = "latin1"`) and Windows-1252 (`encoding = "windows-1252"`), which are common in older French corpora. Use `encoding = "auto"` to let the function detect each file's encoding automatically — if a directory contains files with different encodings, they will all be read correctly and you will get a warning listing what was found. See `demos/demo_corpus_read.R` for examples.

## Parser/tagger

ALSI uses a Universal Dependency based model of the French language. The model was trained on the French-GSD treebank, slightly modified so that AUX tags refer only to actual auxiliary verb, as proposed by Duran et al. (2021). It will therefore produce what we consider to be a more sensible tagging and an appropriate use of the AUX tag, e.g.:
-    ALSI/ISLA custom model: "Le (DET) chat (NOUN) est (VERB) gris (ADJ). Il (PRON) est (AUX) parti (VERB)." The copula "est" is tagged as VERB. The auxiliary "est" in the second sentence is also correctly tagged as AUX.
-    UDPipe model: "Le (DET) chat (NOUN) est (AUX) gris (ADJ).  Il (PRON) est (AUX) parti (VERB)." Both "est" are tagged as AUX, which is confusing for languages that have actual auxiliary verbs.
  
# Bibliography

Duran, M., Pagano, A., Rassi, A., & Pardo, T. (2021). On auxiliary verb in Universal Dependencies: Untangling the issue and proposing a systematized annotation strategy. In N. Mazziotta & S. Mille (Eds.), Proceedings of the Sixth International Conference on Dependency Linguistics (Depling, SyntaxFest 2021) (pp. 10–21). Association for Computational Linguistics. <https://aclanthology.org/2021.depling-1.2/>

François, T., Gala, N., Watrin, P., & Fairon, C. (2014, May). FLELex: a graded lexical resource for French foreign learners. In International conference on Language Resources and Evaluation (LREC 2014).

Loignon, G. (2021). ILSA: an automated language complexity analysis tool for French. Mesure et évaluation en éducation, 44, 61-88. <https://doi.org/10.7202/1095682ar>

Roze, C., Danlos, L., & Muller, P. (2012). LEXCONN: A French lexicon of discourse connectives. Discours, 10. <https://doi.org/10.4000/discours.8645>

Lété, B., Sprenger-Charolles, L., & Colé, P. (2004). MANULEX: A grade-level lexical database from French elementary school readers. Behavior Research Methods, Instruments, & Computers, 36(1), 156-166.

Stanké, B., Mené, M. L., Rezzonico, S., Moreau, A., Dumais, C., Robidoux, J., ... & Royle, P. (2019). ÉQOL: Une nouvelle base de données québécoise du lexique scolaire du primaire comportant une échelle d’acquisition de l’orthographe lexicale. Corpus, (19).
