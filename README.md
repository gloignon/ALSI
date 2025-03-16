---
editor_options: 
  markdown: 
    wrap: 72
---

# ALSI: Analyseur Lexico-Syntaxique Intégré

ILSA: Integrated Lexico-Syntactic Analyzer

Produces classic readability features and more advanced psycholinguistic
features, including POS surprisal and dependency-tree based syntactic
features.

This is a complete re-write of the pipeline described Loignon (2021).
Please cite the 2021 paper if you use ALSI/ILSA.

Included lexical frequency databases:

-   Manulex (Lété et al, 2004)

-   ÉQOL (Stanké et al, 2019)

-   flelex (François et al., 2014)

-   Quebec's Ministry of education vocabulary list. We also include the
    yet unpublished frequencies, scraped from the franqus (USITO)
    website:
    <https://franqus.ca/liste_orthographique/outil_de_recherche/>

Please cite the relevant papers if you use the lexical frequencies
included in the ALSI/ILSA tool.

Parser/tagger: ALSI uses a Universal Dependency based model of the
French language. The model was trained on the French-GSD treebank,
slightly modified so that AUX tags refer only to actual auxiliary verb,
as proposed by Duran et al. (2021). It will therefore produce what we
consider to be the proposer tagging for sentences such as Le (DET) chat
(NOUN) est (VERB) gris (ADJ), instead of tagging the copula verb as AUX,
which can be problematic for languages that have auxiliary verbs.

# Bibliography

Duran, M., Pagano, A., Rassi, A., & Pardo, T. (2021). On auxiliary verb
in Universal Dependencies: Untangling the issue and proposing a
systematized annotation strategy. In N. Mazziotta & S. Mille (Eds.),
Proceedings of the Sixth International Conference on Dependency
Linguistics (Depling, SyntaxFest 2021) (pp. 10–21). Association for
Computational Linguistics. <https://aclanthology.org/2021.depling-1.2/>

François, T., Gala, N., Watrin, P., & Fairon, C. (2014, May). FLELex: a
graded lexical resource for French foreign learners. In International
conference on Language Resources and Evaluation (LREC 2014).

Loignon, G. (2021). ILSA: an automated language complexity analysis tool
for French. Mesure et évaluation en éducation, 44, 61-88.
<https://doi.org/10.7202/1095682ar>

Lété, B., Sprenger-Charolles, L., & Colé, P. (2004). MANULEX: A
grade-level lexical database from French elementary school readers.
Behavior Research Methods, Instruments, & Computers, 36(1), 156-166.

Stanké, B., Mené, M. L., Rezzonico, S., Moreau, A., Dumais, C.,
Robidoux, J., ... & Royle, P. (2019). ÉQOL: Une nouvelle base de données
québécoise du lexique scolaire du primaire comportant une échelle
d’acquisition de l’orthographe lexicale. Corpus, (19). \## ALSI
