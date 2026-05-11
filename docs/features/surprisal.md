# Transformer Surprisal

Breadcrumbs: [Home](../index.md) > [Feature index](index.md) > [Transformer surprisal](surprisal.md)

Transformer surprisal and entropy features estimate token-level predictability using pretrained language models. ALSI supports both masked language models (MLM) and autoregressive (AR) models via a Python backend.

## Code

- `R/fnt_surprisal.R` — R interface functions
- `py/llm_scoring.py` — Python scoring backend
- `tests/test_llm_scoring.py` — unit tests

## R functions

### `load_llm_scorer()`

Loads a model into the Python session. The model is cached; subsequent calls to the scoring functions reuse it unless the model or mode has changed.

```r
load_llm_scorer(
  model_name        = "almanach/moderncamembert-base",
  mode              = "mlm",         # "mlm" or "ar"
  use_fast          = TRUE,
  trust_remote_code = TRUE,
  add_prefix_space  = NULL,
  force_fast        = FALSE
)
```

`force_fast = TRUE` bypasses AutoTokenizer and loads `PreTrainedTokenizerFast` directly from `tokenizer.json`. Use this for models whose `tokenizer_config.json` points at a slow tokenizer class whose vocab files are missing in the repo (e.g. `almanach/camembertv2-base`), which would otherwise fall through to character-level tokenization. When `force_fast = FALSE` (default), an automatic probe checks whether `"Cette"` is tokenized into more than 3 pieces and switches to `PreTrainedTokenizerFast` if so.

### `llm_surprisal_entropy()`

Main scoring function. Takes a parsed-corpus data.table (one row per token, with `doc_id`, `sentence_id`, and `token` columns) and returns the same table with three new columns appended.

```r
llm_surprisal_entropy(
  dt_corpus,
  model_name        = "almanach/moderncamembert-base",
  mode              = "mlm",
  context           = NULL,
  batch_size        = 0,
  temperature       = 1.0,
  use_fast          = TRUE,
  trust_remote_code = TRUE,
  add_prefix_space  = NULL,
  pll_mode          = "original"
)
```

**Output columns:**

| Column | Description |
|---|---|
| `llm_surprisal` | Token-level surprisal in bits (summed over subwords). |
| `llm_entropy` | Token-level predictive entropy in bits (mean over subwords). |
| `llm_subword_n` | Number of subword pieces the token was split into. |

### `llm_surprisal_entropy_sentences()`

Convenience wrapper that takes a data.table of whole sentences (with a `sentence` column) rather than pre-tokenized tokens. Tokenizes on whitespace internally, scores, and returns sentence-level mean surprisal and entropy.

```r
llm_surprisal_entropy_sentences(
  dt_sentences,
  model_name        = "almanach/moderncamembert-base",
  mode              = "mlm",
  context           = NULL,
  batch_size        = 0,
  temperature       = 1.0,
  use_fast          = TRUE,
  trust_remote_code = TRUE,
  add_prefix_space  = NULL,
  pll_mode          = "original"
)
```

Input must have columns: `doc_id`, `sentence_id`, `sentence`.

Output: same columns plus `llm_surprisal` and `llm_entropy` (sentence-level means).

### `llm_surprisal_entropy_raw()`

Like `llm_surprisal_entropy_sentences()`, but returns one row **per word** rather than per sentence. The sentence is passed as a raw string to the Python backend; the LLM tokenizer's own pre-tokenizer determines word boundaries (via `encoding.word_ids()`). This avoids any mismatch between R-side and LLM-side tokenization.

```r
llm_surprisal_entropy_raw(
  dt_sentences,
  model_name        = "almanach/moderncamembert-base",
  mode              = "mlm",
  context           = NULL,
  batch_size        = 0,
  temperature       = 1.0,
  use_fast          = TRUE,
  trust_remote_code = TRUE,
  add_prefix_space  = NULL,
  pll_mode          = "original"
)
```

Output columns: `doc_id`, `sentence_id`, `token_id`, `token`, `llm_surprisal`, `llm_entropy`, `llm_subword_n`.

## MLM scoring modes (`pll_mode`)

MLM surprisal is computed as pseudo-log-likelihood (PLL): each token is masked in turn and the log-probability of the original token is read from the model's output.

| `pll_mode` | Description |
|---|---|
| `"original"` | Mask one subword at a time; sibling subwords of the same word remain visible (Salazar et al., 2020). Default. |
| `"within_word_l2r"` | When scoring subword *k* of a word, also mask all later subwords of that word, so they cannot leak the identity of the target (Kauf & Ivanova, 2023). Single-subword words are unaffected. |

`pll_mode` is passed through to the Python backend and accepted by `llm_surprisal_entropy`, `llm_surprisal_entropy_sentences`, and `llm_surprisal_entropy_raw`. It is ignored for AR models.

## Context prepending

All scoring functions accept a `context` (R) / `context_text` (Python) string that is prepended to every sentence before scoring. This is useful for providing the preceding paragraph as a conditioning context.

Context tokens are inserted **after** any leading special token (BOS / `<s>` / `[CLS]`) so that RoBERTa-family models always see the BOS marker at position 0, as they were trained to expect.

## Model selection

Any HuggingFace MLM or causal LM can be used. Models tested with ALSI:

| Model | Mode | Notes |
|---|---|---|
| `almanach/moderncamembert-base` | MLM | Default; modern CamemBERT trained on recent French web data. |
| `almanach/camembertv2-base` | MLM | Use `force_fast = TRUE` to bypass tokenizer fallback issue. |
| `camembert-base` | MLM | Original CamemBERT. |
| `lightonai/pagnol-small` | AR | Small French GPT-2 style model; useful for testing. |

`add_prefix_space = TRUE` is generally needed for AR models that use a BPE tokenizer (e.g. GPT-2 style) to ensure correct tokenization of word-initial tokens.

## Python backend

The `py/llm_scoring.py` module is loaded via `reticulate::source_python()`. Dependencies are declared with `reticulate::py_require(c("torch", "transformers"))` so reticulate installs them automatically into its managed environment. No manual venv setup is required.

Device selection is automatic: CUDA → MPS (Apple Silicon) → CPU. Override with:

```r
Sys.setenv(ALSIO_FORCE_CPU = "1")
```

## Demo models

The current demo uses:

- `almanach/moderncamembert-base` in MLM mode
- `lightonai/pagnol-small` in AR mode

## References

Kauf, C., & Ivanova, A. A. (2023). A better way to do masked language model scoring. *arXiv*. <https://arxiv.org/abs/2305.10588>

Salazar, J., Liang, D., Nguyen, T. Q., & Kirchhoff, K. (2020). Masked language model scoring. In *Proceedings of ACL 2020*, 2699–2712. <https://aclanthology.org/2020.acl-main.240/>

---

Related pages:

- [POS surprisal](pos-surprisal.md)
- [Embeddings and LLMs](embeddings-and-llms.md)
- [Pipeline](../pipeline.md)
