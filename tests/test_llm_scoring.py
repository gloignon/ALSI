"""
Unit tests for py/llm_scoring.py

Run with the Python interpreter managed by reticulate (py_require handles
package installation). To find the path from R:
  reticulate::py_config()$python

Or run directly:
  /path/to/reticulate/python tests/test_llm_scoring.py

Validates MLM and AR surprisal against reference values computed
with direct transformers calls (verified correct on CPU).

Reference values (bert-base-uncased MLM, "The cat eats the fish"):
  The:     0.0399
  cat:     6.9309
  eats:    7.0374
  the:     1.9208
  fish:   14.6581

Reference values (pagnol-small AR, "Le chat mange le poisson"):
  Le:      first token (convention-dependent)
  chat:   12.2677
  mange:   9.3294
  le:      4.0631
  poisson: 6.5276
"""

import sys
import math
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "py"))
import llm_scoring


# ── Helpers ──────────────────────────────────────────────────────────

def assert_close(actual, expected, tol, label=""):
    diff = abs(actual - expected)
    assert diff <= tol, (
        f"{label}: expected {expected:.4f}, got {actual:.4f}, diff={diff:.4f} > tol={tol}"
    )


# ── MLM Tests ────────────────────────────────────────────────────────

def test_mlm_surprisal():
    """Test MLM surprisal with bert-base-uncased on English sentence."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    tokens = ["The", "cat", "eats", "the", "fish"]
    result = llm_scoring.score_masked_lm_tokens(tokens, temperature=1.0, batch_size=0)

    expected = [0.0399, 6.9309, 7.0374, 1.9208, 14.6581]

    assert len(result["word_surprisals"]) == len(tokens)
    assert len(result["word_entropies"]) == len(tokens)
    assert result["word_token_counts"] == [1, 1, 1, 1, 1]

    for i, tok in enumerate(tokens):
        assert_close(result["word_surprisals"][i], expected[i], tol=0.01, label=tok)
        assert result["word_entropies"][i] > 0, f"{tok}: entropy should be > 0"
        assert math.isfinite(result["word_entropies"][i]), f"{tok}: entropy should be finite"


def test_mlm_empty_input():
    """Empty input should return empty lists."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    result = llm_scoring.score_masked_lm_tokens([])
    assert result["word_surprisals"] == []
    assert result["word_entropies"] == []
    assert result["word_token_counts"] == []


def test_mlm_short_sentence():
    """Single-word sentence should work."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    result = llm_scoring.score_masked_lm_tokens(["Hello"])
    assert len(result["word_surprisals"]) == 1
    assert math.isfinite(result["word_surprisals"][0])
    assert result["word_surprisals"][0] > 0


def test_mlm_subword_alignment():
    """Words split into subwords should still produce one surprisal per word."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    # "methylenedioxy" will be split into multiple subwords by BERT
    tokens = ["This", "is", "methylenedioxy"]
    result = llm_scoring.score_masked_lm_tokens(tokens)

    assert len(result["word_surprisals"]) == 3
    assert len(result["word_entropies"]) == 3
    assert result["word_token_counts"][2] >= 2, (
        f"Expected 'methylenedioxy' to have >=2 subwords, got {result['word_token_counts'][2]}"
    )
    assert result["word_surprisals"][2] > 0
    assert math.isfinite(result["word_surprisals"][2])


def test_mlm_word_tokens_returned():
    """word_tokens and subword_tokens should be present in result."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    tokens = ["The", "cat", "eats"]
    result = llm_scoring.score_masked_lm_tokens(tokens)

    assert "word_tokens" in result, "word_tokens missing from result"
    assert "subword_tokens" in result, "subword_tokens missing from result"
    assert len(result["word_tokens"]) == 3
    assert result["word_tokens"] == tokens


def test_mlm_pll_within_word_l2r():
    """within_word_l2r mode should give different scores than original for multi-subword words."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    # "methylenedioxy" splits into multiple subwords with BERT (verified in test_mlm_subword_alignment)
    tokens = ["This", "is", "methylenedioxy"]

    result_orig = llm_scoring.score_masked_lm_tokens(tokens, pll_mode="original")
    result_l2r  = llm_scoring.score_masked_lm_tokens(tokens, pll_mode="within_word_l2r")

    assert len(result_orig["word_surprisals"]) == 3
    assert len(result_l2r["word_surprisals"]) == 3

    # Single-subword words should be identical across modes
    assert_close(result_orig["word_surprisals"][0], result_l2r["word_surprisals"][0],
                 tol=1e-6, label="This (single subword)")
    assert_close(result_orig["word_surprisals"][1], result_l2r["word_surprisals"][1],
                 tol=1e-6, label="is (single subword)")

    # Multi-subword word should differ between modes
    assert result_orig["word_token_counts"][2] >= 2, (
        "'methylenedioxy' should have >= 2 subwords"
    )
    assert result_orig["word_surprisals"][2] != result_l2r["word_surprisals"][2], (
        "within_word_l2r should give different surprisal for multi-subword words"
    )


def test_mlm_pll_invalid_mode():
    """Invalid pll_mode should raise ValueError."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    try:
        llm_scoring.score_masked_lm_tokens(["test"], pll_mode="invalid")
        assert False, "Should have raised ValueError"
    except ValueError:
        pass


def test_mlm_raw_sentence():
    """is_split_into_words=False should accept a raw string and return word-level scores."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    sentence = "The cat eats the fish"
    result = llm_scoring.score_masked_lm_tokens(sentence, is_split_into_words=False)

    # BERT tokenizer treats "The cat eats the fish" as 5 words
    assert len(result["word_surprisals"]) == 5
    assert len(result["word_entropies"]) == 5
    assert len(result["word_tokens"]) == 5

    for i, surp in enumerate(result["word_surprisals"]):
        assert math.isfinite(surp), f"word {i}: surprisal should be finite"
        assert surp >= 0, f"word {i}: surprisal should be >= 0"


def test_mlm_raw_empty_string():
    """Empty string with is_split_into_words=False should return empty result."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    result = llm_scoring.score_masked_lm_tokens("   ", is_split_into_words=False)
    assert result["word_surprisals"] == []
    assert result["word_tokens"] == []


def test_mlm_batching_consistency():
    """Batched and single-item scoring should give identical results."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    tokens = ["The", "cat", "eats", "the", "fish"]

    result_batch1 = llm_scoring.score_masked_lm_tokens(tokens, batch_size=1)
    result_batch0 = llm_scoring.score_masked_lm_tokens(tokens, batch_size=0)

    for i, tok in enumerate(tokens):
        assert_close(result_batch1["word_surprisals"][i],
                     result_batch0["word_surprisals"][i],
                     tol=1e-4, label=f"{tok} surprisal")
        assert_close(result_batch1["word_entropies"][i],
                     result_batch0["word_entropies"][i],
                     tol=1e-4, label=f"{tok} entropy")


# ── Entropy-mode aggregation (pure, model-free) ──────────────────────
#
# _aggregate_to_words is the single place where a multi-subword word's
# entropy is reduced to one number. It takes per-subword entropies plus a
# word_ids mapping, so its onset/offset/mean/sum contract can be verified
# exactly against hand-set subword scores — no model, fully deterministic.
#
# Fixture: three words, the middle one split into THREE subtokens. Three
# is the minimum needed to separate all four modes at once (with two
# subtokens, mean and offset can coincide and sum/mean ordering is
# ambiguous). The middle word's subtoken entropies are 2.0, 8.0, 4.0, so
#   onset  = 2.0   (first subtoken)
#   offset = 4.0   (last subtoken)
#   mean   = (2 + 8 + 4) / 3 = 4.6666...
#   sum    = 2 + 8 + 4       = 14.0
# — four distinct values, so each mode is pinned uniquely.

# word_ids: subword position -> word index. Word 1 owns positions 1,2,3.
_MODE_WORD_IDS = [0, 1, 1, 1, 2]
_MODE_N_WORDS = 3
# Per-subword entropies aligned to _MODE_WORD_IDS (context_len == 0).
_MODE_SUB_ENTS = [5.0, 2.0, 8.0, 4.0, 9.0]
# Surprisals are always summed regardless of entropy mode; distinct values
# let us confirm the surprisal sum for the multi-subword word too.
_MODE_SUB_SURPS = [1.0, 3.0, 6.0, 2.0, 7.0]


def _aggregate_modes(entropy, context_len=0, pad=0):
    """Run _aggregate_to_words on the fixture for one entropy mode.

    `pad` prepends `context_len` throwaway subword slots (all NaN) to check
    that the context_len offset indexes the subword arrays correctly.
    """
    surps = [float("nan")] * pad + _MODE_SUB_SURPS
    ents = [float("nan")] * pad + _MODE_SUB_ENTS
    return llm_scoring._aggregate_to_words(
        _MODE_WORD_IDS, _MODE_N_WORDS, context_len,
        surps, ents, entropy=entropy,
    )


def test_entropy_mode_onset():
    """onset -> entropy of the word's FIRST subtoken."""
    res = _aggregate_modes("onset")
    assert res["word_token_counts"] == [1, 3, 1]
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9, label="w0 onset")
    assert_close(res["word_entropies"][1], 2.0, tol=1e-9, label="w1 onset")
    assert_close(res["word_entropies"][2], 9.0, tol=1e-9, label="w2 onset")


def test_entropy_mode_offset():
    """offset -> entropy of the word's LAST subtoken."""
    res = _aggregate_modes("offset")
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9, label="w0 offset")
    assert_close(res["word_entropies"][1], 4.0, tol=1e-9, label="w1 offset")
    assert_close(res["word_entropies"][2], 9.0, tol=1e-9, label="w2 offset")


def test_entropy_mode_mean():
    """mean -> arithmetic mean of the word's subtoken entropies."""
    res = _aggregate_modes("mean")
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9, label="w0 mean")
    assert_close(res["word_entropies"][1], 14.0 / 3.0, tol=1e-9, label="w1 mean")
    assert_close(res["word_entropies"][2], 9.0, tol=1e-9, label="w2 mean")


def test_entropy_mode_sum():
    """sum -> sum of the word's subtoken entropies."""
    res = _aggregate_modes("sum")
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9, label="w0 sum")
    assert_close(res["word_entropies"][1], 14.0, tol=1e-9, label="w1 sum")
    assert_close(res["word_entropies"][2], 9.0, tol=1e-9, label="w2 sum")


def test_entropy_mode_none():
    """none -> every word entropy is NaN; surprisals still summed."""
    res = _aggregate_modes("none")
    for i in range(_MODE_N_WORDS):
        assert math.isnan(res["word_entropies"][i]), f"w{i} entropy should be NaN"
    # Surprisal of the 3-subtoken word is still its subtoken sum.
    assert_close(res["word_surprisals"][1], 3.0 + 6.0 + 2.0, tol=1e-9,
                 label="w1 surprisal (none)")


def test_entropy_modes_are_distinct():
    """The 3-subtoken word must get a different value under each mode."""
    vals = {m: _aggregate_modes(m)["word_entropies"][1]
            for m in ("onset", "offset", "mean", "sum")}
    assert len(set(vals.values())) == 4, (
        f"modes should be mutually distinguishable, got {vals}"
    )


def test_entropy_mode_single_subword_invariant():
    """Single-subword words get the same entropy under every mode."""
    for mode in ("onset", "offset", "mean", "sum"):
        res = _aggregate_modes(mode)
        assert_close(res["word_entropies"][0], 5.0, tol=1e-9,
                     label=f"w0 under {mode}")
        assert_close(res["word_entropies"][2], 9.0, tol=1e-9,
                     label=f"w2 under {mode}")


def test_entropy_surprisal_is_summed_all_modes():
    """Surprisal is the subtoken sum regardless of the entropy mode."""
    for mode in ("onset", "offset", "mean", "sum"):
        res = _aggregate_modes(mode)
        assert_close(res["word_surprisals"][1], 3.0 + 6.0 + 2.0, tol=1e-9,
                     label=f"w1 surprisal under {mode}")


def test_entropy_mode_context_len_offset():
    """A nonzero context_len must offset indexing into the subword arrays."""
    res = _aggregate_modes("sum", context_len=4, pad=4)
    assert_close(res["word_entropies"][1], 14.0, tol=1e-9,
                 label="w1 sum with context_len=4")
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9,
                 label="w0 onset with context_len=4")


def test_entropy_mode_unknown_raises():
    """An unrecognized mode is a hard error, never a silent fallback."""
    try:
        _aggregate_modes("bogus")
        assert False, "Should have raised ValueError for unknown mode"
    except ValueError:
        pass


def test_entropy_mode_nan_subword_poisons_word():
    """A word with any NaN subtoken is reported NaN, not a partial aggregate."""
    ents = list(_MODE_SUB_ENTS)
    ents[2] = float("nan")  # middle subtoken of word 1
    res = llm_scoring._aggregate_to_words(
        _MODE_WORD_IDS, _MODE_N_WORDS, 0,
        list(_MODE_SUB_SURPS), ents, entropy="sum",
    )
    assert math.isnan(res["word_entropies"][1]), "w1 should be NaN (bad subtoken)"
    assert math.isnan(res["word_surprisals"][1]), "w1 surprisal should be NaN too"
    # Clean words are unaffected.
    assert_close(res["word_entropies"][0], 5.0, tol=1e-9, label="w0 unaffected")


# ── Entropy-mode round-trip against exposed subword arrays ───────────
#
# The pure tests above pin the mode math. These verify the *plumbing*:
# that the per-subtoken arrays the backend exposes (subword_entropies +
# subword_word_ids) are the same ones the per-word aggregation consumed.
# We score a real word that splits into many subtokens, regroup its
# subtoken entropies from the result's own arrays, and check the per-word
# entropy equals the mode applied to those subtokens.

def _subword_entropies_for_word(result, target_wid):
    """Pull the per-subtoken entropies of one word from a scoring result,
    in subtoken order, using the result's own subword_word_ids mapping."""
    return [
        e for e, wid in zip(result["subword_entropies"],
                            result["subword_word_ids"])
        if wid == target_wid
    ]


def test_mlm_entropy_modes_roundtrip_multi_subword():
    """Per-word entropy must equal the mode applied to the word's own
    exposed subtoken entropies, for a word split into >=3 subtokens."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    # "antidisestablishmentarianism" splits into 8 BERT subtokens.
    tokens = ["The", "word", "antidisestablishmentarianism", "is", "long"]
    target = 2  # index of the multi-subword word

    modes = {
        "onset": lambda xs: xs[0],
        "offset": lambda xs: xs[-1],
        "mean": lambda xs: sum(xs) / len(xs),
        "sum": lambda xs: sum(xs),
    }

    seen = {}
    for mode, reduce_fn in modes.items():
        res = llm_scoring.score_masked_lm_tokens(tokens, entropy=mode)
        subs = _subword_entropies_for_word(res, target)
        assert len(subs) >= 3, (
            f"need >=3 subtokens to separate modes, got {len(subs)}"
        )
        expected = reduce_fn(subs)
        assert_close(res["word_entropies"][target], expected, tol=1e-5,
                     label=f"{mode} (word from its own subtokens)")
        seen[mode] = res["word_entropies"][target]

    # The four modes must land on four different values for this word,
    # otherwise the round-trip could pass by coincidence.
    assert len(set(round(v, 6) for v in seen.values())) == 4, (
        f"modes should differ for a multi-subword word, got {seen}"
    )


def test_mlm_single_subword_word_ignores_mode():
    """A word that is one subtoken has the same entropy under every mode."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model("google-bert/bert-base-uncased", mode="mlm")

    tokens = ["The", "cat", "eats"]  # all single-subtoken for BERT
    vals = [
        llm_scoring.score_masked_lm_tokens(tokens, entropy=m)["word_entropies"][1]
        for m in ("onset", "offset", "mean", "sum")
    ]
    for m, v in zip(("offset", "mean", "sum"), vals[1:]):
        assert_close(v, vals[0], tol=1e-6, label=f"cat under {m} vs onset")


# ── AR Tests ─────────────────────────────────────────────────────────

def test_ar_surprisal():
    """Test AR surprisal with pagnol-small on French sentence."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model(
        "lightonai/pagnol-small", mode="ar", add_prefix_space=True
    )

    tokens = ["Le", "chat", "mange", "le", "poisson"]
    result = llm_scoring.score_autoregressive_tokens(tokens, temperature=1.0)

    # Reference values (excluding first token — convention-dependent)
    expected_from_2nd = [12.2677, 9.3294, 4.0631, 6.5276]

    assert len(result["word_surprisals"]) == 5
    assert len(result["word_entropies"]) == 5

    for i, (tok, exp) in enumerate(
        zip(tokens[1:], expected_from_2nd), start=1
    ):
        assert_close(result["word_surprisals"][i], exp, tol=0.5, label=tok)

    for i, tok in enumerate(tokens):
        assert result["word_surprisals"][i] >= 0, f"{tok}: surprisal should be >= 0"
        assert math.isfinite(result["word_surprisals"][i]), f"{tok}: surprisal should be finite"


def test_ar_empty_input():
    """Empty input should return empty lists."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model(
        "lightonai/pagnol-small", mode="ar", add_prefix_space=True
    )

    result = llm_scoring.score_autoregressive_tokens([])
    assert result["word_surprisals"] == []
    assert result["word_entropies"] == []


def test_ar_entropy_positive():
    """Entropy should be positive for all non-first tokens."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model(
        "lightonai/pagnol-small", mode="ar", add_prefix_space=True
    )

    tokens = ["Elle", "parle", "bien"]
    result = llm_scoring.score_autoregressive_tokens(tokens)

    for i in range(1, len(tokens)):
        assert result["word_entropies"][i] > 0, (
            f"{tokens[i]}: entropy should be > 0, got {result['word_entropies'][i]}"
        )


def test_ar_word_tokens_returned():
    """word_tokens and subword_tokens should be present in AR result."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model(
        "lightonai/pagnol-small", mode="ar", add_prefix_space=True
    )

    tokens = ["Le", "chat", "mange"]
    result = llm_scoring.score_autoregressive_tokens(tokens)

    assert "word_tokens" in result, "word_tokens missing from AR result"
    assert "subword_tokens" in result, "subword_tokens missing from AR result"
    assert len(result["word_tokens"]) == 3


def test_ar_raw_sentence():
    """is_split_into_words=False should work for AR models."""
    os.environ["ALSIO_FORCE_CPU"] = "1"
    llm_scoring.load_llm_model(
        "lightonai/pagnol-small", mode="ar", add_prefix_space=True
    )

    sentence = "Le chat mange le poisson"
    result = llm_scoring.score_autoregressive_tokens(sentence, is_split_into_words=False)

    assert len(result["word_surprisals"]) == 5
    assert len(result["word_tokens"]) == 5

    for i, surp in enumerate(result["word_surprisals"]):
        assert math.isfinite(surp), f"word {i}: surprisal should be finite"
        assert surp >= 0, f"word {i}: surprisal should be >= 0"


# ── Run all tests ────────────────────────────────────────────────────

if __name__ == "__main__":
    tests = [
        test_mlm_surprisal,
        test_mlm_empty_input,
        test_mlm_short_sentence,
        test_mlm_subword_alignment,
        test_mlm_word_tokens_returned,
        test_mlm_pll_within_word_l2r,
        test_mlm_pll_invalid_mode,
        test_mlm_raw_sentence,
        test_mlm_raw_empty_string,
        test_mlm_batching_consistency,
        test_entropy_mode_onset,
        test_entropy_mode_offset,
        test_entropy_mode_mean,
        test_entropy_mode_sum,
        test_entropy_mode_none,
        test_entropy_modes_are_distinct,
        test_entropy_mode_single_subword_invariant,
        test_entropy_surprisal_is_summed_all_modes,
        test_entropy_mode_context_len_offset,
        test_entropy_mode_unknown_raises,
        test_entropy_mode_nan_subword_poisons_word,
        test_mlm_entropy_modes_roundtrip_multi_subword,
        test_mlm_single_subword_word_ignores_mode,
        test_ar_surprisal,
        test_ar_empty_input,
        test_ar_entropy_positive,
        test_ar_word_tokens_returned,
        test_ar_raw_sentence,
    ]
    passed = 0
    failed = 0
    for t in tests:
        try:
            t()
            print(f"  PASS  {t.__name__}")
            passed += 1
        except Exception as e:
            print(f"  FAIL  {t.__name__}: {e}")
            failed += 1

    print(f"\n{passed} passed, {failed} failed")
    sys.exit(1 if failed else 0)
