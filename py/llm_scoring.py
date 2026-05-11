import math
import os
from typing import List, Dict, Optional

import torch
from transformers import AutoTokenizer, AutoModelForMaskedLM, AutoModelForCausalLM

LN2 = math.log(2.0)

_tokenizer = None
_model = None
_device = None
_mode = None
_model_name = None
_add_prefix_space = None


def _best_device():
    if os.environ.get("ALSIO_FORCE_CPU") in ("1", "true", "TRUE", "yes", "YES"):
        return torch.device("cpu")
    if torch.cuda.is_available():
        return torch.device("cuda")
    if hasattr(torch.backends, "mps") and torch.backends.mps.is_available():
        return torch.device("mps")
    return torch.device("cpu")


def load_llm_model(
    model_name: str,
    mode: str = "mlm",
    use_fast: bool = True,
    trust_remote_code: bool = True,
    add_prefix_space: bool = False,
    force_fast: bool = False,
):
    """Load tokenizer + model.

    `force_fast=True` bypasses AutoTokenizer entirely and loads
    PreTrainedTokenizerFast directly from tokenizer.json. Use this when a
    repo's tokenizer_config.json points at a slow tokenizer class whose
    merges/vocab files are missing (e.g., almanach/camembertv2-base ships only
    tokenizer.json but declares tokenizer_class: RobertaTokenizer, which then
    falls through to character-level tokenization).

    `force_fast=False` (default) tries AutoTokenizer and auto-falls-back to
    PreTrainedTokenizerFast only if the loaded tokenizer breaks the French word
    "Cette" into more than 3 subwords (a clear sign of the BPE-merges-not-loaded
    failure mode).
    """
    global _tokenizer, _model, _device, _mode, _model_name, _add_prefix_space
    _device = _best_device()
    tokenizer_kwargs = {
        "trust_remote_code": trust_remote_code,
        "use_fast": use_fast,
    }
    if add_prefix_space:
        tokenizer_kwargs["add_prefix_space"] = True

    if force_fast:
        from transformers import PreTrainedTokenizerFast
        _tokenizer = PreTrainedTokenizerFast.from_pretrained(model_name)
    else:
        try:
            _tokenizer = AutoTokenizer.from_pretrained(model_name, **tokenizer_kwargs)
        except TypeError:
            tokenizer_kwargs.pop("add_prefix_space", None)
            _tokenizer = AutoTokenizer.from_pretrained(model_name, **tokenizer_kwargs)

        # Auto-fallback: detect character-level BPE failure and switch to
        # PreTrainedTokenizerFast (reads tokenizer.json directly).
        try:
            _probe_ids = _tokenizer("Cette", add_special_tokens=False)["input_ids"]
            if len(_probe_ids) > 3:
                from transformers import PreTrainedTokenizerFast
                _fast = PreTrainedTokenizerFast.from_pretrained(model_name)
                _probe_fast = _fast("Cette", add_special_tokens=False)["input_ids"]
                if len(_probe_fast) <= 3:
                    _tokenizer = _fast
        except Exception:
            pass

    if mode == "mlm":
        _model = AutoModelForMaskedLM.from_pretrained(
            model_name, trust_remote_code=trust_remote_code
        )
    elif mode == "ar":
        _model = AutoModelForCausalLM.from_pretrained(
            model_name, trust_remote_code=trust_remote_code
        )
    else:
        raise ValueError("mode must be one of: 'mlm', 'ar'")
    _mode = mode
    _model_name = model_name
    _add_prefix_space = bool(add_prefix_space)
    _model = _model.to(_device)
    _model.eval()
    print(f"Loaded {mode.upper()} model: {model_name} on {_device}")
    return True


def get_llm_state():
    return {"model_name": _model_name, "mode": _mode, "add_prefix_space": _add_prefix_space}


def _compute_surprisal_entropy(logits: torch.Tensor, target_id: int, temperature: float = 1.0):
    if temperature <= 0:
        raise ValueError("temperature must be > 0")
    scaled = logits / temperature
    probs = torch.softmax(scaled, dim=-1)
    log_probs = torch.log_softmax(scaled, dim=-1)
    surprisal = -log_probs[target_id].item() / LN2
    entropy = -(probs * log_probs).sum().item() / LN2
    return surprisal, entropy


def _get_word_ids(encoding) -> Optional[List]:
    """Try to get word_ids from encoding; return None if unavailable."""
    if hasattr(encoding, "word_ids"):
        try:
            return encoding.word_ids(0)
        except Exception:
            return None
    return None


def _aggregate_to_words(
    word_ids: List, n_words: int, context_len: int,
    subword_surprisals: List[float], subword_entropies: List[float],
) -> Dict[str, List]:
    """Aggregate subword scores to word level using word_ids mapping."""
    word_surps = [0.0] * n_words
    word_ents = [0.0] * n_words
    word_counts = [0] * n_words

    for pos, wid in enumerate(word_ids):
        if wid is None:
            continue
        if 0 <= wid < n_words:
            sub_pos = context_len + pos
            if not math.isnan(subword_surprisals[sub_pos]):
                word_surps[wid] += subword_surprisals[sub_pos]
            if not math.isnan(subword_entropies[sub_pos]):
                word_ents[wid] += subword_entropies[sub_pos]
            word_counts[wid] += 1

    # Entropy: mean per word; surprisal: sum per word
    for i in range(n_words):
        if word_counts[i] > 0:
            word_ents[i] = word_ents[i] / word_counts[i]
        else:
            word_surps[i] = float("nan")
            word_ents[i] = float("nan")

    return {
        "word_surprisals": word_surps,
        "word_entropies": word_ents,
        "word_token_counts": word_counts,
    }


def _empty_result() -> Dict:
    return {
        "word_surprisals": [],
        "word_entropies": [],
        "word_token_counts": [],
        "word_tokens": [],
        "subword_surprisals": [],
        "subword_entropies": [],
        "subword_word_ids": [],
        "subword_tokens": [],
    }


def _extract_word_tokens(
    is_split_into_words: bool,
    input_obj,
    word_ids,
    sentence_ids,
    n_words: int,
) -> List[str]:
    """Return one string per word. If pretokenized, that's the input list;
    otherwise reconstruct each word from its subword group via the tokenizer."""
    if is_split_into_words:
        return list(input_obj)
    if word_ids is None or n_words == 0:
        return []
    subword_tokens = _tokenizer.convert_ids_to_tokens(sentence_ids.tolist())
    groups: Dict[int, List[int]] = {}
    for pos, wid in enumerate(word_ids):
        if wid is None:
            continue
        groups.setdefault(wid, []).append(pos)
    out = [""] * n_words
    for wid, positions in groups.items():
        if 0 <= wid < n_words:
            subs = [subword_tokens[p] for p in positions]
            try:
                out[wid] = _tokenizer.convert_tokens_to_string(subs).strip()
            except Exception:
                out[wid] = "".join(subs)
    return out


def _build_input_ids(context_ids: list, sentence_ids) -> torch.Tensor:
    """Concatenate context and sentence ids, keeping any leading special token
    at position 0 (RoBERTa-family models expect BOS/<s> at position 0)."""
    if not context_ids:
        return sentence_ids
    sent_list = sentence_ids.tolist()
    has_leading_special = sent_list and sent_list[0] in _tokenizer.all_special_ids
    if has_leading_special:
        return torch.tensor(sent_list[:1] + context_ids + sent_list[1:], dtype=torch.long)
    return torch.tensor(context_ids + sent_list, dtype=torch.long)


def score_masked_lm_tokens(
    tokens,
    temperature: float = 1.0,
    batch_size: int = 0,
    context_text: str = None,
    is_split_into_words: bool = True,
    pll_mode: str = "original",
) -> Dict[str, List]:
    """Score a single sentence under an MLM.

    When `is_split_into_words=True` (default) `tokens` is a list of word
    strings and each element becomes one "word" in the per-word output.

    When `is_split_into_words=False` `tokens` is a raw sentence string and
    the LLM tokenizer's own pre-tokenizer decides where word boundaries fall;
    word indices come from `encoding.word_ids()`.

    `pll_mode`:
      - "original": mask one subword at a time (Salazar et al. 2020). Sibling
        subwords of a multi-subword word stay visible.
      - "within_word_l2r": when scoring subword k of a word, also mask all
        later subwords of that same word (Kauf & Ivanova 2023). Falls back to
        "original" when word_ids is unavailable.
    """
    global _tokenizer, _model, _device

    if _tokenizer is None or _model is None or _mode != "mlm":
        raise RuntimeError("Model not loaded in MLM mode. Call load_llm_model(mode='mlm') first.")

    if pll_mode not in ("original", "within_word_l2r"):
        raise ValueError("pll_mode must be one of: 'original', 'within_word_l2r'")

    if is_split_into_words:
        if tokens is None or len(tokens) == 0:
            return _empty_result()
        tokens = ["" if t is None else str(t) for t in tokens]
    else:
        if tokens is None or not str(tokens).strip():
            return _empty_result()
        tokens = str(tokens)

    if context_text is not None:
        context_text = str(context_text)

    encoding = _tokenizer(
        tokens,
        is_split_into_words=is_split_into_words,
        return_tensors="pt",
        add_special_tokens=True,
    )
    sentence_ids = encoding["input_ids"][0]
    word_ids = _get_word_ids(encoding)

    context_ids = []
    if context_text is not None and str(context_text).strip():
        ctx = _tokenizer(context_text, add_special_tokens=False, return_tensors="pt")
        context_ids = ctx["input_ids"][0].tolist()

    input_ids = _build_input_ids(context_ids, sentence_ids)
    context_len = len(context_ids)
    seq_len = input_ids.shape[0]

    if is_split_into_words:
        n_words = len(tokens)
    else:
        valid_wids = [w for w in (word_ids or []) if w is not None]
        n_words = (max(valid_wids) + 1) if valid_wids else 0

    # Determine which positions to mask
    if word_ids is not None:
        positions = [context_len + i for i, wid in enumerate(word_ids) if wid is not None]
    else:
        special_ids = set()
        for attr in ("cls_token_id", "sep_token_id", "pad_token_id", "bos_token_id", "eos_token_id"):
            tid = getattr(_tokenizer, attr, None)
            if tid is not None:
                special_ids.add(tid)
        positions = [
            context_len + i
            for i in range(len(sentence_ids))
            if int(sentence_ids[i].item()) not in special_ids
        ]

    if not positions:
        result = {
            "subword_surprisals": [float("nan")] * seq_len,
            "subword_entropies": [float("nan")] * seq_len,
            "subword_word_ids": word_ids if word_ids is not None else [],
            "subword_tokens": _tokenizer.convert_ids_to_tokens(sentence_ids.tolist()),
        }
        if word_ids is not None:
            result.update(_aggregate_to_words(word_ids, n_words, context_len,
                                              result["subword_surprisals"], result["subword_entropies"]))
        else:
            result.update({"word_surprisals": [], "word_entropies": [], "word_token_counts": []})
        result["word_tokens"] = _extract_word_tokens(
            is_split_into_words, tokens, word_ids, sentence_ids, n_words
        )
        return result

    mask_id = _tokenizer.mask_token_id
    if mask_id is None:
        raise ValueError("Model doesn't have a [MASK] token.")

    # Build (target_pos, extra_positions_to_also_mask) pairs.
    # For "within_word_l2r": when scoring subword k of a word, also mask later
    # subwords of that word so they can't leak the target's identity
    # (Kauf & Ivanova 2023). Falls back to "original" when word_ids unavailable.
    if pll_mode == "within_word_l2r" and word_ids is not None:
        word_groups: Dict[int, List[int]] = {}
        for sent_i, wid in enumerate(word_ids):
            if wid is None:
                continue
            word_groups.setdefault(wid, []).append(context_len + sent_i)
        variants: List = []
        for group in word_groups.values():
            for k, target_pos in enumerate(group):
                variants.append((target_pos, group[k + 1:]))
    else:
        variants = [(pos, []) for pos in positions]

    total = len(variants)
    # reticulate passes R numerics as Python float; coerce to int for range()
    chunk = int(batch_size) if batch_size and batch_size > 0 else total

    subword_surprisals = [float("nan")] * seq_len
    subword_entropies = [float("nan")] * seq_len

    for start in range(0, total, chunk):
        end = min(start + chunk, total)
        batch_variants = variants[start:end]

        masked_batch = []
        for target_pos, extra_positions in batch_variants:
            masked = input_ids.clone()
            masked[target_pos] = mask_id
            for extra in extra_positions:
                masked[extra] = mask_id
            masked_batch.append(masked)

        batch = torch.stack(masked_batch).to(_device)

        with torch.no_grad():
            outputs = _model(batch)
            logits_batch = outputs.logits  # (batch, seq_len, vocab)

        for local_idx, (target_pos, _extra) in enumerate(batch_variants):
            target_id = int(input_ids[target_pos].item())
            logits = logits_batch[local_idx, target_pos]
            surp, ent = _compute_surprisal_entropy(logits, target_id, temperature)
            subword_surprisals[target_pos] = surp
            subword_entropies[target_pos] = ent

    result = {
        "subword_surprisals": subword_surprisals,
        "subword_entropies": subword_entropies,
        "subword_word_ids": word_ids if word_ids is not None else [],
        "subword_tokens": _tokenizer.convert_ids_to_tokens(sentence_ids.tolist()),
    }

    if word_ids is not None:
        result.update(_aggregate_to_words(word_ids, n_words, context_len,
                                          subword_surprisals, subword_entropies))
    else:
        result.update({"word_surprisals": [], "word_entropies": [], "word_token_counts": []})
    result["word_tokens"] = _extract_word_tokens(
        is_split_into_words, tokens, word_ids, sentence_ids, n_words
    )

    return result


def score_autoregressive_tokens(
    tokens,
    temperature: float = 1.0,
    context_text: str = None,
    is_split_into_words: bool = True,
) -> Dict[str, List]:
    """AR counterpart of ``score_masked_lm_tokens``; see that function's
    docstring for the meaning of ``is_split_into_words``.
    """
    global _tokenizer, _model, _device, _mode

    if _tokenizer is None or _model is None or _mode != "ar":
        raise RuntimeError("Model not loaded in AR mode. Call load_llm_model(mode='ar') first.")

    if is_split_into_words:
        if tokens is None or len(tokens) == 0:
            return _empty_result()
        tokens = ["" if t is None else str(t) for t in tokens]
    else:
        if tokens is None or not str(tokens).strip():
            return _empty_result()
        tokens = str(tokens)

    if context_text is not None:
        context_text = str(context_text)

    encoding = _tokenizer(
        tokens,
        is_split_into_words=is_split_into_words,
        return_tensors="pt",
        add_special_tokens=True,
    )
    sentence_ids = encoding["input_ids"][0]
    word_ids = _get_word_ids(encoding)

    if is_split_into_words:
        n_words = len(tokens)
    else:
        valid_wids = [w for w in (word_ids or []) if w is not None]
        n_words = (max(valid_wids) + 1) if valid_wids else 0

    context_ids = []
    if context_text is not None and str(context_text).strip():
        ctx = _tokenizer(context_text, add_special_tokens=False, return_tensors="pt")
        context_ids = ctx["input_ids"][0].tolist()

    input_ids = _build_input_ids(context_ids, sentence_ids)
    context_len = len(context_ids)
    seq_len = input_ids.shape[0]
    input_tensor = input_ids.unsqueeze(0).to(_device)

    with torch.no_grad():
        outputs = _model(input_tensor)
        logits = outputs.logits[0]  # (seq_len, vocab)

    subword_surprisals = [float("nan")] * seq_len
    subword_entropies = [float("nan")] * seq_len

    for pos in range(seq_len):
        if pos < context_len:
            continue
        if pos == 0:
            # Use BOS context if available; otherwise skip
            if getattr(_tokenizer, "bos_token_id", None) is None:
                continue
            bos_id = int(_tokenizer.bos_token_id)
            # Minimal logits from BOS-only input
            with torch.no_grad():
                bos_logits = _model(torch.tensor([[bos_id]], device=_device)).logits[0, -1]
            target_id = int(input_ids[pos].item())
            surp, ent = _compute_surprisal_entropy(bos_logits, target_id, temperature)
        else:
            target_id = int(input_ids[pos].item())
            surp, ent = _compute_surprisal_entropy(logits[pos - 1], target_id, temperature)

        subword_surprisals[pos] = surp
        subword_entropies[pos] = ent

    result = {
        "subword_surprisals": subword_surprisals,
        "subword_entropies": subword_entropies,
        "subword_word_ids": word_ids if word_ids is not None else [],
        "subword_tokens": _tokenizer.convert_ids_to_tokens(sentence_ids.tolist()),
    }

    if word_ids is not None:
        result.update(_aggregate_to_words(word_ids, n_words, context_len,
                                          subword_surprisals, subword_entropies))
    else:
        result.update({"word_surprisals": [], "word_entropies": [], "word_token_counts": []})
    result["word_tokens"] = _extract_word_tokens(
        is_split_into_words, tokens, word_ids, sentence_ids, n_words
    )

    return result
