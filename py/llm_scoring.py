import math
import os
from typing import List, Dict

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
):
    global _tokenizer, _model, _device, _mode, _model_name, _add_prefix_space
    _device = _best_device()
    tokenizer_kwargs = {
        "trust_remote_code": trust_remote_code,
        "use_fast": use_fast,
    }
    if add_prefix_space:
        tokenizer_kwargs["add_prefix_space"] = True
    try:
        _tokenizer = AutoTokenizer.from_pretrained(model_name, **tokenizer_kwargs)
    except TypeError:
        # Some tokenizers don't accept add_prefix_space
        tokenizer_kwargs.pop("add_prefix_space", None)
        _tokenizer = AutoTokenizer.from_pretrained(model_name, **tokenizer_kwargs)
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


def score_masked_lm_tokens(
    tokens: List[str],
    temperature: float = 1.0,
    batch_size: int = 0,
    context_text: str = None,
) -> Dict[str, List[float]]:
    global _tokenizer, _model, _device

    if _tokenizer is None or _model is None or _mode != "mlm":
        raise RuntimeError("Model not loaded in MLM mode. Call load_llm_model(mode='mlm') first.")

    if tokens is None or len(tokens) == 0:
        return {
            "word_surprisals": [],
            "word_entropies": [],
            "word_token_counts": [],
            "subword_surprisals": [],
            "subword_entropies": [],
            "subword_word_ids": [],
        }

    tokens = ["" if t is None else str(t) for t in tokens]
    if context_text is not None:
        context_text = str(context_text)

    encoding = _tokenizer(
        tokens,
        is_split_into_words=True,
        return_tensors="pt",
        add_special_tokens=True,
    )
    sentence_ids = encoding["input_ids"][0]

    if hasattr(encoding, "word_ids"):
        word_ids = encoding.word_ids(0)
    else:
        word_ids = None

    if word_ids is None:
        raise ValueError("Tokenizer does not provide word_ids; use a fast tokenizer.")

    context_ids = []
    if context_text is not None and str(context_text).strip():
        ctx = _tokenizer(context_text, add_special_tokens=False, return_tensors="pt")
        context_ids = ctx["input_ids"][0].tolist()

    input_ids = torch.tensor(context_ids + sentence_ids.tolist(), dtype=torch.long)
    context_len = len(context_ids)
    seq_len = input_ids.shape[0]

    # Build masked variants for all positions that map to a word
    positions = [context_len + i for i, wid in enumerate(word_ids) if wid is not None]

    if not positions:
        return {
            "word_surprisals": [float("nan")] * len(tokens),
            "word_entropies": [float("nan")] * len(tokens),
            "word_token_counts": [0] * len(tokens),
            "subword_surprisals": [float("nan")] * seq_len,
            "subword_entropies": [float("nan")] * seq_len,
            "subword_word_ids": word_ids,
        }

    mask_id = _tokenizer.mask_token_id
    if mask_id is None:
        raise ValueError("Model doesn't have a [MASK] token.")

    # Prepare batches
    total = len(positions)
    # reticulate passes R numerics as Python float; coerce to int for range()
    chunk = int(batch_size) if batch_size and batch_size > 0 else total

    subword_surprisals = [float("nan")] * seq_len
    subword_entropies = [float("nan")] * seq_len

    for start in range(0, total, chunk):
        end = min(start + chunk, total)
        batch_positions = positions[start:end]

        masked_batch = []
        for pos in batch_positions:
            masked = input_ids.clone()
            masked[pos] = mask_id
            masked_batch.append(masked)

        batch = torch.stack(masked_batch).to(_device)

        with torch.no_grad():
            outputs = _model(batch)
            logits_batch = outputs.logits  # (batch, seq_len, vocab)

        for local_idx, pos in enumerate(batch_positions):
            target_id = int(input_ids[pos].item())
            logits = logits_batch[local_idx, pos]
            surp, ent = _compute_surprisal_entropy(logits, target_id, temperature)
            subword_surprisals[pos] = surp
            subword_entropies[pos] = ent

    # Aggregate by word id
    n_words = len(tokens)
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

    # Convert entropies to mean per word
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
        "subword_surprisals": subword_surprisals,
        "subword_entropies": subword_entropies,
        "subword_word_ids": word_ids,
    }


def score_autoregressive_tokens(
    tokens: List[str],
    temperature: float = 1.0,
    context_text: str = None,
) -> Dict[str, List[float]]:
    global _tokenizer, _model, _device, _mode

    if _tokenizer is None or _model is None or _mode != "ar":
        raise RuntimeError("Model not loaded in AR mode. Call load_llm_model(mode='ar') first.")

    if tokens is None or len(tokens) == 0:
        return {
            "word_surprisals": [],
            "word_entropies": [],
            "word_token_counts": [],
            "subword_surprisals": [],
            "subword_entropies": [],
            "subword_word_ids": [],
        }

    tokens = ["" if t is None else str(t) for t in tokens]
    if context_text is not None:
        context_text = str(context_text)

    encoding = _tokenizer(
        tokens,
        is_split_into_words=True,
        return_tensors="pt",
        add_special_tokens=True,
    )
    sentence_ids = encoding["input_ids"][0]

    if hasattr(encoding, "word_ids"):
        word_ids = encoding.word_ids(0)
    else:
        word_ids = None

    if word_ids is None:
        raise ValueError("Tokenizer does not provide word_ids; use a fast tokenizer.")

    context_ids = []
    if context_text is not None and str(context_text).strip():
        ctx = _tokenizer(context_text, add_special_tokens=False, return_tensors="pt")
        context_ids = ctx["input_ids"][0].tolist()

    input_ids = torch.tensor(context_ids + sentence_ids.tolist(), dtype=torch.long)
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

    # Aggregate by word id
    n_words = len(tokens)
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
        "subword_surprisals": subword_surprisals,
        "subword_entropies": subword_entropies,
        "subword_word_ids": word_ids,
    }
