"""Versioned, model-specific tokenizer policies for CamemBERTv2."""

import hashlib
import json
import copy
from pathlib import Path

from transformers import PreTrainedTokenizerFast
import transformers
import tokenizers

POLICY_VERSION = "1"
CAMEMBERTV2_MODEL = "almanach/camembertv2-base"
SOURCE_REVISION = "42096e44334c6d256ef099c5d8936f9e272a938f"
SOURCE_CAP = 16
REPAIRED_CAP = 100


def load_tokenizer(model_name, policy="official", add_prefix_space=False):
    if policy not in ("official", "camembertv2_cap100_v1"):
        raise ValueError(f"Unknown tokenizer policy: {policy}")
    if policy == "camembertv2_cap100_v1" and model_name != CAMEMBERTV2_MODEL:
        raise ValueError(
            "camembertv2_cap100_v1 only supports " + CAMEMBERTV2_MODEL
        )
    if policy == "official":
        tokenizer = PreTrainedTokenizerFast.from_pretrained(
            model_name, add_prefix_space=add_prefix_space
        )
        raw = tokenizer_effective_bytes(tokenizer)
        return tokenizer, _metadata(policy, raw, None, None)

    tokenizer_path = _pinned_tokenizer_path()
    tokenizer_data = json.loads(tokenizer_path.read_text(encoding="utf-8"))
    tokenizer_data = repair_tokenizer_data(tokenizer_data, model_name)
    source_hash = hashlib.sha256(tokenizer_path.read_bytes()).hexdigest()
    serialized = json.dumps(tokenizer_data, ensure_ascii=False, separators=(",", ":"))
    tokenizer = PreTrainedTokenizerFast(
        tokenizer_object=_tokenizer_json(serialized),
        add_prefix_space=add_prefix_space,
        **_special_token_kwargs(tokenizer_data["model"]["vocab"]),
    )
    return tokenizer, _metadata(
        policy, tokenizer_effective_bytes(tokenizer), source_hash, SOURCE_CAP
    )


def repair_tokenizer_data(tokenizer_data, model_name):
    if model_name != CAMEMBERTV2_MODEL:
        raise ValueError("camembertv2_cap100_v1 only supports " + CAMEMBERTV2_MODEL)
    data = copy.deepcopy(tokenizer_data)
    model = data.get("model", {})
    if model.get("type") != "WordPiece" or model.get("max_input_chars_per_word") != SOURCE_CAP:
        raise ValueError("Pinned CamemBERTv2 tokenizer is not the verified WordPiece cap-16 source")
    vocab = model.get("vocab")
    if not isinstance(vocab, dict) or not vocab:
        raise ValueError("Pinned tokenizer has no WordPiece vocabulary")
    for special in data.get("added_tokens", []):
        if special.get("special") and vocab.get(special.get("content")) != special.get("id"):
            raise ValueError("Special-token IDs do not match the pinned vocabulary")
    data["model"]["max_input_chars_per_word"] = REPAIRED_CAP
    return data


def _tokenizer_json(serialized):
    from tokenizers import Tokenizer
    return Tokenizer.from_str(serialized)


def _pinned_tokenizer_path():
    from huggingface_hub import hf_hub_download
    return Path(hf_hub_download(
        CAMEMBERTV2_MODEL, "tokenizer.json", revision=SOURCE_REVISION
    ))


def _metadata(policy, effective, source_hash, source_cap):
    return {
        "tokenizer_policy": policy,
        "tokenizer_policy_version": POLICY_VERSION,
        "tokenizer_sha256": hashlib.sha256(effective).hexdigest(),
        "tokenizer_source_sha256": source_hash,
        "tokenizer_source_revision": SOURCE_REVISION if source_hash else None,
        "tokenizer_original_cap": source_cap,
        "tokenizer_effective_cap": REPAIRED_CAP if source_hash else None,
        "tokenizer_library_version": {
            "transformers": transformers.__version__,
            "tokenizers": tokenizers.__version__,
        },
    }


def tokenizer_hash(tokenizer):
    return hashlib.sha256(tokenizer_effective_bytes(tokenizer)).hexdigest()


def tokenizer_effective_bytes(tokenizer):
    special_map = {
        key: str(value) for key, value in tokenizer.special_tokens_map.items()
    }
    effective = json.dumps({
        "tokenizer": tokenizer.backend_tokenizer.to_str(),
        "special_tokens": special_map,
    }, sort_keys=True, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    return effective


def _special_token_kwargs(vocab):
    candidates = {
        "unk_token": ("[UNK]", "<unk>"),
        "pad_token": ("[PAD]", "<pad>"),
        "mask_token": ("[MASK]", "<mask>"),
        "cls_token": ("[CLS]", "<s>"),
        "sep_token": ("[SEP]", "</s>"),
        "bos_token": ("<s>", "[CLS]"),
        "eos_token": ("</s>", "[SEP]"),
    }
    result = {}
    for key, values in candidates.items():
        token = next((value for value in values if value in vocab), None)
        if token is not None:
            result[key] = token
    required = ("unk_token", "pad_token", "mask_token", "cls_token", "sep_token")
    if not all(key in result for key in required):
        raise ValueError("Pinned tokenizer is missing a required special token")
    return result
