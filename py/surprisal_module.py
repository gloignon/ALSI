'''
Surprisal computation module for the ALSI/ILSA pipeline.

Partner module for the surprisal_reticulate.R script in the R pipeline.

Supports three modes:
  1) "ar" (autoregressive): Computes surprisal using the model's autoregressive capabilities. Requires an ar model such as gpt2.
  2) "mlm" (masked language model): Computes surprisal using the model's masked language modeling capabilities.
  3) "pseudo_ar": A pseudo-autoregressive approach that uses masked tokens to compute surprisal on left context.

How to use:
1) Load the model with set_model(). Gpu (cuda) and cpu supported, will load by default 
if available. This function accepts: model_name, mode ("ar", "mlm" or "pseudo_ar"), 
and force_device (optional, gpu or cpu).

2) Call get_surprisal_from_token_ids() to compute surprisals. The parameters are 
left_ids, target_ids, right_ids, which are lists of token ids. left_ids and right_ids
are the left and right context, and target_ids are the target tokens for which we
want surprisal values. Note thate right_ids is only used in the "mlm" mode, which
is set when initializing the model with set_model().

Rudimentary batch processing available by calling the get_surprisals_batch() function instead of
get_surprisal_from_token_ids(). 

loignon.guillaume@uqam.ca
Last modif.: 2024-04-17.

'''
import torch
import torch.nn.functional as F
from transformers import AutoTokenizer, AutoModelForMaskedLM
from transformers import AutoModelForCausalLM

# Global variables 
_model = None
_tokenizer = None
_mode = None  # "ar", "mlm", "pseudo_ar"
_device = None  # Device for model (CPU or GPU)


def set_model(model_name, mode, force_device=None):
    global _model, _tokenizer, _mode, _device

    if mode in ("mlm", "pseudo_ar"):
      _tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=True, use_fast=False)
      _model = AutoModelForMaskedLM.from_pretrained(model_name, trust_remote_code=True)
    elif mode == "ar":
      _tokenizer = AutoTokenizer.from_pretrained(model_name)
      _model = AutoModelForCausalLM.from_pretrained(model_name)
    else:
      raise ValueError("Mode must be one of: 'ar', 'mlm', 'pseudo_ar'")

    _mode = mode
    
    if force_device == "cpu":
        _device = torch.device("cpu")
    elif torch.cuda.is_available():
        _device = torch.device("cuda")
    else:
        _device = torch.device("cpu")

    _model = _model.to(_device)
    _model.eval()

    print(f"Model '{model_name}' loaded with mode '{mode}' on {_device}.")

# Will return the (otherwise private) tokenizer object to R
def get_tokenizer():
    return _tokenizer


def get_surprisal_from_token_ids(left_ids, target_ids, right_ids):
    global _model, _tokenizer, _mode, _device

    # Convert inputs directly on the model's device
    left_ids = torch.tensor(left_ids, dtype=torch.long, device=_device).reshape(-1)
    target_ids = torch.tensor(target_ids, dtype=torch.long, device=_device).reshape(-1)
    right_ids = torch.tensor(right_ids, dtype=torch.long, device=_device).reshape(-1)

    input_ids = torch.cat([left_ids, target_ids, right_ids], dim=0).unsqueeze(0)

    target_start = len(left_ids)
    target_end = target_start + len(target_ids)

    if input_ids.shape[1] > getattr(_model.config, "max_position_embeddings", 512):
        print("Warning: input exceeds model max length. Consider chunking.")

    target_tokens = _tokenizer.convert_ids_to_tokens(target_ids.tolist())

    if _mode == "ar":
        return _surprisal_ar(input_ids, target_start, target_end, target_tokens)
    elif _mode == "mlm":
        return _surprisal_mlm(input_ids[0], target_ids, target_start, target_tokens)
    elif _mode == "pseudo_ar":
        return _surprisal_pseudo_ar(input_ids[0], target_ids, target_start, target_tokens)
    else:
        raise ValueError("Unknown mode")

def get_surprisals_batch(batch):
    global _model, _tokenizer, _mode, _device

    results = []

    for item in batch:
        left_ids = item.get("left_ids", [])
        target_ids = item.get("target_ids", [])
        right_ids = item.get("right_ids", [])

        # Convert inputs to tensors and move to model device
        left_ids = torch.tensor(left_ids, dtype=torch.long, device=_device).reshape(-1)
        target_ids = torch.tensor(target_ids, dtype=torch.long, device=_device).reshape(-1)
        right_ids = torch.tensor(right_ids, dtype=torch.long, device=_device).reshape(-1)

        input_ids = torch.cat([left_ids, target_ids, right_ids], dim=0).unsqueeze(0)

        target_start = len(left_ids)
        tokens = _tokenizer.convert_ids_to_tokens(target_ids.tolist())

        if _mode == "ar":
            output = _surprisal_ar(input_ids, target_start, target_start + len(target_ids), tokens)
        elif _mode == "mlm":
            output = _surprisal_mlm(input_ids[0], target_ids, target_start, tokens)
        elif _mode == "pseudo_ar":
            output = _surprisal_pseudo_ar(input_ids[0], target_ids, target_start, tokens)
        else:
            raise ValueError("Unknown mode")

        results.append(output)

    return results
  


# --- Mode-specific implementations ---

def _surprisal_ar(input_ids, target_start, target_end, tokens):
    global _model, _device

    input_ids = input_ids.to(_device)

    with torch.no_grad():
        logits = _model(input_ids).logits[:, :-1, :]
        log_probs = F.log_softmax(logits, dim=-1)

    ids = input_ids[:, 1:]  # shift for causal alignment
    token_ids = ids[0, target_start:target_end].to(dtype=torch.long, device=_device)
    probs = log_probs[0, target_start:target_end]

    surprisals = -probs.gather(1, token_ids.unsqueeze(1)).squeeze(1).tolist()
    return list(zip(tokens, surprisals))


def _surprisal_mlm(input_ids, target_ids, target_start, tokens):
    global _model, _tokenizer, _device

    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id
    surprisals = []

    for i in range(len(target_ids)):
        pos = target_start + i

        masked = input_ids.clone().to(_device)
        masked[pos] = mask_id
        masked[pos + 1:] = pad_id

        attention_mask = (masked != pad_id).long().unsqueeze(0).to(_device)

        with torch.no_grad():
            logits = _model(masked.unsqueeze(0), attention_mask=attention_mask).logits
            log_probs = F.log_softmax(logits[0, pos], dim=-1)
            token_id = int(target_ids[i].item())
            surprisals.append(-log_probs[token_id].item())

    return list(zip(tokens, surprisals))



def _surprisal_pseudo_ar(input_ids, target_ids, target_start, tokens):
    global _model, _tokenizer, _device

    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id
    surprisals = []

    input_ids = input_ids.to(_device)
    target_ids = target_ids.to(_device)

    for i in range(len(target_ids)):
        pos = target_start + i

        # Clone input and apply mask
        masked = input_ids.clone()
        masked[pos] = mask_id
        masked[pos + 1:] = pad_id

        # Prepare input and attention mask
        masked_input = masked.unsqueeze(0).to(_device)
        attention_mask = (masked_input != pad_id).long().to(_device)

        with torch.no_grad():
            logits = _model(masked_input, attention_mask=attention_mask).logits
            log_probs = F.log_softmax(logits[0, pos], dim=-1)
            token_id = int(target_ids[i].item())
            surprisals.append(-log_probs[token_id].item())

    return list(zip(tokens, surprisals))


