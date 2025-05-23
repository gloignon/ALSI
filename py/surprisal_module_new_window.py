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


def set_model(
    model_name,
    mode,
    force_device=None,
    use_fast=False,
    trust_remote_code=True,
    use_sliding_window=False,
    window_size=50,
    stride=1,
    target_span=1
):
    global _model, _tokenizer, _mode, _device
    global _use_sliding_window, _window_size, _stride, _target_span

    if mode in ("mlm", "pseudo_ar"):
        _tokenizer = AutoTokenizer.from_pretrained(model_name, trust_remote_code=trust_remote_code, use_fast=use_fast)
        _model = AutoModelForMaskedLM.from_pretrained(model_name, trust_remote_code=trust_remote_code)
    elif mode == "ar":
        _tokenizer = AutoTokenizer.from_pretrained(model_name)
        _model = AutoModelForCausalLM.from_pretrained(model_name)
    else:
        raise ValueError("Mode must be one of: 'ar', 'mlm', 'pseudo_ar'")

    _mode = mode
    _use_sliding_window = use_sliding_window
    _window_size = window_size
    _stride = stride
    _target_span = target_span

    if force_device == "cpu":
        _device = torch.device("cpu")
    elif torch.cuda.is_available():
        _device = torch.device("cuda")
    else:
        _device = torch.device("cpu")

    _model = _model.to(_device)
    _model.eval()

    print(f"Model '{model_name}' loaded with mode '{mode}' on {_device}.")
    if _use_sliding_window:
        print(f"Sliding window mode activated: window_size={_window_size}, stride={_stride}, target_span={_target_span}")


# Will return the (otherwise private) tokenizer object to R
def get_tokenizer():
    return _tokenizer

"""
Returns:
    List[Tuple[str, float, float]]: Each tuple contains
        (token_text, surprisal, entropy)
"""
def get_surprisal_from_token_ids(left_ids, target_ids, right_ids):
    global _model, _tokenizer, _mode, _device
  
    # Convert inputs directly on the model's device
    left_ids = torch.tensor(left_ids, dtype=torch.long, device=_device).reshape(-1)
    target_ids = torch.tensor(target_ids, dtype=torch.long, device=_device).reshape(-1)
    right_ids = torch.tensor(right_ids, dtype=torch.long, device=_device).reshape(-1)

    # Early return if there's nothing to predict
    if target_ids.numel() == 0:
        return []
      
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
    global _mode, _use_sliding_window, _window_size, _stride, _target_span

    if _use_sliding_window:
        if not isinstance(batch, list) or not all(isinstance(x, int) for x in batch):
            raise ValueError("In sliding window mode, batch must be a flat list of token IDs.")
        return _sliding_surprisal_masked(
            token_ids=batch,
            window_size=_window_size,
            stride=_stride,
            target_span=_target_span,
            mode=_mode
        )

    batch_left_ids = [item.get("left_ids", []) for item in batch]
    batch_target_ids = [item.get("target_ids", []) for item in batch]
    batch_right_ids = [item.get("right_ids", []) for item in batch]

    if _mode == "ar":
        return _batch_surprisal_ar(batch_left_ids, batch_target_ids, batch_right_ids)
    elif _mode in ("mlm", "pseudo_ar"):
        return _batch_surprisal_masked(batch_left_ids, batch_target_ids, batch_right_ids, mode=_mode)
    else:
        raise ValueError(f"Unsupported mode: {_mode}")


# --- Mode-specific implementations ---

def _batch_surprisal_ar(batch_left_ids, batch_target_ids, batch_right_ids):
    """
    Batch implementation of autoregressive surprisal calculation
    """
    global _model, _tokenizer, _device
    
    batch_size = len(batch_left_ids)
    results = []
    
    # Create full sequences for the entire batch
    batch_input_ids = []
    batch_attention_masks = []
    target_positions = []
    
    # Process each item in the batch
    for i in range(batch_size):
        left_ids = batch_left_ids[i]
        target_ids = batch_target_ids[i]
        right_ids = batch_right_ids[i]
        
        # Combine all inputs
        full_sequence = left_ids + target_ids + right_ids
        
        # Record the start position of target tokens
        target_start = len(left_ids)
        target_end = target_start + len(target_ids)
        target_positions.append((target_start, target_end))
        
        # Add to batch
        batch_input_ids.append(torch.tensor(full_sequence, dtype=torch.long))
    
    # Pad sequences to the same length
    max_length = max(len(ids) for ids in batch_input_ids)
    padded_input_ids = []
    attention_masks = []
    
    for seq in batch_input_ids:
        padding_length = max_length - len(seq)
        padded_seq = torch.cat([seq, torch.tensor([_tokenizer.pad_token_id] * padding_length, dtype=torch.long)])
        attention_mask = torch.cat([torch.ones(len(seq), dtype=torch.long), 
                                    torch.zeros(padding_length, dtype=torch.long)])
        
        padded_input_ids.append(padded_seq)
        attention_masks.append(attention_mask)
    
    # Stack tensors
    input_tensor = torch.stack(padded_input_ids).to(_device)
    attention_tensor = torch.stack(attention_masks).to(_device)
    
    # Process batch through model
    with torch.no_grad():
        logits = _model(input_ids=input_tensor, attention_mask=attention_tensor).logits[:, :-1, :]
        log_probs = F.log_softmax(logits, dim=-1)
    
    # Get shifted token IDs for causal alignment
    shifted_ids = input_tensor[:, 1:]
    
    # Calculate surprisals for each item in the batch
    for i in range(batch_size):
        tokens = _tokenizer.convert_ids_to_tokens(batch_target_ids[i])
        target_start, target_end = target_positions[i]
        
        token_ids = shifted_ids[i, target_start:target_end].to(dtype=torch.long)
        probs = log_probs[i, target_start:target_end]
        surprisals = -probs.gather(1, token_ids.unsqueeze(1)).squeeze(1).tolist()
        
        results.append(list(zip(tokens, surprisals)))
    
    return results

def _batch_surprisal_mlm(batch_left_ids, batch_target_ids, batch_right_ids):
    global _model, _tokenizer, _device
    
    batch_size = len(batch_left_ids)
    results = [[] for _ in range(batch_size)]
    
    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id
    
    # Precompute basic info
    full_sequences = []
    target_positions = []
    original_tokens = []
    
    for i in range(batch_size):
        full_seq = batch_left_ids[i] + batch_target_ids[i] + batch_right_ids[i]
        target_start = len(batch_left_ids[i])
        target_end = target_start + len(batch_target_ids[i])
        
        full_sequences.append(full_seq)
        target_positions.append((target_start, target_end))
        original_tokens.append([_tokenizer.convert_ids_to_tokens([tid])[0] for tid in batch_target_ids[i]])
    
    # Process each target position across the entire batch
    max_targets = max(len(ids) for ids in batch_target_ids)
    
    for target_idx in range(max_targets):
        # Create a batch where each sentence has one token masked
        batch_masked = []
        batch_masks = []
        batch_token_ids = []
        active_indices = []
        
        for i in range(batch_size):
            target_start, target_end = target_positions[i]
            if target_start + target_idx < target_end:
                # Clone the sequence
                sequence = full_sequences[i].copy()
                # Mask the current target token
                sequence[target_start + target_idx] = mask_id
                
                # Create attention mask (1 for tokens, 0 for padding)
                attention_mask = [1 if tok != pad_id else 0 for tok in sequence]
                
                # No need for additional padding since we're masking everything after
                
                batch_masked.append(sequence)
                batch_masks.append(attention_mask)
                batch_token_ids.append(batch_target_ids[i][target_idx])
                active_indices.append(i)
        
        if not batch_masked:
            continue
        
        # Find the max length of the current batch
        max_length = max(len(seq) for seq in batch_masked)
        
        # Pad sequences to the same length
        for j in range(len(batch_masked)):
            seq_len = len(batch_masked[j])
            if seq_len < max_length:
                batch_masked[j] = batch_masked[j] + [pad_id] * (max_length - seq_len)
                batch_masks[j] = batch_masks[j] + [0] * (max_length - seq_len)
        
        # Convert to tensors
        input_tensor = torch.tensor(batch_masked, dtype=torch.long).to(_device)
        mask_tensor = torch.tensor(batch_masks, dtype=torch.long).to(_device)
        
        # Get model predictions
        with torch.no_grad():
            outputs = _model(input_ids=input_tensor, attention_mask=mask_tensor)
            logits = outputs.logits
        
        # Process results for each active sequence
        for j, batch_idx in enumerate(active_indices):
            target_start = target_positions[batch_idx][0]
            mask_pos = target_start + target_idx
            token_id = batch_token_ids[j]
        
            # Get log probabilities for the masked position
            log_probs = F.log_softmax(logits[j, mask_pos], dim=-1)
        
            # Surprisal
            surprisal = -log_probs[token_id].item()
        
            # Entropy = -Σ p(x) log p(x)
            probs = log_probs.exp()
            entropy = -(probs * log_probs).sum().item()
        
            # Get the token text
            token = original_tokens[batch_idx][target_idx]
        
            # Add to results
            results[batch_idx].append((token, surprisal, entropy))
    
    return results
  
def _batch_surprisal_pseudo_ar(batch_left_ids, batch_target_ids, batch_right_ids):
    """
    Batch implementation of pseudo-autoregressive surprisal calculation
    Similar to MLM but with different masking pattern
    """
    global _model, _tokenizer, _device
    
    batch_size = len(batch_left_ids)
    results = [[] for _ in range(batch_size)]
    
    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id
    
    # First, precompute basic info
    full_sequences = []
    target_positions = []
    
    for i in range(batch_size):
        # Ensure all inputs are lists
        left = batch_left_ids[i] if isinstance(batch_left_ids[i], list) else [batch_left_ids[i]]
        target = batch_target_ids[i] if isinstance(batch_target_ids[i], list) else [batch_target_ids[i]]
        right = batch_right_ids[i] if isinstance(batch_right_ids[i], list) else [batch_right_ids[i]]
        
        full_seq = left + target + right
        target_start = len(left)
        target_end = target_start + len(target)
        
        full_sequences.append(full_seq)
        target_positions.append((target_start, target_end))
    
    # Determine max sequence length for padding
    max_length = max(len(seq) for seq in full_sequences)
    
    # Process each target position across the entire batch
    max_targets = max(len(target_ids) if isinstance(target_ids, list) else 1 
                    for target_ids in batch_target_ids)
    
    for target_idx in range(max_targets):
        # Create a batch where each sentence has one token masked and all following tokens padded
        batch_masked = []
        batch_masks = []
        batch_token_ids = []
        active_indices = []  # Keep track of which batch items have a token at this position
        
        for i in range(batch_size):
            target_start, target_end = target_positions[i]
            target = batch_target_ids[i] if isinstance(batch_target_ids[i], list) else [batch_target_ids[i]]
            
            if target_start + target_idx < target_end:  # Check if this batch item has a token at this position
                # Clone the sequence
                sequence = full_sequences[i].copy()
                # Mask the current target token
                mask_pos = target_start + target_idx
                sequence[mask_pos] = mask_id
                # Set all tokens after the mask to padding
                for j in range(mask_pos + 1, len(sequence)):
                    sequence[j] = pad_id
                
                # Create attention mask (1 for tokens, 0 for padding)
                attention_mask = [1] * (mask_pos + 1) + [0] * (max_length - (mask_pos + 1))
                # Pad sequence to max length
                padded_seq = sequence[:mask_pos+1] + [pad_id] * (max_length - (mask_pos + 1))
                
                batch_masked.append(padded_seq)
                batch_masks.append(attention_mask)
                batch_token_ids.append(target[target_idx])
                active_indices.append(i)
        
        if not batch_masked:  # Skip if no sequences have a token at this position
            continue
        
        # Convert to tensors
        input_tensor = torch.tensor(batch_masked, dtype=torch.long).to(_device)
        mask_tensor = torch.tensor(batch_masks, dtype=torch.long).to(_device)
        
        # Get model predictions
        with torch.no_grad():
            outputs = _model(input_ids=input_tensor, attention_mask=mask_tensor)
            logits = outputs.logits
        
        # Process results for each active sequence
        for j, batch_idx in enumerate(active_indices):
            target_start = target_positions[batch_idx][0]
            mask_pos = target_start + target_idx
            token_id = batch_token_ids[j]
        
            # Get log probabilities for the masked position
            log_probs = F.log_softmax(logits[j, mask_pos], dim=-1)
        
            # Surprisal
            surprisal = -log_probs[token_id].item()
        
            # Entropy
            probs = log_probs.exp()
            entropy = -(probs * log_probs).sum().item()
        
            # Token text
            token = _tokenizer.convert_ids_to_tokens([token_id])[0]
        
            # Add to results
            results[batch_idx].append((token, surprisal, entropy))

    return results

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
    results = []

    for i in range(len(target_ids)):
        pos = target_start + i

        masked = input_ids.clone().to(_device)
        masked[pos] = mask_id

        # Keep full context, including right side
        attention_mask = torch.ones_like(masked).unsqueeze(0).to(_device)

        with torch.no_grad():
            logits = _model(masked.unsqueeze(0), attention_mask=attention_mask).logits
            log_probs = F.log_softmax(logits[0, pos], dim=-1)

            # Surprisal
            token_id = int(target_ids[i].item())
            surprisal = -log_probs[token_id].item()

            # Entropy
            probs = log_probs.exp()
            entropy = -(probs * log_probs).sum().item()

            results.append((tokens[i], surprisal, entropy))

    return results



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
            
            # Surprisal
            token_id = int(target_ids[i].item())
            surprisal = -log_probs[token_id].item()
        
            # Entropy = -Σ p(x) log p(x)
            probs = log_probs.exp()
            entropy = -(probs * log_probs).sum().item()
        
            # Append (token, surprisal, entropy)
            surprisals.append((tokens[i], surprisal, entropy))

    return surprisals
  
def _batch_surprisal_masked(batch_left_ids, batch_target_ids, batch_right_ids, mode="mlm"):
    global _model, _tokenizer, _device

    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id

    batch_size = len(batch_left_ids)
    results = [[] for _ in range(batch_size)]

    full_sequences = []
    target_positions = []
    original_tokens = []

    for i in range(batch_size):
        left = batch_left_ids[i]
        target = batch_target_ids[i]
        right = batch_right_ids[i]

        full_seq = left + target + right
        target_start = len(left)
        target_end = target_start + len(target)

        full_sequences.append(full_seq)
        target_positions.append((target_start, target_end))
        original_tokens.append(_tokenizer.convert_ids_to_tokens(target))

    max_targets = max(len(ids) for ids in batch_target_ids)

    for target_idx in range(max_targets):
        batch_masked = []
        batch_masks = []
        batch_token_ids = []
        active_indices = []

        for i in range(batch_size):
            target_start, target_end = target_positions[i]
            target = batch_target_ids[i]

            if target_start + target_idx < target_end:
                sequence = full_sequences[i].copy()
                mask_pos = target_start + target_idx
                sequence[mask_pos] = mask_id

                if mode == "pseudo_ar":
                    for j in range(mask_pos + 1, len(sequence)):
                        sequence[j] = pad_id
                    attention_mask = [1] * (mask_pos + 1) + [0] * (len(sequence) - (mask_pos + 1))
                else:  # MLM
                    attention_mask = [1] * len(sequence)

                batch_masked.append(sequence)
                batch_masks.append(attention_mask)
                batch_token_ids.append(target[target_idx])
                active_indices.append(i)

        if not batch_masked:
            continue

        max_length = max(len(seq) for seq in batch_masked)
        for j in range(len(batch_masked)):
            pad_len = max_length - len(batch_masked[j])
            batch_masked[j] += [pad_id] * pad_len
            batch_masks[j] += [0] * pad_len

        input_tensor = torch.tensor(batch_masked, dtype=torch.long).to(_device)
        mask_tensor = torch.tensor(batch_masks, dtype=torch.long).to(_device)

        with torch.no_grad():
            logits = _model(input_ids=input_tensor, attention_mask=mask_tensor).logits

        for j, batch_idx in enumerate(active_indices):
            target_start = target_positions[batch_idx][0]
            mask_pos = target_start + target_idx
            token_id = batch_token_ids[j]
            log_probs = F.log_softmax(logits[j, mask_pos], dim=-1)
            surprisal = -log_probs[token_id].item()  # surprisal = -log(p(x))
            probs = log_probs.exp()
            entropy = -(probs * log_probs).sum().item()
            token = original_tokens[batch_idx][target_idx]

            results[batch_idx].append((token, surprisal, entropy))

    return results


def _sliding_surprisal_masked(
    token_ids,
    window_size=50,
    stride=1,
    target_span=1,
    mode="mlm"
):
    global _model, _tokenizer, _device

    pad_id = _tokenizer.pad_token_id or _tokenizer.eos_token_id
    mask_id = _tokenizer.mask_token_id
    results = []

    length = len(token_ids)

    for start in range(0, length - window_size + 1, stride):
        window = token_ids[start : start + window_size]
        target_start = window_size - target_span
        target_ids = window[target_start:]

        for i, token_id in enumerate(target_ids):
            mask_pos = target_start + i
            masked = window.copy()
            masked[mask_pos] = mask_id

            if mode == "pseudo_ar":
                masked[mask_pos + 1:] = [pad_id] * (len(masked) - mask_pos - 1)
                attention_mask = [1] * (mask_pos + 1) + [0] * (len(masked) - mask_pos - 1)
            else:
                attention_mask = [1] * len(masked)

            input_tensor = torch.tensor([masked], dtype=torch.long, device=_device)
            mask_tensor = torch.tensor([attention_mask], dtype=torch.long, device=_device)

            with torch.no_grad():
                logits = _model(input_tensor, attention_mask=mask_tensor).logits[0, mask_pos]
                log_probs = F.log_softmax(logits, dim=-1)
                surprisal = -log_probs[token_id].item()
                probs = log_probs.exp()
                entropy = -(probs * log_probs).sum().item()
                token = _tokenizer.convert_ids_to_tokens([token_id])[0]

                results.append({
                    "position": start + mask_pos,
                    "token": token,
                    "surprisal": surprisal,
                    "entropy": entropy
                })

    return results
