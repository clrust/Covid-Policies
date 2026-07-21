# Created by: CR
# Date: 7/16/26
# Script to compute embeddings for all press releases in the data

from pathlib import Path
import numpy as np
import pandas as pd
import sentence_transformers
import torch
import transformers
from sentence_transformers import SentenceTransformer

print("sentence-transformers:", sentence_transformers.__version__)
print("transformers:", transformers.__version__)
print("torch:", torch.__version__)
### Global Constants; can be changed ###

WORKING_DIRECTORY = Path(
    "/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies"
)
INPUT_PATH = WORKING_DIRECTORY / "Data/05_combine_all_states.csv"
OUTPUT_PATH = (
    WORKING_DIRECTORY
    / "Analysis/Testing/Results/qwen_embeddings.parquet"
)
MODEL_NAME = "Qwen/Qwen3-Embedding-0.6B"
PROMPT = None

#testing with first 100 releases
data = pd.read_csv(INPUT_PATH)

print(data.head(5))

# Keep a stable link to the row in the source CSV.
data.insert(0, "source_row", data.index)

data["embedding_text"] = data["Text"].fillna("").str.strip()

if data["embedding_text"].eq("").any():
    empty_rows = data.loc[data["embedding_text"].eq(""), "source_row"].tolist()
    raise ValueError(f"No text in source rows: {empty_rows}")

if torch.backends.mps.is_available():
    device = "mps"
elif torch.cuda.is_available():
    device = "cuda"
else:
    device = "cpu"

print("Using device:", device)

# The first run downloads the model; later runs use the Hugging Face cache.
model = SentenceTransformer(MODEL_NAME, device=device)

print("Maximum sequence length:", model.max_seq_length)
print("Embedding dimension:", model.get_sentence_embedding_dimension())
print("Model device:", model.device)

texts = data["embedding_text"].tolist()

# Check lengths with the model's tokenizer before encoding so truncation is visible.
data["token_count"] = [
    len(model.tokenizer.encode(text, add_special_tokens=True))
    for text in texts
]

too_long = data["token_count"] > model.max_seq_length

if too_long.any():
    raise ValueError(
        "At least one release exceeds the model context window:\n"
        + data.loc[too_long, ["source_row", "token_count"]].to_string(index=False))

# calculate embeddings
embeddings = model.encode(
    texts,
    batch_size=1, # can increase batch size to speed things up, but requires higher working memory
    show_progress_bar=True,
    normalize_embeddings=True,
    convert_to_numpy=True,
    prompt = PROMPT
)

print("Embedding matrix shape:", embeddings.shape)
print("Row norms:", np.linalg.norm(embeddings, axis=1))

# Save one numeric column per embedding dimension for straightforward use in R.
embedding_columns = [
    f"embedding_{i:04d}" for i in range(1, embeddings.shape[1] + 1)
]
embedding_data = pd.DataFrame(embeddings, columns=embedding_columns)
embedding_data.insert(0, "source_row", data["source_row"])
embedding_data.insert(1, "Title", data["Title"])

embedding_data.to_parquet(
    OUTPUT_PATH,
    engine="pyarrow",
    compression="zstd",
    index=False
)