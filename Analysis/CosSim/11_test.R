# Created by: CR
# Date: 7/24/26
# Seeing differences between higher and lower dimensional verions from different models

library(arrow)

fourb <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings_8B.parquet.4B")

# hmm so it seems he used 2560 dimensional embedding

eightb <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings_8B.parquet")
