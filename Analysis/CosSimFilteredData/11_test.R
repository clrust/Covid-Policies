# Created by: CR
# Date: 7/24/26
# Seeing differences between higher and lower dimensional verions from different models

library(arrow)

fourb <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings_8B.parquet.4B")


normal <-  read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings.parquet")
# hmm so it seems he used 2560 dimensional embedding

eightb <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings_8B.parquet")

dataaaa <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Data/05_combine_all_states.csv")
source("~/covidpolicies/Analysis/CosSim/12_utils.R")

df <- clean_data(normal, dataaaa)
df1 <- clean_data(fourb, dataaaa)
df2 <- clean_data(eightb, dataaaa)

test <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/07_cossim_data.csv")

huh <- all.equal(df, test, tolerance = 1e-12,
                 check.attributes = FALSE)

#TRUE Yay


#testing differences
n_df <- df %>%
  select(where(is.numeric))

n_df1 <- df1 %>%
  select(where(is.numeric))

n_df2 <- df2 %>%
  select(where(is.numeric))


# avg diff 0.6B and 4B -> 0.03720827
mean(abs(as.matrix(n_df - n_df1)), na.rm = T)

# avg diff 4B and 8B -> 0.03378761
mean(abs(as.matrix(n_df1 - n_df2)), na.rm = T)

# avg diff 0.6B and 8B -> 0.03399011
mean(abs(as.matrix(n_df - n_df2)), na.rm = T)


# seems to be pretty minimal differences here

