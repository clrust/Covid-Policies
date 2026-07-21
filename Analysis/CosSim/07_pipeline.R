# Created by: CR
# Date: 7/20/26
# Equivalent to 07_pipeline in scripts folder but for cosine similarity analysis
# Reads in parquet with each press release embedded, forward fills NA, calculates cosine similarity
# These embeddings are already normalized
library(arrow)
library(tidyverse)

# read in embeddings, df has dimensions: (number of press releases, number of embedding dimensions + 2)
embeddings <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings.parquet")

data <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Data/05_combine_all_states.csv")

# turns the 1024 embedding columns into one list column
embeddings2 <- embeddings %>%
  rowwise() %>%
  mutate(
    embedding = list(c_across(starts_with("embedding_")))
  ) %>%
  ungroup() %>%
  dplyr::select(source_row, Title, embedding)

# join the vector embeddings to the press release data
data2 <- data %>% left_join(embeddings2) 

data_test <- data2 %>%
  slice(1:3)

all_states_complete <- data_test %>%
  group_by(State, Agency, Date) %>% # if there are multiple releases on one day, take the vector mean
  summarise(
    embedding = list(matrix(unlist(embedding), ncol = 1024, byrow = TRUE) |>
                       colMeans()),
    .groups = "drop"
  ) %>%
  group_by(State, Agency) %>% # creating rows for days with no observations
  complete(Date = seq.Date(
    from = as.Date("2020-03-01"),
    to   = as.Date("2022-12-31"),
    by   = "day"
  )) %>%
  arrange(Date) %>%
  fill(everything(), .direction = "down") %>%
  drop_na() %>% 
  ungroup() %>%
  pivot_wider(names_from = Agency, values_from = embedding) %>%
  mutate(GU = map2_dbl(Governor, University, ~ as.numeric(.x %*% .y)),
         GH = map2_dbl(Governor, Health, ~ as.numeric(.x %*% .y)),
         HU = map2_dbl(Health, University, ~ as.numeric(.x %*% .y)))


toy_df <- all_states_complete %>%
  slice(1:6)

Agency <- rep(c("Governor", "University", "Health"), 2)
Date = c(ymd("2020-03-25", "2020-03-25", "2020-03-25", "2020-03-26", "2020-03-26", "2020-03-26"))
toy_df$Date <- Date
toy_df$Agency <- Agency

toy_df2 <- toy_df %>% 
  pivot_wider(names_from = Agency, values_from = embedding) %>%
  mutate(GU = map2_dbl(Governor, University, ~ as.numeric(.x %*% .y)),
         GH = map2_dbl(Governor, Health, ~ as.numeric(.x %*% .y)),
         HU = map2_dbl(Health, University, ~ as.numeric(.x %*% .y)))
