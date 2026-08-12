# Created by: CR
# Date: 7/20/26
# Equivalent to 07_pipeline in scripts folder but for cosine similarity analysis
# Reads in parquet with each press release embedded, forward fills NA, calculates cosine similarity
# These embeddings are already normalized
library(arrow)
library(tidyverse)
library(patchwork)

# read in embeddings, df has dimensions: (number of press releases, number of embedding dimensions + 2)
embeddings <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings.parquet") %>%
  filter(str_detect(Title, "covid|COVID|case"))


# read in data, add row number with 0 indexing
data <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Data/05_combine_all_states.csv") %>%
  mutate(source_row = row_number() - 1) %>%
  filter(str_detect(Title, "covid|COVID|case"))

# turns the 1024 embedding columns into one list column
embeddings2 <- embeddings %>%
  rowwise() %>%
  mutate(
    embedding = list(c_across(starts_with("embedding_")))
  ) %>%
  ungroup() %>%
  dplyr::select(source_row, Title, embedding)

# join the vector embeddings to the press release data
data2 <- data %>% left_join(embeddings2, 
                            join_by(Title == Title, source_row == source_row)) 

# Safe dot product function that handles null embeddings created by lag

safe_dot <- function(x, y) {
  if (is.null(x) || is.null(y) || length(x) == 0 || length(y) == 0) {
    return(NA_real_)
  }
  
  sum(x * y)
}

all_states_complete <- data2 %>%
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
  # adding flag to track whether a agency-day was filled
  mutate(no_release_flag =  if_else(
    map_lgl(embedding, is.null),
    "no_release",
    "release"
  )) %>%
  arrange(Date) %>%
  fill(everything(), .direction = "down") %>%
  drop_na() %>% 
  mutate(agency_min_date = min(Date)) %>% # state-agency min
  ungroup() %>%
  group_by(State) %>%
  mutate(state_max_min = max(agency_min_date)) %>% # state max-min
  ungroup() %>%
  filter(Date >= state_max_min) %>% # filter to state max-min
  pivot_wider(
    id_cols = c(State, Date),
    names_from = Agency, 
    values_from = c(embedding, no_release_flag)) %>%
  drop_na() %>% 
  # grouping by state so that lag gets the previous days embeddings, if they exist, within the state
  group_by(State) %>% 
  arrange(Date, .by_group = TRUE) %>%
  # adding 1 day lag for comparison between agencies across days
  mutate(lag1_Governor = lag(embedding_Governor),
         lag1_Health = lag(embedding_Health),
         lag1_University = lag(embedding_University)) %>%
  ungroup() %>%
  # calculating cosine similarity scores
  mutate(GU = map2_dbl(embedding_Governor, embedding_University, safe_dot),
         GH = map2_dbl(embedding_Governor, embedding_Health, safe_dot),
         HU = map2_dbl(embedding_Health, embedding_University, safe_dot)) %>%
  # cosine similarity scores for lagged agencies
  mutate(G_Ulag1 = map2_dbl(embedding_Governor, lag1_University, safe_dot),
         G_Hlag1 = map2_dbl(embedding_Governor, lag1_Health, safe_dot),
         H_Ulag1 = map2_dbl(embedding_Health, lag1_University, safe_dot),
         Glag1_U = map2_dbl(lag1_Governor, embedding_University, safe_dot),
         Glag1_H = map2_dbl(lag1_Governor, embedding_Health, safe_dot),
         Hlag1_U = map2_dbl(lag1_Health, embedding_University, safe_dot))

cossim_data <- all_states_complete %>%
  select(-c(embedding_Governor, embedding_Health, embedding_University, starts_with("lag1")))

write_csv(cossim_data, "~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/07_cossim_data_filtere.csv")







