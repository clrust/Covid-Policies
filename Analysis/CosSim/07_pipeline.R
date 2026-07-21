# Created by: CR
# Date: 7/20/26
# Equivalent to 07_pipeline in scripts folder but for cosine similarity analysis
# Reads in parquet with each press release embedded, forward fills NA, calculates cosine similarity
# These embeddings are already normalized
library(arrow)
library(tidyverse)
library(patchwork)

# read in embeddings, df has dimensions: (number of press releases, number of embedding dimensions + 2)
embeddings <- read_parquet("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/qwen_embeddings.parquet")

# read in data, add row number with 0 indexing
data <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Data/05_combine_all_states.csv") %>%
  mutate(source_row = row_number() - 1)

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
    values_from = embedding) %>%
  drop_na() %>% # Florida has no University
  mutate(GU = map2_dbl(Governor, University, ~ as.numeric(.x %*% .y)),
         GH = map2_dbl(Governor, Health, ~ as.numeric(.x %*% .y)),
         HU = map2_dbl(Health, University, ~ as.numeric(.x %*% .y)))

#---- Some plots
gu <- ggplot(all_states_complete) +
  geom_line(aes(y = GU, x = Date, color = State))

gh <- ggplot(all_states_complete) +
  geom_line(aes(y = GH, x = Date, color = State))

hu <- ggplot(all_states_complete) +
  geom_line(aes(y = HU, x = Date, color = State))

gu/gh/hu

#----NY
NY <- all_states_complete %>%
  filter(State == "NY")

ggplot(NY) +
  geom_line(aes(x = Date, y = GU, color = "Governor → University")) +
  geom_line(aes(x = Date, y = GH, color = "Governor → Health")) +
  geom_line(aes(x = Date, y = HU, color = "Health → University")) +
  scale_color_manual(
    name = "Series",
    values = c(
      "Governor → University" = "red",
      "Governor → Health" = "blue",
      "Health → University" = "green"
    )
  ) +
  labs(y = "Similarity Score", title = "New York")

#------
TX <- all_states_complete %>%
  filter(State == "TX")

ggplot(TX) +
  geom_line(aes(x = Date, y = GU, color = "Governor → University")) +
  geom_line(aes(x = Date, y = GH, color = "Governor → Health")) +
  geom_line(aes(x = Date, y = HU, color = "Health → University")) +
  scale_color_manual(
    name = "Series",
    values = c(
      "Governor → University" = "red",
      "Governor → Health" = "blue",
      "Health → University" = "green"
    )
  ) +
  labs(y = "Similarity Score", title = "Texas")
#-----
MA <- all_states_complete %>%
  filter(State == "MA")

ggplot(MA) +
  geom_line(aes(x = Date, y = GU, color = "Governor → University")) +
  geom_line(aes(x = Date, y = GH, color = "Governor → Health")) +
  geom_line(aes(x = Date, y = HU, color = "Health → University")) +
  scale_color_manual(
    name = "Series",
    values = c(
      "Governor → University" = "red",
      "Governor → Health" = "blue",
      "Health → University" = "green"
    )
  ) +
  labs(y = "Similarity Score", title = "MA")

#---Regression
lm(GU ~ HU -1, data = all_states_complete)






