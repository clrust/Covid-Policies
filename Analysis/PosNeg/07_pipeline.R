# Created by: CR
# Date: 4/27/26

library(tidyverse)
library(janitor)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

EPSILON <- 0.1

data <- read_csv("Testing/Results/06_burnham_posneg_all_states.csv") %>%
  clean_names() %>%
  select(-x1)

data_ent <- data %>%
  select(ends_with("_ent")) %>%
  select(sort(names(.)))

data_dis <- data %>%
  select(ends_with("_dis")) %>%
  select(sort(names(.)))

data_no_topics <- data %>%
  select(-c(ends_with("_ent"), ends_with("_dis")))

epsilon_df <- as.data.frame(matrix(EPSILON, nrow = nrow(data), ncol = 12))

# (Positive scores + epsilon/2) / (positive scores + negative scores + epsilon) 
data_proj = tibble((data_ent + epsilon_df / 2) / (data_ent + data_dis + epsilon_df))

complete_data <- bind_cols(data_no_topics, data_proj) %>%
  mutate(row_max = do.call(pmax, select(., where(is.numeric))))


ggplot(complete_data) +
  geom_histogram(aes(x = row_max))

