# Created by: CR
# Date: 4/27/26
# Changing this so that it keeps data by using latest first press release from each state

library(tidyverse)
library(janitor)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

data <- read_csv("Testing/Results/06_burnham_all_states.csv")

# dates filled in
all_states_complete <- data %>%
  group_by(State, Agency, Date) %>% # if there are multiple releases on one day, average them out
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
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
  ungroup()

# finding most recent first press release date of any agency per state
thresholds <- all_states_complete %>% 
  group_by(Agency, State) %>%
  summarize(min = min(Date)) %>%
  ungroup() %>%
  group_by(State) %>%
  summarize(maxmin = max(min))


# last_first_release <- max(test$min) # currently CA Health, january 1, 2020

# filtering to first day we have releases from all agencies
all_states_complete2 <- all_states_complete %>%
  left_join(thresholds, join_by(State == State)) %>%
  filter(Date >= maxmin) %>%
  clean_names() %>%
  dplyr::select(-x1) %>%
  pivot_wider(names_from = agency,
              values_from = where(is.numeric),
              names_sep = "_")

# some states don't have all of health, university, and governor's office. 
# these are dropped when I filter out the NAs
final_data <- all_states_complete2 %>%
  ungroup() %>%
  drop_na()

# getting matrices to calculate Euclidean distance
final_data_gov <- final_data %>%
  dplyr::select(ends_with("Governor")) %>%
  as.matrix()

final_data_health <- final_data %>%
  dplyr::select(ends_with("Health")) %>%
  as.matrix()

final_data_university <- final_data %>%
  dplyr::select(ends_with("University")) %>%
  as.matrix()

#Calculating Euclidean distances
U_gov_dist = sqrt(rowSums((final_data_gov - final_data_university)^2))
U_health_dist = sqrt(rowSums((final_data_health - final_data_university)^2))

final_data$U_gov_dist <- U_gov_dist
final_data$U_health_dist <- U_health_dist

# writing cleaned data
write_csv(final_data, "Testing/Results/07_pipeline.csv")



