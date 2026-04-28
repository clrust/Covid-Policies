# Created by: CR
# Date: 4/27/26

library(tidyverse)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

all_states <- read_csv("Testing/Results/06_burnham_all_states.csv")

# dates filled in
all_states_complete <- all_states %>%
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
  fill(everything(), .direction = "down")


