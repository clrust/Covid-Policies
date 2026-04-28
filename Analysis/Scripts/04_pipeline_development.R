# Created by: CR
# Date: 4/27/26

library(tidyverse)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

mn <- read_csv("Testing/Results/03_burnham_test.csv")

# dates filled in
mn_complete <- mn %>%
  complete(Date = seq.Date(
    from = as.Date("2020-03-01"),
    to   = as.Date("2022-12-31"),
    by   = "day"
  )) %>%
  arrange(Date) %>%
  fill(everything(), .direction = "down")


