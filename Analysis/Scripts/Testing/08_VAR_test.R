library(vars)
library(tidyverse)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

final_data <- read_csv("Testing/Results/04_pipeline_development.csv")


# testing this on NY
ny <- final_data %>%
  filter(state == "NY")

gov_dist <- ts(ny$U_gov_dist,
          start = c(2021, 1), 
          frequency = 365)

health_dist <- ts(ny$U_health_dist,
                  start = c(2021, 1),
                  frequency = 365)

gov_health <- window(ts.union(gov_dist, health_dist), start = c(2021, 1))
# p is how much lag we want, p = 5 would give us terms for all of the last five days
v1 <- VAR(gov_health, p = 7)
