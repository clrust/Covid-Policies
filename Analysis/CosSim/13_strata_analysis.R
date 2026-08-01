# Created by: CR
# Date: 7/31/26
# States categorized into strata based on party control of executive/legislative, t tests and such

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

purple_state <- c("VA", "PA", "MN", "MI", "MA")
red_state <- c("TX", "GA", "FL")
blue_state <- c("CA", "CO", "IL", "NY")

data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  mutate(strata = case_when(
    State %in% purple_state ~ "purple",
    State %in% red_state ~ "red",
    State %in% blue_state ~ "blue",
    .default = NA
  )) %>%
  group_by(Date, strata) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

r_diff <- data[data$strata == "red",]$Glag1_U - data[data$strata == "red",]$Hlag1_U
b_diff <- data[data$strata == "blue",]$Glag1_U - data[data$strata == "blue",]$Hlag1_U

t.test(r_diff, b_diff)

data2 <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  mutate(strata = case_when(
    State %in% purple_state ~ "purple",
    State %in% red_state ~ "red",
    State %in% blue_state ~ "blue",
    .default = NA
  )) 

r_diff2 <- data2[data2$strata == "red",]$Glag1_U - data2[data2$strata == "red",]$Hlag1_U
b_diff2 <- data2[data2$strata == "blue",]$Glag1_U - data2[data2$strata == "blue",]$Hlag1_U
p_diff2 <- data2[data2$strata == "purple",]$Glag1_U - data2[data2$strata == "purple",]$Hlag1_U
t.test(r_diff2, b_diff2)
t.test(p_diff2, b_diff2)
# if anything, these statistically significant negative differences would suggest that it is the red states that listen to Health more
# but purple different?
# would be interested if different lag is different