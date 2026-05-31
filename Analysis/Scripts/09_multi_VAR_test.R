library(vars)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

final_data <- read_csv("Testing/Results/07_pipeline.csv")

# testing this on NY
ny <- final_data %>%
  filter(state == "NY")

ny_uni <- ny %>%
  select(ends_with("University"))

ny_gh <- ny %>%
  select(ends_with(c("Governor", "Health")))



Y <- as.matrix(ny_uni)
X <- as.matrix(ny_gh)

# oh wait of course my predictors are collinear...they are constrained to sum to 1
fit <- lm(Y ~ X)
