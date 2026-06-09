# VARs by topic but without CA because of CA University oddities
# Date: 6/9/26
# Created by: CR

library(vars)
library(fixest)
library(tidyverse)

# K different linear regression models, (one for each topic)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

# this data is filtered to the state level last first agency message date
state_max_min_data <- read_csv("Testing/Results/07_pipeline.csv") %>%
  filter(state != "CA") %>%
  filter(date >= max(maxmin))

# cleaned data starts on 05/18/2026
#------create data
data_lag <- state_max_min_data %>%
  arrange(date, state) %>%
  group_by(state) %>%
  mutate(
    across(
      ends_with(c("_Governor", "_Health", "_University")),
      ~ lag(.x, n = 1),
      .names = "{.col}_lag1"
    ) 
  ) %>%
  ungroup()


topics <- c(
  "housing",
  "other",
  "positive_cases",
  "economic_relief",
  "healthcare_infrastructure",
  "reopening",
  "jobs",
  "food",
  "research",
  "healthcare_professionals",
  "testing",
  "vaccines"
)

models <- lapply(topics, function(topic) {
  
  formula <- as.formula(
    paste0(
      topic, "_University ~ ",
      topic, "_Governor_lag1 + ",
      topic, "_Health_lag1 + ",
      topic, "_University_lag1 + ",
      "state"
    )
  )
  
  lm(formula, data = data_lag)
})

names(models) <- topics

#stars indicate highest level of statistical significance between lagged Governor and Health Department terms

summary(models$vaccines) #**
summary(models$housing)
summary(models$other)
summary(models$economic_relief)
summary(models$healthcare_infrastructure)  #*
summary(models$healthcare_professionals)
summary(models$reopening)
summary(models$jobs)
summary(models$food)
summary(models$research)
summary(models$testing)
summary(models$positive_cases) #**
