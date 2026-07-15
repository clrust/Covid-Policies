# VARs by topic
# Date: 6/1/26
# Created by: CR

library(vars)
library(fixest)
library(tidyverse)

# K different linear regression models, (one for each topic)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

# this data is already filtered to the last first agency message date
overall_max_min_data <- read_csv("Testing/Results/04_pipeline_development.csv")

#------create data
data_lag <- overall_max_min_data %>%
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


summary(models$vaccines) #**
# this is intuitive, ss positive coefficient for Health
summary(models$housing)
summary(models$other)
summary(models$economic_relief)
summary(models$healthcare_infrastructure)
summary(models$healthcare_professionals)
summary(models$reopening)
summary(models$jobs)
summary(models$food)
summary(models$research)
summary(models$testing) #*
# ss negative coefficients for both Health and Governor which doesn't make sense
summary(models$positive_cases) #**
# ss negative coefficient for Health which doesn't make any sense here


### jb 18 jun 2026
### use modelsummary::modelsummary() to print concise set of results

library(modelsummary)

### I don't know if there is a way to hack the list of topics except manually.
### This would be worth exploring

renamer <- function(old_names) {
  new_names <- gsub("vaccines_|housing_|other_|economic_relief_|healthcare_infrastructure_|healthcare_professionals_|reopening_|jobs_|food_|research_|testing_|positive_cases_", "", old_names)
  new_names <- gsub("_lag1", "[-1]", new_names)
  setNames(new_names, old_names)
}

modelsummary(models[1:6],
             coef_omit="Intercept|^state.*$",
             coef_rename=renamer,
             gof_omit="AIC|BIC|R2|R2 Adj.|Log.Lik.|F",
             output="lag1 by topic pt1.txt")

modelsummary(models[7:12],
             coef_omit="Intercept|^state.*$",
             coef_rename=renamer,
             gof_omit="AIC|BIC|R2|R2 Adj.|Log.Lik.|F",
             output="lag1 by topic pt2.txt")

#CO, GA, IL, MA, MI, MN, NY, PA, TX, VA



















