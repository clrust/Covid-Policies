# Created by: CR
# Date: 8/5/26
# Plot mean across all states over time

library(tidyverse)
library(patchwork)
library(modelsummary)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

# different states entering at different times tho
data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  group_by(Date) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

