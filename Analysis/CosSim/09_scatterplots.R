# Created by: CR
# Date: 7/24/26
# Scatterplots comparing influence

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

cossim_data <- read_csv("Testing/Results/07_cossim_data.csv")


ggplot(cossim_data) +
  geom_point(aes(x = Glag1_H, y = G_Hlag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

ggplot(cossim_data) +
  geom_point(aes(x = Glag1_U, y = G_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

ggplot(cossim_data) +
  geom_point(aes(x = Hlag1_U, y = H_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

#---- Some plots
gu <- ggplot(all_states_complete) +
  geom_line(aes(y = GU, x = Date, color = State))

gh <- ggplot(all_states_complete) +
  geom_line(aes(y = GH, x = Date, color = State))

hu <- ggplot(all_states_complete) +
  geom_line(aes(y = HU, x = Date, color = State))

gu/gh/hu
