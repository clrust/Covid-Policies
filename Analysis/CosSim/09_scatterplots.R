# Created by: CR
# Date: 7/24/26
# Scatterplots comparing influence

library(tidyverse)
library(patchwork)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

cossim_data <- read_csv("Testing/Results/07_cossim_data.csv")


gh <- ggplot(cossim_data) +
  geom_point(aes(x = Glag1_H, y = G_Hlag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

gu <- ggplot(cossim_data) +
  geom_point(aes(x = Glag1_U, y = G_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

hu <- ggplot(cossim_data) +
  geom_point(aes(x = Hlag1_U, y = H_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") 

ggsave(filename = "Testing/Results/ScatterPlot/gh_scatter.png",
       plot = gh,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/ScatterPlot/hu_scatter.png",
       plot = hu,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


ggsave(filename = "Testing/Results/ScatterPlot/gu_scatter.png",
       plot = gu,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)




#---- Some plots
gu <- ggplot(all_states_complete) +
  geom_line(aes(y = GU, x = Date, color = State))

gh <- ggplot(all_states_complete) +
  geom_line(aes(y = GH, x = Date, color = State))

hu <- ggplot(all_states_complete) +
  geom_line(aes(y = HU, x = Date, color = State))

gu/gh/hu
