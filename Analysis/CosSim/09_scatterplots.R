# Created by: CR
# Date: 7/24/26
# Scatterplots comparing influence

library(tidyverse)
library(patchwork)
library(tls)
setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

cossim_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") 

gh_data <- cossim_data %>% 
  filter(G_Hlag1 != Glag1_H)

# these are interesting statistics. Can see they are very close. 
# Could do t-tests on these metrics, split them up by strata etc.
mean(cossim_data$Glag1_H - cossim_data$G_Hlag1, na.rm = T)

mean(cossim_data$Glag1_U - cossim_data$G_Ulag1, na.rm = T)

mean(cossim_data$Hlag1_U - cossim_data$H_Ulag1, na.rm = T)

#nothing is statistically significant, let's stratify!
t.test(cossim_data$Glag1_U, cossim_data$G_Ulag1)
t.test(cossim_data$Glag1_H, cossim_data$G_Hlag1)
t.test(cossim_data$Hlag1_U, cossim_data$H_Ulag1)


# very similar coefficients, both less than 1
# story of who influences who is not clear from this metric
lm(G_Hlag1 ~ Glag1_H -1 , data = gh_data)
tls(G_Hlag1 ~ Glag1_H -1 , data = gh_data)

lm(Glag1_H ~ G_Hlag1 -1, data = gh_data)
tls(Glag1_H ~ G_Hlag1 -1 , data = gh_data)

gh <- ggplot(gh_data) +
  geom_point(aes(x = Glag1_H, y = G_Hlag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

ggplot(cossim_data) +
  geom_histogram(aes(x = Glag1_H - G_Hlag1))

ggplot(gh_data) +
  geom_histogram(aes(x = Glag1_H - G_Hlag1))

ggplot(gh_data) +
  geom_density(aes(x = Glag1_H - G_Hlag1))

gu_data <- cossim_data %>% 
  filter(G_Ulag1 != Glag1_U)


lm(G_Hlag1 ~ Glag1_H -1, data = gu_data)
tls(G_Hlag1 ~ Glag1_H -1 , data = gu_data)

gu <- ggplot(gu_data) +
  geom_point(aes(y = Glag1_U, x = G_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

ggplot(cossim_data) +
  geom_histogram(aes(x = Glag1_U - G_Ulag1))

ggplot(gu_data) +
  geom_histogram(aes(x = Glag1_U - G_Ulag1))

ggplot(gu_data) +
  geom_density(aes(x = Glag1_H - G_Hlag1))

# So minimal it is hard to conclude much
hu_data <- cossim_data %>% 
  filter(H_Ulag1 != Hlag1_U)

lm(H_Ulag1 ~ Hlag1_U -1, data = hu_data)
tls(H_Ulag1 ~ Hlag1_U -1 , data = hu_data)

hu <- ggplot(hu_data) +
  geom_point(aes(x = Hlag1_U, y = H_Ulag1), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

ggplot(hu_data) +
  geom_histogram(aes(x = Hlag1_U - H_Ulag1))

ggplot(hu_data) +
  geom_density(aes(x = Hlag1_U - H_Ulag1))

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
