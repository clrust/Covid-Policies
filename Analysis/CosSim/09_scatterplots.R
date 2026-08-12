# Created by: CR
# Date: 7/24/26
# Scatterplots comparing influence

library(tidyverse)
library(patchwork)
library(tls)
setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

purple_state <- c("VA", "PA", "MN", "MI", "MA")
red_state <- c("TX", "GA", "FL")
blue_state <- c("CA", "CO", "IL", "NY")

cossim_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") %>%
  mutate(party = case_when(
    State %in% purple_state ~ "purple",
    State %in% red_state ~ "red",
    State %in% blue_state ~ "blue",
    .default = NA
  )) 

gh_data <- cossim_data %>% 
  filter(G_Hlag1 != Glag1_H)

# Hypothesis: the governor's office influences the health department and university more than the other way around
# No statistically significant different in either the plots or the data analysis


# these are interesting statistics. Can see they are very close. 
# Could do t-tests on these metrics, split them up by strata etc.
mean(cossim_data$Glag1_H - cossim_data$G_Hlag1, na.rm = T)

mean(cossim_data$Glag1_U - cossim_data$G_Ulag1, na.rm = T)

mean(cossim_data$Hlag1_U - cossim_data$H_Ulag1, na.rm = T)

#nothing is statistically significant, let's stratify!
t.test(cossim_data$Glag1_U, cossim_data$G_Ulag1, paired = T)
t.test(cossim_data$Glag1_H, cossim_data$G_Hlag1)
t.test(cossim_data$Hlag1_U, cossim_data$H_Ulag1)


# very similar coefficients, both less than 1
# story of who influences who is not clear from this metric
lm(G_Hlag1 ~ Glag1_H -1 , data = gh_data)
tls(G_Hlag1 ~ Glag1_H -1 , data = gh_data)

lm(Glag1_H ~ G_Hlag1 -1, data = gh_data)
tls(Glag1_H ~ G_Hlag1 -1 , data = gh_data)

gh <- ggplot(gh_data) +
  geom_point(aes(x = Glag1_H, y = G_Hlag1, color = party, shape = party), size = 1, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Governor_{t-1} and Health_t",
       y = "Health_{t-1} and Governor_t") +
  scale_color_manual(values = c(
    red = "red",
    blue = "blue",
    purple = "purple"
  ))

ggplot(cossim_data) +
  geom_histogram(aes(x = Glag1_H - G_Hlag1))

gh_hist <- ggplot(gh_data) +
  geom_histogram(aes(x = Glag1_H - G_Hlag1), color = "white") +
  theme_bw() +
  labs(x = "difference")

ggplot(gh_data) +
  geom_density(aes(x = Glag1_H - G_Hlag1))

gu_data <- cossim_data %>% 
  filter(G_Ulag1 != Glag1_U)


lm(G_Hlag1 ~ Glag1_H -1, data = gu_data)
tls(G_Hlag1 ~ Glag1_H -1 , data = gu_data)

gu <- ggplot(gu_data) +
  geom_point(aes(x = Glag1_U, y = G_Ulag1, color = party, shape = party), size = 1, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Governor_{t-1} and University_t",
         y = "University_{t-1} and Governor_t") +
  scale_color_manual(values = c(
    red = "red",
    blue = "blue",
    purple = "purple"
  ))


#thinking about doing an analysis where we look only for changes from both

### what happens if I filter out the interpolated days where they are equal...still nothing statistically significant
gu_df <- tidy(t.test(gu_data$Glag1_U, gu_data$G_Ulag1, paired = TRUE)) %>%
  mutate(agencies = "GU")

gh_df <- tidy(t.test(gh_data$Glag1_H, gh_data$G_Hlag1, paired = TRUE)) %>%
  mutate(agencies = "GH")
hu_df <- tidy(t.test(hu_data$Hlag1_U, hu_data$H_Ulag1, paired = TRUE)) %>%
  mutate(agencies = "HU")


t_df <- rbind(gu_df, gh_df, hu_df) %>%
  rename(t = statistic, p = p.value, CI_low = conf.low, CI_hi = conf.high) %>%
  select(-c(parameter, method, alternative))

tab <- gt(t_df) %>%
  fmt_number(decimals = 3) 


gtsave(tab, "Testing/Results/FilteredDataPlots/ScatterPlot/scatter_table.tex")

#--------table stuff end

ggplot(cossim_data) +
  geom_histogram(aes(x = Glag1_U - G_Ulag1))

gu_hist <- ggplot(gu_data) +
  geom_histogram(aes(x = Glag1_U - G_Ulag1), color = "white") +
  theme_bw() +
  labs(x = "difference")

ggplot(gu_data) +
  geom_density(aes(x = Glag1_H - G_Hlag1))

# So minimal it is hard to conclude much
hu_data <- cossim_data %>% 
  filter(H_Ulag1 != Hlag1_U)

lm(H_Ulag1 ~ Hlag1_U -1, data = hu_data)
tls(H_Ulag1 ~ Hlag1_U -1 , data = hu_data)

hu <- ggplot(hu_data) +
  geom_point(aes(x = Hlag1_U, y = H_Ulag1, color = party, shape = party), alpha = 0.5, size = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Health_{t-1} and University_t",
       y = "University_{t-1} and Health_t") +
  scale_color_manual(values = c(
    red = "red",
    blue = "blue",
    purple = "purple"
  ))

hu_hist <- ggplot(hu_data) +
  geom_histogram(aes(x = Hlag1_U - H_Ulag1), color = "white") +
  theme_bw() +
  labs(x = "difference")

ggplot(hu_data) +
  geom_density(aes(x = Hlag1_U - H_Ulag1))

ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/gh_scatter.png",
       plot = gh,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/hu_scatter.png",
       plot = hu,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)


ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/gu_scatter.png",
       plot = gu,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/gh_hist.png",
       plot = gh_hist,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/hu_hist.png",
       plot = hu_hist,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


ggsave(filename = "Testing/Results/FilteredDataPlots/ScatterPlot/gu_hist.png",
       plot = gu_hist,
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
