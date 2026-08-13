# Created by: CR
# Date: 7/24/26
# Scatterplots comparing influence

library(tidyverse)
library(patchwork)
library(tls)
setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")


cossim_data <- read_csv("Testing/Results/07_cossim_data_filtered.csv") 

# filtering to only days where the university released a press release
uni_data <- cossim_data %>%
  filter(no_release_flag_University == "release") %>%
  mutate(density_cat = if_else(density > 108.3, "high", "low"))
  # 108.3 is the median density across all states
  
# Hypothesis: the governor's office influences the health department and university more than the other way around
# No statistically significant different in either the plots or the data analysis

# university: comparing influence by governor and health, all parties
uni <- ggplot(uni_data) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U, color = party, shape = party), size = 1, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t") +
  scale_color_manual(values = c(
    red = "red",
    blue = "blue",
    purple = "purple"
  ))


blue_uni <- ggplot(filter(uni_data, party == "blue")) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U), size = 1, alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t")

red_uni <- ggplot(filter(uni_data, party == "red")) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U), size = 1, alpha = 0.6, color = "red") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t")

purple_uni <- ggplot(filter(uni_data, party == "purple")) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U), size = 1, alpha = 0.6, color = "purple") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t")

hi_uni <- ggplot(filter(uni_data, density_cat == "high")) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U), size = 1, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t")

low_uni <- ggplot(filter(uni_data, density_cat == "low")) +
  geom_point(aes(x = Hlag1_U, y = Glag1_U), size = 1, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Health_{t-1} and University_t",
       y = "Governor_{t-1} and University_t")



#thinking about doing an analysis where we look only for changes from both
blue <- uni_data %>% filter(party == "blue")
purple <- uni_data %>% filter(party == "purple")
red <- uni_data %>% filter(party == "red")
hi <- uni_data %>% filter(density_cat == "high")
low <- uni_data %>% filter(density_cat == "low")

influence_df <- tidy(t.test(uni_data$Glag1_U, uni_data$Hlag1_U, paired = TRUE)) %>% 
  mutate(strata = "all states")
blue_df <- tidy(t.test(blue$Glag1_U, blue$Hlag1_U, paired = TRUE)) %>%
  mutate(strata = "blue states")# stat sig gov more
red_df <-  tidy(t.test(red$Glag1_U, red$Hlag1_U, paired = TRUE)) %>%
  mutate(strata = "red states") # stat sig health more
purple_df <- tidy(t.test(purple$Glag1_U, purple$Hlag1_U, paired = TRUE)) %>%
  mutate(strata = "purple states") # no stat sig difference
hi_df <-  tidy(t.test(hi$Glag1_U, hi$Hlag1_U, paired = TRUE)) %>%
  mutate(strata = "high density states")
low_df <- tidy(t.test(low$Glag1_U, low$Hlag1_U, paired = TRUE)) %>%
  mutate(strata = "low density states")


t_df <- rbind(influence_df, blue_df, red_df, purple_df, hi_df, low_df) %>%
  rename(t = statistic, p_val = p.value, ci_low = conf.low, ci_hi = conf.high) %>%
  select(-c(parameter, method, alternative))

# paired t test, estimate is the difference between influence of governor and influence of health
tab <- gt(t_df) %>%
  fmt_number(decimals = 3) 


gtsave(tab, "Testing/Results/StarFilteredDataPlots/ScatterPlot/scatter_table.tex")

#--------table stuff end


ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/uni_scatter.pdf",
       plot = uni,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/blue_uni_scatter.pdf",
       plot = blue_uni,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)


ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/red_uni_scatter.pdf",
       plot = red_uni,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/purple_uni_scatter.pdf",
       plot = purple_uni,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/hidensity_uni_scatter.pdf",
       plot = hi_uni,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

ggsave(filename = "Testing/Results/StarFilteredDataPlots/ScatterPlot/lowdensity_uni_scatter.pdf",
       plot = low_uni,
       width = 10,
       height = 8,
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
