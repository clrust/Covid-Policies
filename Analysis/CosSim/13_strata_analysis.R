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

# Hypothesis: The difference in influence of the governor and the health department on the university is greater red states than blue states
# this does not seem to be true

r_diff2 <- data2[data2$strata == "red",]$Glag1_U - data2[data2$strata == "red",]$Hlag1_U
b_diff2 <- data2[data2$strata == "blue",]$Glag1_U - data2[data2$strata == "blue",]$Hlag1_U
p_diff2 <- data2[data2$strata == "purple",]$Glag1_U - data2[data2$strata == "purple",]$Hlag1_U
tidy(t.test(r_diff2, b_diff2))
tidy(t.test(p_diff2, b_diff2))


# just raw numbers here 
# Under the admittedly tenuous assumption that Alag1_B is the influence of A on B, this is just comparing influences
# I think the issue is that it is hard to distinguish pure similarity versus influence
# we do see that we get similar plots without the lag
rbind(tidy(t.test(data2[data2$strata == "red",]$Glag1_U, data2[data2$strata == "blue",]$Glag1_U)),
tidy(t.test(data2[data2$strata == "purple",]$Glag1_U, data2[data2$strata == "blue",]$Glag1_U)),
tidy(t.test(data2[data2$strata == "red",]$Hlag1_U, data2[data2$strata == "blue",]$Hlag1_U)),
tidy(t.test(data2[data2$strata == "purple",]$Hlag1_U, data2[data2$strata == "blue",]$Hlag1_U)))

# what I'm not sure about here is the time frame 
tidy(t.test(data2[data2$strata == "red",]$Glag1_H, data2[data2$strata == "blue",]$Glag1_H))
# reverse trend with purple
tidy(t.test(data2[data2$strata == "purple",]$Glag1_H, data2[data2$strata == "blue",]$Glag1_H))

# differences in differences lol
# I don't think this is all that interperable but say we thought about this as some form of 
# normalized difference in influence between G on U and H on U. The higher the number, the more G influences U relatively to H
# the results would suggest that, if anything, the infleunce of G and H is more balanced in red and purple states, and more G heavy in blue states
# but nothing is statistically significant
r_diff3 <- (data2[data2$strata == "red",]$Glag1_U - data2[data2$strata == "red",]$G_Ulag1) - 
  (data2[data2$strata == "red",]$Hlag1_U - data2[data2$strata == "red",]$H_Ulag1)
b_diff3 <- (data2[data2$strata == "blue",]$Glag1_U - data2[data2$strata == "blue",]$G_Ulag1) - 
  (data2[data2$strata == "blue",]$Hlag1_U - data2[data2$strata == "blue",]$H_Ulag1)
p_diff3 <- (data2[data2$strata == "purple",]$Glag1_U - data2[data2$strata == "purple",]$G_Ulag1) - 
  (data2[data2$strata == "purple",]$Hlag1_U - data2[data2$strata == "purple",]$H_Ulag1)


tidy(t.test(r_diff3, b_diff3))
tidy(t.test(p_diff3, b_diff3))

# if anything, these statistically significant negative differences would suggest that it is the red states that listen to Health more
# but purple different?
# would be interested if different lag is different

#-----What I was seeing in the plots was that across the board GH was higher...issue is that GH, GU and HU are definitely not independent
# Dependency because two depend on each of the three embeddings, and likely dependency between days. For simplicity's sake I will just use anova
# while recognizing that this is not valid


# linear mixed effects model
df_long <- pivot_longer(
  data2,
  cols = c(GH, GU, HU),
  names_to = "pair",
  values_to = "similarity"
)

library(lme4)

fit <- lmer(
  similarity ~ pair + (1 | Date),
  data = df_long
)

anova(fit)

#---Scatter plot analysis by strata
#Hypothesis: the influence of the health department on the university is greater in blue states than red states


# Does Governor or Health Influence University when split by strata? Nope
t.test(data2[data2$strata == "blue",]$Glag1_U, data2[data2$strata == "blue",]$G_Ulag1)
t.test(data2[data2$strata == "blue",]$Hlag1_U, data2[data2$strata == "blue",]$H_Ulag1)

t.test(data2[data2$strata == "red",]$Glag1_U, data2[data2$strata == "red",]$G_Ulag1)
t.test(data2[data2$strata == "red",]$Hlag1_U, data2[data2$strata == "red",]$H_Ulag1)


t.test(data2[data2$strata == "purple",]$Glag1_U, data2[data2$strata == "purple",]$G_Ulag1)
t.test(data2[data2$strata == "purple",]$Hlag1_U, data2[data2$strata == "purple",]$H_Ulag1)


# Does governor influence health or the other way around? Neither it seems. However, this might be colored by the fact we interpolated
# I will try analysis with those that are equal filtered out
t.test(data2[data2$strata == "blue",]$Glag1_H, data2[data2$strata == "blue",]$G_Hlag1)
t.test(data2[data2$strata == "red",]$Glag1_H, data2[data2$strata == "red",]$G_Hlag1)
t.test(data2[data2$strata == "purple",]$Glag1_H, data2[data2$strata == "purple",]$G_Hlag1)










