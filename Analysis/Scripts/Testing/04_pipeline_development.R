# Created by: CR
# Date: 4/27/26

library(tidyverse)
library(janitor)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

data <- read_csv("Testing/Results/06_burnham_all_states.csv")

# dates filled in
all_states_complete <- data %>%
  group_by(State, Agency, Date) %>% # if there are multiple releases on one day, average them out
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  group_by(State, Agency) %>% # creating rows for days with no observations
  complete(Date = seq.Date(
    from = as.Date("2020-03-01"),
    to   = as.Date("2022-12-31"),
    by   = "day"
  )) %>%
  arrange(Date) %>%
  fill(everything(), .direction = "down") %>%
  drop_na() 

# finding latest first press release date of any agency
test <- all_states_complete %>% 
  group_by(Agency, State) %>%
  summarize(min = min(Date))

last_first_release <- max(test$min) # currently CA Health, january 1, 2020

# filtering to first day we have releases from all agencies
all_states_complete2 <- all_states_complete %>%
  filter(Date >= last_first_release) %>%
  clean_names() %>%
  select(-x1) %>%
  pivot_wider(names_from = agency,
              values_from = where(is.numeric),
              names_sep = "_")

# some states don't have all of health, university, and governor's office. 
# these are dropped when I filter out the NAs
final_data <- all_states_complete2 %>%
  ungroup() 

# %>%
#   drop_na()

# getting matrices to calculate Euclidean distance
final_data_gov <- final_data %>%
  select(ends_with("Governor")) %>%
  as.matrix()

final_data_health <- final_data %>%
  select(ends_with("Health")) %>%
  as.matrix()

final_data_university <- final_data %>%
  select(ends_with("University")) %>%
  as.matrix()

#Calculating Euclidean distances
U_gov_dist = sqrt(rowSums((final_data_gov - final_data_university)^2))
U_health_dist = sqrt(rowSums((final_data_health - final_data_university)^2))

final_data$U_gov_dist <- U_gov_dist
final_data$U_health_dist <- U_health_dist

# writing cleaned data
write_csv(final_data, "Testing/Results/04_pipeline_development.csv")


summary(lm(U_gov_dist ~ U_health_dist, data = final_data))


# function to run model for a certain state
# args: 
#   data (df): all state data, 
#   state (chr): initials of state of interest, 
#   formula (formula): formula regression model, v
#   var1 (sym): name of first variable to plot on y axis (red), 
#   var2 (sym): name of second variable to plot on y axis (blue)
# 
# returns named list with ggplot object and lm object

run_state <- function(data, state_val, formula, var1, var2){
  state_data <- data %>%
    filter(state == state_val)
  
  model <- lm(formula, data = state_data)
  plot <- ggplot(state_data) +
    geom_line(aes(x = date, y = {{var1}}, color = "government")) +
    geom_line(aes(x = date, y = {{var2}}, color = "health")) +
    labs(title = state_val, y = "distance") +
    scale_color_manual(
      values = c(
        "health" = "red",
        "government" = "blue"
      )
    )
  
  return(list(
    model = model,
    plot = plot
    ))
}

states <- (final_data %>% 
  distinct(state))$state

test <- map(states, 
            ~ run_state(final_data, .x, U_gov_dist ~ U_health_dist, 
                        U_gov_dist, U_health_dist))


a <- test[[1]]$plot



# playing around with VAR
ny <- final_data %>%
  filter(state == "NY")



# 
# tx <- final_data %>%
#   filter(state == "TX")
# ggplot(ny) +
#   geom_line(aes(x = date, y = U_health_dist), color = "red") +
#   geom_line(aes(x = date, y = U_gov_dist), color = "blue")
# 
# a <- ggplot(ny) +
#   geom_line(aes(x = date, y = U_health_dist), color = "red") +
#   scale_y_continuous(limits = c(0, 1.5))
# 
# b <- ggplot(ny) +
#   geom_line(aes(x = date, y = U_gov_dist), color = "blue")+
#   scale_y_continuous(limits = c(0, 1.5))
# 
# 
# c <- ggplot(tx) +
#   geom_line(aes(x = date, y = U_health_dist), color = "red") +
#   scale_y_continuous(limits = c(0, 1.5))
# 
# d <- ggplot(tx) +
#   geom_line(aes(x = date, y = U_gov_dist), color = "blue")+
#   scale_y_continuous(limits = c(0, 1.5))



