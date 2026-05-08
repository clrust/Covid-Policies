# Created by: CR
# 5/8/26
# Code to produce models and plots at the state level
# Oh wait we want it to use all the data for each state don't we

library(tidyverse)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

final_data <- read_csv("Testing/Results/07_pipeline.csv")

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
        "government" = "red",
        "health" = "blue"
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
