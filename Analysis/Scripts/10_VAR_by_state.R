# VARs run by state
# Date: 5/31/26
# Created by: CR

library(vars)
library(tidyverse)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Analysis")

# this data is not filtered to the last first agency message date
state_min_data <- read_csv("Testing/Results/07_pipeline.csv")

# function to run VAR model for a certain state
# args: 
#   data (df): all state data, 
#   state_str (chr): initials of state of interest, 
#   lag (num): number of days of to include in the VAR model
# 
# 
# returns VAR object

var_by_state <- function(data, state_str, lag) {
  state_df <- data %>%
    filter(state == state_str)
  
  min_date <- min(state_df$date)
  min_year <- year(min_date) # year of the earliest press release from that
  min_year_j1 <- lubridate::make_date(year(min_date), 1, 1) 
  days_into_year <- min_date - min_year_j1 # number of days into the year
  
  gov_dist <- ts(state_df$U_gov_dist,
                   start = c(min_year, days_into_year),
                   frequency = 365)
  
  health_dist <- ts(state_df$U_health_dist,
                    start = c(min_year, days_into_year),
                    frequency = 365)
  
  gov_health <- window(ts.union(gov_dist, health_dist), start = c(min_year, days_into_year))
  v1 <- VAR(gov_health, p = lag)
  return(v1)
}

var_by_state(state_min_data, "NY", 7)



#--------------VAR whole data


gov_dist <- ts(state_df$U_gov_dist,
               start = c(min_year, days_into_year),
               frequency = 365)

health_dist <- ts(state_df$U_health_dist,
                  start = c(min_year, days_into_year),
                  frequency = 365)


# this data is already filtered to the last first agency message date
overall_max_min_data <- read_csv("Testing/Results/04_pipeline_development.csv")




# how would you run this on the entire set of data? average out the distances for each day? Average across states?




