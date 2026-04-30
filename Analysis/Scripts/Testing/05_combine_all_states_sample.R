# Created by: CR
# Date: 4/27/26
# combining all states into one tibble, taking a random sample of institution;
# The purpose of this is to allow me to have a sample that is small enough to run on my local device, 
# which I can use to write all the necessary code
library(tidyverse)

data_path <- "~/Library/CloudStorage/Box-Box/Covid Policies/Data"
rescraped_path <- "~/Library/CloudStorage/Box-Box/Covid Policies/Rescraping/Data2"


setwd(data_path)

# MA
MA.t <- read_csv("MA0.csv") %>%
  rename(Title = title,
         Date = date,
         Text = content) %>%
  mutate(State = "MA",
         Agency = if_else(source == "MA_DPH", "Health", 
                          if_else(source == "MA_Gov", "Governor", "University"))) %>%
  select(Date, Title, Text, State, Agency)

# CA
setwd(rescraped_path)
CA_Gov.t <- read_csv("CA_Gov.csv")

setwd(data_path)
CA_Health.t <- read_csv("CA_Health.csv")

CA_University.t <- read_csv("CA_University.csv")
CA.t <- rbind(CA_Gov.t, CA_Health.t) %>%
  mutate(Date = date(Date))

# CO
CO_Gov.t <- read_csv("CO_Gov.csv")
CO_Health.t <- read_csv("CO_Health.csv")
CO_University.t <- read_csv("CO_University.csv")
CO.t <- rbind(CO_Gov.t, CO_Health.t, CO_University.t)

# FL
setwd(rescraped_path)
FL_Gov.t <- read_csv("FL_Gov.csv") 
FL_Health.t <- read_csv("FL_Health_r2.csv") %>%
  select(-c(non_local_flag, Filename))

# FL University here
FL.t <- rbind(FL_Gov.t, FL_Health.t)

# GA
setwd(data_path)
GA_Gov.t <- read_csv("GA_Gov.csv") %>%
  mutate(State = "GA", Agency="Governor")
GA_Health.t <- read_csv("GA_Health.csv")
GA_University.t <- read_csv("GA_University.csv")
GA.t <- rbind(GA_Gov.t, GA_Health.t, GA_University.t)

# IL
IL_Gov.t <- read_csv("IL_Gov.csv")
IL_Health.t <- read_csv("IL_Health.csv")
setwd(rescraped_path)
IL_University.t <- read_csv("IL_University_r2.csv") %>%
  select(-c(non_local_flag, File))
IL.t <- rbind(IL_Gov.t, IL_Health.t, IL_University.t)

# MA
setwd(data_path)
MA.t <- read_csv("MA0.csv") %>%
  rename(Title = title,
         Date = date,
         Text = content,
         Agency = source) %>%
  mutate(State = "MA",
         Agency = if_else(Agency == "MA_Gov", "Governor",
                          if_else(Agency == "MA_DPH", "Health", "University")))
# MI
MI_Gov.t <- read_csv("MI.csv")
MI_Health.t <- read_csv("MI_Health.csv")
MI_University.t <- read_csv("MI_University.csv")
MI.t <- rbind(MI_Gov.t, MI_Health.t, MI_University.t)

# MN
MN_Gov.t <- read_csv("MN_gov_short.csv")
MN_Health.t <- read_csv("MI_Health.csv") %>%
  mutate(State = "MN")
MN_University.t <- read_csv("MN_University.csv")
MN.t <- rbind(MN_Gov.t, MN_Health.t, MN_University.t)

# NC
# NC Gov here
NC_Health.t <- read_csv("NC_Health.csv")
NC_University.t <- read_csv("NC_University.csv")
NC.t <- rbind(NC_Health.t, NC_University.t)

# NY
setwd(rescraped_path)
NY_Gov.t <- read_csv("NY_Gov.csv")
NY_University.t <- read_csv("NY_University_r2.csv") %>%
  select(-File)
setwd(data_path)
NY_Health.t <- read_csv("NY_Health.csv") %>%
  mutate(Date = lubridate::mdy(Date))
NY.t <- rbind(NY_Gov.t, NY_Health.t, NY_University.t)

# OH
OH_Gov.t <- read_csv("OH_Gov.csv")
OH_Health.t <- read_csv("OH_Health.csv")
OH_University.t <- read_csv("OH_University.csv")
OH.t <- rbind(OH_Gov.t, OH_Health.t, OH_University.t)

# PA
PA_Gov.t <- read_csv("PA_Gov.csv")
PA_Health.t <- read_csv("PA_Health.csv")
PA_University.t <- read_csv("PA_University.csv")
PA.t <- rbind(PA_Gov.t, PA_Health.t, PA_University.t)

# TX
setwd(rescraped_path)
TX_Gov.t <- read_csv("TX_Gov.csv")
TX_Health.t <- read_csv("TX_Health.csv")
TX_University.t <- read_csv("TX_University.csv")
TX.t <- rbind(TX_Gov.t, TX_Health.t, TX_University.t)

# VA
setwd(data_path)
VA_Health.t <- read_csv("VA_Health.csv")
VA_University.t <- read.csv("VA_University.csv")
VA_Gov.t <- read_csv("VA_Gov.csv") %>%
  mutate(State="VA", Agency="Governor")
VA.t <- rbind(VA_Gov.t,VA_Health.t, VA_University.t)

# WI

All.t <- rbind(CA.t, CO.t, FL.t, GA.t, IL.t, MA.t, MI.t, MN.t, NC.t, NY.t, PA.t, TX.t, VA.t)
# All.t %>% write_csv("All.csv")

All.t_sample <- All.t %>%
  drop_na() %>%
  group_by(State, Agency) %>%
  slice_sample(prop = 0.1)
All.t_sample %>% write_csv("05_combine_all_states_sample.csv")


