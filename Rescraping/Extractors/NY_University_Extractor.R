# R code to extract from the NY_Health RawData file

setwd("~/Library/CloudStorage/Box-Box/Covid Policies")

library(rvest)
library(xml2)
library(stringr)
library(lubridate)
library(dplyr)
library(purrr)

filenames <- list.files("RawData/NY_University/", pattern="*", full.names=TRUE)

# problem is that all the files have different structure...
# VERY FEW are caught by the first try.

NY_University_Reader <- function(FNAME) {
  this_ref <- FNAME
    
  thepage.h <- xml2::read_html(this_ref)
  
  state.title <- thepage.h %>% html_element("title") %>% 
    html_text() 
  
  temp <- thepage.h %>% html_elements("div.entry-meta") %>% 
    html_elements("div.meta-item.meta-date") %>% 
    html_elements("span.updated") %>%
    html_text()
  state.date <- temp[1] %>% mdy()
  
  temp <- thepage.h %>% html_elements("div.entry-content") %>%
    html_text()
  state.text <- temp[1] %>% 
    str_replace_all("\\n", " ") %>% 
    str_replace_all("\\t", " ") %>%
    str_trim()
  
  tibble(Title=state.title, Date=state.date, Text=state.text)
}

NY_University.t <- map_dfr(filenames, NY_University_Reader) %>%
  mutate(State = "NY", Agency = "University") %>%
  select(Date, Title, Text, State, Agency)

readr::write_csv(NY_University.t, "Data/NY_University.csv")

