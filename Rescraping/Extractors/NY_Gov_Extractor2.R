# Created by: CR
# Date: 11/27/25
# Script NY Gov Extractor

library(RSelenium)
library(tidyverse)
library(rvest)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies/Rescraping")

filenames <- list.files("RawData2/NY_Gov/", pattern="*", full.names=TRUE)
# test_filenames <- sample(filenames, size = 50, replace = FALSE)

ny_gov_reader <- function(fname) {
  html <- read_html(fname, encoding = "UTF-8")
  
  title <- html %>%
    html_elements("h1.a-title") %>% 
    html_text2()
  
  date <- html %>%
    html_elements("span.a-date") %>%
    html_text2() %>%
    mdy()
  
  text <- html %>% 
    html_elements("div.o-wysiwyg") %>%
    html_text2() %>%
    paste(collapse = " ") %>%
    str_replace_all("\n|\t", " ") %>%
    str_squish()
  
  row <- tibble(Title = title, Date = date,  Text = text)

  cat("Extracted", title)
  
  return(row)
}

all_year_data <- map_dfr(filenames, .f = ny_gov_reader) %>%
  mutate(State = "NY",
         Agency = "Governor")

write_csv(all_year_data, "Data2/NY_Gov.csv")

#old version of function
# ny_gov_reader <- function(fname) {
#   html <- read_html(fname, encoding = "UTF-8")
#   
#   title <- html %>%
#     html_elements("h1.a-title") %>% 
#     html_text2()
#   
#   date <- html %>%
#     html_elements("span.a-date") %>%
#     html_text2() %>%
#     mdy()
#   
#   text <- html %>% 
#     html_elements("div.o-jazzed-release") %>%
#     html_elements("div.a-text__html.o-jazzed-release__wysiwyg1") %>%
#     html_text2() %>%
#     str_replace_all("\n|\t", " ") %>%
#     str_squish()
#   
#   row <- tibble(Title = title, Date = date,  Text = text)
#   
#   return(row)
# }
#some are much faster than others not sure why