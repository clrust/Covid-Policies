# Created by: CR
# Date: 7/22/26
# Script NY Health Extractor

library(RSelenium)
library(tidyverse)
library(rvest)

setwd("~/covidpolicies/Rescraping")

filenames <- list.files("RawData/NY_Health/", pattern="*", full.names=TRUE)
test_filenames <- sample(filenames, size = 50, replace = FALSE)

ny_health_reader <- function(fname) {
  html <- read_html(fname, encoding = "UTF-8")
  # html <- read_html(filenames[1], encoding = "UTF-8")
  
  title <- html %>%
    html_elements("div#content") %>%
    html_elements("h1#pagetitle") %>% 
    html_text2()
  
  text <- html %>% 
    html_elements("div#content") %>%
    html_text2() %>%
    paste(collapse = " ") %>%
    str_replace_all("\n|\t", " ") %>%
    str_squish()
  
  if (length(title)==0|length(text)==0) {
    title = "unable to extract"
    text = "unable to extract"
  }
  
  row <- tibble(Title = title, Text = text, file = fname)

  cat("Extracted", title)
  
  return(row)
}

# works for press releases from governors office posted by health department
ny_health_reader_gov <- function(fname) {
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
  
  if (length(title)==0|length(text)==0) {
    title = "unable to extract"
    text = "unable to extract"
  }
  
  row <- tibble(Title = title, Text = text, file = fname)
  
  cat("Extracted", title)
  
  return(row)
}

# works for press releases from governors office that no longer exist
ny_health_reader_gov2 <- function(fname) {
  html <- read_html(fname, encoding = "UTF-8")
  
  title <- html %>%
    html_elements(".p-notFound__headline") %>%
    html_text2()
  
  if (length(title)==0) {
    title = "unable to extract"
  }
  
  row <- tibble(Title = title, file = fname)
  return(row)
}

all_year_data <- map_dfr(filenames, .f = ny_health_reader) %>%
  mutate(State = "NY",
         Agency = "Health")

all_year_data_clean <- all_year_data %>%
  filter(Title != "unable to extract")

r2 <- all_year_data %>%
  filter(Title == "unable to extract")

r2_data <-  map_dfr(r2$file, .f = ny_health_reader_gov) %>%
  mutate(State = "NY",
         Agency = "Health")

r2_data_clean <- r2_data %>%
  filter(Title != "unable to extract")

# have to do pdfs byhand
r3 <- r2_data %>%
  filter(Title == "unable to extract")

# mostly pages that no longer exist
r3_data <- map_dfr(r3$file, .f = ny_health_reader_gov2) %>%
  mutate(State = "NY",
         Agency = "Health")

r3_data_clean <- r3_data %>%
  filter(Title != "unable to extract" & Title != "We're sorry, the page that you are looking for is not available.")

# pdfs have to do by hand
# others
r4 <- r3_data %>%
  filter(Title != "We're sorry, the page that you are looking for is not available.")

# writing to box for hand extraction
write_csv(r4, "~/Library/CloudStorage/Box-Box/Covid Policies/Rescraping/HandExtraction/Raw/NY_Health_by_hand.csv")  

r4_data_clean <- read_csv("~/Library/CloudStorage/Box-Box/Covid Policies/Rescraping/HandExtraction/ByHand/NY_Health_by_hand_completed.csv") %>%
  select(-c(Title...1, Notes)) %>%
  rename(Title = Title...2) %>%
  filter(Title != "unable to copy")

# dates not on all press releases. Mergine on dates from the url archive
final_data <- rbind(all_year_data_clean,
                    r2_data_clean,
                    r3_data_clean,
                    r4_data_clean) %>%
  mutate(url = basename(file))

dates_to_merge <- read_csv("URLs/NY_Health_urls.csv") %>%
  mutate(url = basename(url))

final_data_merged <- final_data %>%
  left_join(dates_to_merge, join_by(url==url)) %>%
  select(c(Title, Text, State, Agency, date)) %>%
  rename(Date = date)

write_csv(final_data_merged, "~/Library/CloudStorage/Box-Box/Covid Policies/Rescraping/Data2/NY_Health.csv")

