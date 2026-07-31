# Created by: CR
# Date: 10/6/2025
# FL University Extractor (UF website)

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)

setwd("~/Library/CloudStorage/Box-Box/Covid Policies")


extract_title <- function(html) {
  selectors <- c(
    ".Story__Title",       # UF Health
    ".image-hero h1",      # UF News with hero image
    "#content h1",         # Other UF News pages
    ".single-news h1",     # Engineering
    ".PageTitle",          # Veterinary Medicine
    "main h1",             # General fallback
    "title"                # Final browser-title fallback
  )
  
  for (selector in selectors) {
    value <- html %>%
      html_elements(selector) %>%
      html_text2() %>%
      str_squish()
    
    value <- value[value != ""]
    
    if (length(value) > 0) {
      return(value[1])
    }
  }
  
  NA_character_
}

extract_date <- function(html) {
  # Prefer machine-readable publication dates when they are available.
  date_candidates <- c(
    html %>%
      html_elements('meta[property="article:published_time"]') %>%
      html_attr("content"),
    html %>%
      html_elements("time.entry-date") %>%
      html_attr("datetime"),
    html %>%
      html_elements("time") %>%
      html_attr("datetime")
  )

  date_candidates <- date_candidates[
    !is.na(date_candidates) & date_candidates != ""
  ]

  if (length(date_candidates) > 0) {
    parsed_date <- suppressWarnings(
      parse_date_time(
        date_candidates,
        orders = c("ymd HMSz", "ymd HMz", "ymd"),
        quiet = TRUE
      )
    )
    parsed_date <- as.Date(parsed_date[!is.na(parsed_date)])

    if (length(parsed_date) > 0) {
      return(parsed_date[1])
    }
  }

  # UF Health and UF News put the date in visible byline/meta text.
  visible_metadata <- html %>%
    html_elements(
      paste(
        ".Story__Byline",
        ".single-news-meta",
        ".entry-meta",
        ".entry-date",
        sep = ", "
      )
    ) %>%
    html_text2() %>%
    paste(collapse = " ")

  date_text <- str_extract(
    visible_metadata,
    regex(
      paste0(
        "\\b(?:January|February|March|April|May|June|July|",
        "August|September|October|November|December)\\s+",
        "\\d{1,2},\\s+\\d{4}\\b"
      ),
      ignore_case = TRUE
    )
  )

  if (is.na(date_text)) {
    return(as.Date(NA))
  }

  suppressWarnings(mdy(date_text))
}


extract_first_nonempty <- function(html, selectors) {
  for (selector in selectors) {
    nodes <- html %>%
      html_elements(selector)
    
    if (length(nodes) == 0) {
      next
    }
    
    # UF News pages place the article title inside the body container
    if (selector == ".single-news-body") {
      nodes %>%
        html_elements("h1") %>%
        xml2::xml_remove()
    }
    
    values <- nodes %>%
      html_text2()
    
    values <- values[values != ""]
    
    if (length(values) > 0) {
      return(
        values %>%
          paste(collapse = " ") %>%
          str_squish()
      )
    }
  }
  
  NA_character_
}

fl_university_reader <- function(fname) {
  html <- read_html(fname, encoding = "UTF-8")
  
  # Extract the visible headline before body elements are modified
  title <- extract_title(html)
  date <- extract_date(html)
  
  text <- extract_first_nonempty(
    html,
    c(
      ".Story__EntryContent",
      ".single-news-body",
      ".single-news.container .entry-content",
      "main#main .entry-content"
    )
  )
  
  tibble(
    Date = date,
    Title = title,
    Text = text,
    file = fname,
    extraction_success = !is.na(text)
  )
}

filenames <- list.files(
  "RawData/FL_University",
  full.names = TRUE
)

fl_university_data <- map_dfr(
  filenames,
  possibly(
    fl_university_reader,
    otherwise = tibble(
      Date = as.Date(NA),
      Title = NA_character_,
      Text = NA_character_,
      file = NA_character_,
      extraction_success = FALSE
    )
  )
)

all_year_data <- fl_university_data %>%
  filter(extraction_success) %>%
  mutate(
    State = "FL",
    Agency = "University"
  ) %>%
  select(Date, Title, Text, State, Agency)
 

write_csv(all_year_data, "Data/FL_University.csv")
