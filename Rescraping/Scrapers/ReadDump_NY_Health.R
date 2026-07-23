# Created by: CR
# Date: 7/22/26
# Script to rescrape NY Health

library(RSelenium)
library(tidyverse)
library(rvest)
library(lubridate)

setwd("~/covidpolicies/Rescraping")

rD <- rsDriver(browser="firefox", port=9835L, verbose=FALSE, phantomver = NULL, check = FALSE)
remDr <- rD[["client"]]

baseurl <- "https://www.health.ny.gov/press/releases/"

years <- c("2020", "2021", "2022")
url_data_NY <- tibble()

year <- "2020"
for (year in years) {
  url <- paste0(baseurl, year, "/")
  
  Sys.sleep(1)
  
  remDr$navigate(url)
  pg.html <- remDr$getPageSource()[[1]] %>%
    read_html()

  # Each date is an h2 followed by a ul containing one or more releases.
  # Build the rows one date at a time so the date is repeated when several
  # releases were published on the same day.
  year_data <- pg.html %>%
    html_elements("#content > h2") %>%
    map_dfr(function(date_node) {
      release_nodes <- date_node %>%
        html_element(xpath = "following-sibling::*[1][self::ul]") %>%
        html_elements("li > a")

      release_nodes <- release_nodes[
        !is.na(html_attr(release_nodes, "href"))
      ]

      if (length(release_nodes) == 0) {
        return(tibble())
      }

      tibble(
        url = release_nodes %>%
          html_attr("href") %>%
          xml2::url_absolute(baseurl),
        date = rep(
          date_node %>% html_text2() %>% mdy() %>% as.Date(),
          length(release_nodes)
        ),
        title = release_nodes %>% html_text2()
      )
    })

  url_data_NY <- bind_rows(url_data_NY, year_data)
}

write_csv(url_data_NY, "URLs/NY_Health_urls.csv")


#---------scraping htmls

start_date <- ymd("2020-03-01")
end_date <- ymd("2022-12-31")

consider_data <- url_data_NY %>%
  filter(date <= end_date) %>%
  filter(date >= start_date)

for (i in seq_along(consider_data$url)) {
  url <- consider_data$url[i]
  cat("Navigating to:", url, "\n")                                 
  remDr$navigate(url)
  Sys.sleep(1)  # Adding a small delay to allow the page to load
  press_release <- remDr$getPageSource()[[1]]
  press_output <- paste0("RawData/NY_Health/", basename(url))
  cat("Writing:", press_output, "\n")                                   # Declare we are writing
  
  # Check if the output directory exists, if not, create it
  if (!file.exists(dirname(press_output))) {
    dir.create(dirname(press_output), recursive = TRUE)
  }
  
  writeLines(press_release, press_output)                                   # Write the press release to the press_output
}                                                                       # Ignore the warnings

# Close RSelenium session
remDr$close()
rD$server$stop()
