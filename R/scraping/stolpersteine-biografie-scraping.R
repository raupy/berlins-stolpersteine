library(readxl)
library(lubridate)
library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(assertive)
library(rebus)
library(writexl)
library(tidyr)


## biographie details scraping functions ----


get_single_biografie_detail <- function(detail, html_node) {
  text <- html_node %>%
    html_nodes(paste0(".", detail)) %>%
    html_text() %>%
    str_squish()
  if(length(text) == 0) {
    text <- NA
  }
  text
}

get_biographie_details <- function(url) {
  resp <- slow_GET(url)
  biografie_html <- content(resp) %>%
    html_nodes(".span7")

  parameter <- c("ort", "bezirk", "datum", "geboren", "beruf",
                 "flucht", "inhaftierung", "deportation", "schicksal")

  details <- map(parameter, ~ get_single_biografie_detail(.x, biografie_html))
  incons <- sapply(details, length) > 1
  incons_det <- details[incons]
  if (length(incons_det) > 0) {
    details[incons] <- sapply(incons_det, paste, collapse = ", ")
  }
  details <- details %>%
    unlist()

  tibble(
    ort = details[1],
    bezirk = details[2],
    datum = details[3],
    geboren = details[4],
    beruf = details[5],
    flucht = details[6],
    inhaftierung = details[7],
    deportation = details[8],
    schicksal = details[9],
  )
}


get_all_biographies <- function(urls) {
  map_dfr(urls, get_biographie_details)
}



