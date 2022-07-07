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

## Preparation ----


## ├ Throttling GET function (gentle webscraping) ----

slow_GET <- slowly(~ GET(.), rate = rate_delay(1), quiet = TRUE)


## ├ Build a vector of urls to crawl ----

get_sites_to_crawl <- function(url) {
  resp <- slow_GET(url)
  assert_are_set_equal(resp$status_code, 200)

  number_of_results_text <- content(resp) %>%
    html_node(".view-footer") %>%
    html_text()

  number_of_results_vec <- str_extract_all(
    number_of_results_text,
    "\\(?[0-9,.]+\\)?")[[1]] %>%
    as.numeric()
  if (length(number_of_results_vec) == 0) {
    warning("The length of number_of_results_vec is 0.
            Cannot find out the number of sites to crawl.")
    sites <- url
  } else {
    assert_are_set_equal(length(number_of_results_vec), 3)
    assert_is_not_null(number_of_results_vec[2])

    number_of_pages <- as.integer(number_of_results_vec[3] /
                                    number_of_results_vec[2])
    sites <- c(paste0(url, "?page=") %>%
                 paste0(0:(number_of_pages - 1))
    )
  }
  sites
}



## Scraping ----

get_stolpersteine_for_page <- function(url, home_url){
  resp <- slow_GET(url)
  stolpersteine_html <- content(resp) %>%
    html_nodes('#table')

  stolpersteine <- stolpersteine_html %>%
    html_table()
  stolpersteine <- stolpersteine[[1]]


  links <- stolpersteine_html %>%
    html_nodes("a")
  links <- links[(ncol(stolpersteine)+1):(ncol(stolpersteine) + nrow(stolpersteine))] %>%
    html_attr("href")

  stolpersteine %>%
    mutate(
      Name = ifelse(
        str_detect(Name, START %R% UPPER %R% one_or_more(LOWER) %R% UPPER),
        str_remove(Name, START %R% UPPER %R% one_or_more(LOWER)),
        Name),
      url = paste0(home_url, links)
    )
}

get_all_stolpersteine <- function(urls, home_url) {
  map_dfr(urls, get_stolpersteine_for_page, home_url)
}




url <- "https://www.stolpersteine-berlin.de"
urls <- get_sites_to_crawl(paste0(url, "/de/stolpersteine-finden"))
stolpersteine <- get_all_stolpersteine(urls, url)

write_xlsx(stolpersteine, "output/stolpersteine.xlsx")

