source("R/map/geocode-functions.R")
library(leaflet)
library(rebus)

geburtsorte_geocoded <- stolpis %>%
  #filter(!is.na(geburtsort)) %>%
  #distinct(geburtsort) %>%
  #select(geburtsort) %>%
  filter(!is.na(geburtsort)) %>%
  distinct(geburtsort) %>%
  pull(geburtsort) %>%
  map_dfr(get_geocode)

writexl::write_xlsx(geburtsorte_geocoded, "output/geburtsorte_geocoded.xlsx")

geburtsorte_geocoded %>%
  count(locality, sort = TRUE)

geburtsorte_geocoded %>%
  filter(!is.na(address)) %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lat = ~ lat,
                   lng = ~ lng,
                   radius = 2,
                   popup = ~ paste0("<b>", query_address, "</b>", "<br/>", country)
  )

geburtsorte_geocoded_na <- geburtsorte_geocoded %>%
  filter(is.na(address))

stolpi_bio <- bios_clean %>%
  bind_cols(stolpersteine) %>%
  inner_join(geburtsorte_geocoded_na,
             by = c("geburtsort" = "query_address")) %>%
  select(Name, geburtsort, url)

get_birthplace_from_url <- function(url) {
  resp <- slow_GET(url)
  bio_text <- content(resp) %>%
    html_nodes(".field-item p")
  if (length(bio_text) > 0) {
    bio_text <- bio_text[1] %>% html_text()
    str_extract(bio_text,
                or("in", "im") %R%
                  one_or_more(or(WRD, SPACE, "\\(", "\\)", ",", "/", ":", "-")) %R%
                  or("geboren", "auf die Welt")) %>%
      str_remove(START %R% or("in ", "im ")) %>%
      #str_remove("in ") %>%
      #str_remove("im ") %>%
      str_remove(or("geboren", "auf die Welt") %R% END) %>%
      str_squish()
  } else {
    NA
  }
}

geburtsort_query <- map_chr(stolpi_bio$url, get_birthplace_from_url)

stolpi_bio <- stolpi_bio %>%
  mutate(geburtsort_query = geburtsort_query)

map_chr(stolpi_bio$url, get_birthplace_from_url)

slow_GET <- slowly(~ GET(.), rate = rate_delay(1), quiet = TRUE)

