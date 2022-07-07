library(ggmap)
library(tidyverse)
library(readxl)

## Script to get Geocode information for Stolperstein addresses by using Google's Geocode API

get_address_component <- function(component_type, components_list) {
  len <- length(components_list)
  map_type_results <- map_int(1:len,
                              ~ components_list[[.]]$types %>%
                                unlist() %>%
                                str_detect(rebus::exactly(component_type)) %>%
                                sum()
  )
  res <- which(map_type_results == 1)
  if(length(res) == 0) {
    res <- NA
  }
  ifelse(is.na(res),
         NA,
         components_list[[res]]$long_name)
}


get_geocode <- function(addresse) {
  types <- c("sublocality_level_1", "locality","administrative_area_level_1", "country", "postal_code")
  geocoded <- geocode(addresse, output = "all")

  if (class(geocoded)[1] == "list") {
    sapply(types, get_address_component,
           geocoded$results[[1]]$address_components) %>%
      bind_rows() %>%
      mutate(sublocality_level_1 = str_remove(sublocality_level_1, fixed("Bezirk ")),
             address = geocoded$results[[1]]$formatted_address,
             lat = geocoded$results[[1]]$geometry$location$lat,
             lng = geocoded$results[[1]]$geometry$location$lng,
             query_address = addresse) %>%
      select(query_address, address, lat, lng, everything())
  } else {
    tibble(query_address = addresse, address = NA, lat = NA, lng = NA,
           sublocality_level_1 = NA, locality = NA, administrative_area_level_1 = NA,
           country = NA, postal_code = NA)
  }

}


# run with your own Google Geocode API key
register_google(key = "[your key]")
