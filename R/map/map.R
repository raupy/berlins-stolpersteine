library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)

stolpersteine <- readxl::read_excel("output/stolpersteine.xlsx")
stolpersteine_geo_coded <-  readxl::read_excel("output/distinct_stolperstein_address_geocoded.xlsx")

districts <- stolpersteine_geo_coded %>%
  distinct(sublocality_level_1) %>%
  pull(sublocality_level_1)

mypalette <- brewer.pal(12, "Set3")
pal <- colorFactor(palette = mypalette, levels = districts)

stolpersteine %>%
  mutate(query_address = paste(Strasse, paste0("Berlin-", Ortsteil), sep = ", ")) %>%
  full_join(stolpersteine_geo_coded, by = "query_address") %>%
  filter(str_detect(query_address, "Falkentaler Steig")) %>%
  select(lat, lng)

my_popup <- function(query_address_popup) {
  stolpersteine %>%
    mutate(query_address = paste(Strasse, paste0("Berlin-", Ortsteil), sep = ", ")) %>%
    full_join(stolpersteine_geo_coded, by = "query_address") %>%
    filter(str_detect(query_address, query_address_popup)) %>%
    select(Name, Gebjahr) %>%
    unite("person", Name:Gebjahr, sep = ", ") %>%
    pull(person) %>%
    paste0("<br/>", collapse = "")
}

stolpersteine %>%
  mutate(query_address = paste(Strasse, paste0("Berlin-", Ortsteil), sep = ", ")) %>%
  full_join(stolpersteine_geo_coded, by = "query_address") %>%
  #group_by(lat, lng) %>%
  #mutate(lat_jitter = jitter(lat, factor = 0.0002),
  #       lng_jitter = jitter(lng, factor = 0.0002)) %>%
  #ungroup() %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addMarkers(lat = ~ lat,
                   lng = ~ lng,
                   #radius = 2,
                   #color = ~ pal(sublocality_level_1),
                   popup = ~ paste0("<b>", Name, "</b>", "<br/>", query_address), #~my_popup(query_address)
                   clusterOptions = markerClusterOptions()) #%>%
  #addLegend(pal = pal,
  #          values = districts,
  #          opacity = 0.5, title = "Bezirk", position = "topright")



### datacamp code ----

m4 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  addProviderTiles("Esri", group = "Esri") %>%
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),  group = "Public") %>%
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "Private")  %>%
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label), group = "For-Profit")  %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"),
                   overlayGroups = c("Public", "Private", "For-Profit")) %>%
  setView(lat = 39.8282, lng = -98.5795, zoom = 4)


## search
m4_search <- m4  %>%
  addSearchFeatures(
    targetGroups = c("Public", "Private", "For-Profit"),
    # Set the search zoom level to 18
    options = searchFeaturesOptions(zoom = 18))

# Try searching the map for a college
m4_search


