source("R/map/geocode-functions.R")


stolpersteine_geo_coded <- stolpersteine %>%
  mutate(addresse = paste(Strasse, paste0("Berlin-", Ortsteil), sep = ", ")) %>%
  # reduce api use by filtering for distinct addresses
  distinct(addresse) %>%
  pull(addresse) %>%
  map_dfr(get_geocode)

writexl::write_xlsx(stolpersteine_geo_coded, "output/distinct_stolperstein_address_geocoded.xlsx")

# by running this code we see that there are only 8 rows with postal_code == NA
stolpersteine_geo_coded %>%
  summarise_all(~ sum(is.na(.)))

# filter for these rows with postal_code == NA
stolpersteine_geo_coded_na <- stolpersteine_geo_coded %>%
  filter(is.na(postal_code))

