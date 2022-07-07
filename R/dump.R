
## biographie ----
write_xlsx(bios, "output/bios0001-0100.xlsx")
bios101_1000 <- get_all_biographies(stolpersteine$url[101:1000])
bios1001_2000 <- get_all_biographies(stolpersteine$url[1001:2000])
bios2001_3000 <- get_all_biographies(stolpersteine$url[2001:3000])
bios3001_4000 <- get_all_biographies(stolpersteine$url[3001:4000])
bios4001_5000 <- get_all_biographies(stolpersteine$url[4001:5000])
bios5001_6000 <- get_all_biographies(stolpersteine$url[5001:6000])
bios6001_7000 <- get_all_biographies(stolpersteine$url[6001:7000])
bios7001_8000 <- get_all_biographies(stolpersteine$url[7001:8000])
bios8001_8740 <- get_all_biographies(stolpersteine$url[8001:8740])

all_bios <- read_excel("output/bios0001-0100.xlsx") %>%
  bind_rows(bios101_1000, bios1001_2000, bios2001_3000, bios3001_4000,
            bios4001_5000, bios5001_6000, bios6001_7000, bios7001_8000, bios8001_8740)

write_xlsx(all_bios, "output/all_bios.xlsx")




# stolpersteine ----

## geo info ----
geo_info <- stolpersteine %>%
  mutate(berlin = "Berlin") %>%
  select(Strasse, berlin, Ortsteil) %>%
  unite("Stadt", berlin:Ortsteil, sep = "-") %>%
  distinct(Strasse, Stadt)
write_xlsx(geo_info, "output/geo_info.xlsx")

geo1 <- read_csv("data/12.6.2022-19-00-54.csv") %>%
  mutate(postcode = as.character(postcode))
geo2 <- read_csv("data/12.6.2022-19-06-14.csv")
geo3 <- read_csv("data/12.6.2022-19-08-46.csv")
geo4 <- read_csv("data/12.6.2022-19-08-48.csv")
geo5 <- read_csv("data/12.6.2022-19-12-22.csv")
geo6 <- read_csv("data/12.6.2022-19-13-07.csv")
geo7 <- read_csv("data/12.6.2022-19-16-23.csv")

geo_coded_info <- bind_rows(geo1, geo2, geo3, geo4, geo5, geo6, geo7) %>%
  separate(original_address, into = c("Strasse", "Stadt"), sep = "\t")
write_xlsx(geo_coded_info, "output/geo_coded_info.xlsx")

falsch <- geo_coded_info %>%
  arrange(city) %>%
  head(10) %>%
  bind_rows(geo_coded_info %>%
              arrange(desc(city)) %>%
              head(109))

write_xlsx(falsch, "output/falsch.xlsx")




## zusammenführen von falschen und richtigen ----

falsche_neu <- read_csv("data/falsche-neu.csv") %>%
  mutate(postcode = as.character(postcode))
falsche_neu$original_address <- falsch$Strasse
falsche_neu$Stadt <- falsch$Stadt
falsche_neu <- falsche_neu %>% rename(Strasse = original_address)

geo_coded_info_clean <- geo_coded_info %>%
  arrange(city) %>%
  filter(!row_number() %in% 1:10) %>%
  arrange(desc(city)) %>%
  filter(!row_number() %in% 1:109) %>%
  bind_rows(falsche_neu)

na_s <- geo_coded_info_clean %>% filter(is.na(lat) | is.na(lon))
write_xlsx(na_s, "output/na_s.xlsx")

na_s_neu <- read_csv("data/na_s-neu.csv") %>%
  mutate(postcode = as.character(postcode),
         housenumber = as.character(housenumber))
na_s_neu$original_address <- na_s$Strasse
na_s_neu$Stadt <- na_s$Stadt
na_s_neu <- na_s_neu %>% rename(Strasse = original_address)

geo_coded_info_clean <- geo_coded_info_clean %>%
  filter(!is.na(lat)) %>%
  bind_rows(na_s_neu)
write_xlsx(geo_coded_info_clean, "output/geo_coded_info_clean.xlsx")






####  stolpersteine_geo_info ----


stolpersteine_geo_info <- stolpersteine %>%
  mutate(berlin = "Berlin") %>%
  select(berlin, Ortsteil, everything()) %>%
  unite("Stadt", berlin:Ortsteil, sep = "-", remove = FALSE) %>%
  select(Strasse, Stadt, everything()) %>%
  unite("Adresse", Strasse:Stadt, sep = " ", remove = FALSE) %>%
  full_join(geo_coded_info_clean, by = c("Strasse", "Stadt")) %>%
  arrange(city)

write_xlsx(stolpersteine_geo_info, "output/stolpersteine_geo_info.xlsx")
