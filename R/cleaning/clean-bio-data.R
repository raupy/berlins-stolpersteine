library(tidyverse)
library(readxl)
library(rebus)
library(data.table)
library(lubridate)


bios <- readxl::read_excel("output/all_bios.xlsx")



## cleaning biographies ----


clean_dates <- function(datum) {
  Month <- str_extract(datum, UPPER %R% one_or_more(LOWER))
  month <- case_when(Month == "Januar" ~ "01",
                     Month == "Februar" ~ "02",
                     Month == "März" ~ "03",
                     Month == "April" ~ "04",
                     Month == "Mai" ~ "05",
                     Month == "Juni" ~ "06",
                     Month == "Juli" ~ "07",
                     Month == "August" ~ "08",
                     Month == "September" ~ "09",
                     Month == "Oktober" ~ "10",
                     Month == "November" ~ "11",
                     Month == "Dezember" ~ "12",
                     TRUE ~ "13")
  datum <- str_remove(datum, fixed(paste0(Month, " ")))
  paste0("15.", month, ".", datum)
}




bios_clean <- bios %>%
  mutate(ort = str_remove(ort, fixed("VERLEGEORT ")),
         bezirk = str_remove(bezirk, fixed("BEZIRK/ORTSTEIL "))
  ) %>%
  separate(bezirk, into = c("bezirk", "ortsteil"), sep = "\u0096") %>%
  mutate(datum = str_remove(datum, fixed("VERLEGEDATUM ")),
         datum = ifelse(str_detect(datum, UPPER %R% one_or_more(LOWER)),
                        clean_dates(datum),
                        datum),
         date = dmy(datum),
         geboren = str_remove(geboren, fixed("GEBOREN ")),
         geburtsort = ifelse(str_detect(geboren, fixed(" in ")),
                             str_remove(geboren, one_or_more(DGT) %R% DOT %R%
                                          one_or_more(DGT) %R% DOT %R%
                                          one_or_more(DGT) %R% " in "),
                             NA) %>%
           str_remove(repeated(DGT, lo = 4, hi = 4) %R% " in ") %>%
           as_factor(),
         geboren2 = str_extract(geboren, one_or_more(DGT) %R% DOT %R%
                                  one_or_more(DGT) %R% DOT %R%
                                  one_or_more(DGT)) %>% dmy(),
         geburtsjahr = str_extract(geboren, repeated(DGT, lo = 4, hi = 4) ) %>% as.numeric(),
         beruf = str_remove(beruf, fixed("BERUF ")),
         flucht = str_remove(flucht, fixed("FLUCHT ")),
         deportation = str_remove(deportation, "DEPORTATION" %R% optional(SPACE))
  )


write_tsv(bios_clean, "output/bios_clean.tsv")


geo_coded_info_clean <- read_excel("output/geo_coded_info_clean.xlsx")
stolpersteine <- read_excel("output/stolpersteine.xlsx")


stolpersteine_bios_geo_info_clean <- bind_cols(stolpersteine, bios_clean) %>%
  mutate(berlin = "Berlin") %>%
  select(berlin, Ortsteil, everything()) %>%
  unite("Stadt", berlin:Ortsteil, sep = "-", remove = FALSE) %>%
  select(Strasse, Stadt, everything()) %>%
  unite("Adresse", Strasse:Stadt, sep = " ", remove = FALSE) %>%
  full_join(geo_coded_info_clean, by = c("Strasse", "Stadt"))


# final dataset! just needs some more cleaning for "flucht", "inhaftierung", "deportation", "schicksal" columns
write_xlsx(stolpersteine_bios_geo_info_clean, "output/stolpersteine_bios_geo_info_clean.xlsx")
