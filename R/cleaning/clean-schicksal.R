library(tidyverse)
library(rebus)

bios_clean <- read_tsv("output/bios_clean.tsv")

## Extract infos from schicksal column  ----

destiny <- bios_clean %>%
  select(schicksal) %>%
  mutate(schicksal = str_remove_all(schicksal, QUESTION),
         destiny_location = str_extract(schicksal, or("in", "im") %R% ".*"),


         destiny_type = str_extract(schicksal,
                                    one_or_more(or(UPPER,
                                                   SPACE,
                                                   "ß", "ü", "u", "ä",
                                                   "ö", "\"", "-"))) %>%
           str_trim() %>%
           str_remove(SPACE %R% UPPER %R% optional(or("u", "ä")) %R% END),
         destiny_type_fac = as_factor(destiny_type) %>%
           fct_explicit_na(na_level = "unknown") %>%
           fct_collapse("murdered" = c("ERMORDET",
                                       "ERMORDET IM RAHMEN DES \"\"EUTHANASIE\"\"-PROGRAMMS",
                                       "ERMORDET WäHREND KöPENICKER BLUTWOCHE",
                                       "ERSCHOSSEN ALS GEISEL",
                                       "HINGERICHTET",
                                       "IN DEN TOD GETRIEBEN"),
                        "deceased" = c("FLUCHT IN DEN TOD", "FüR TOT ERKLäRT", "GESTORBEN",
                                       "TOD AUF FLüCHTLINGSSCHIFF", "TOT", "AN HAFTFOLGEN GESTORBEN",
                                       "VERSTORBEN AN DEN FOLGEN VON HAFT UND FOLTER",
                                       "NACH MIßHANDLUNGEN GESTORBEN"),
                        "unknown" = c("VERBLEIB UNKLAR", "VERSCHOLLEN", "LETZTES LEBENZEICHEN"),
                        "survived" = "ÜBERLEBT") %>%
           fct_infreq(),


         destiny_when = str_remove(schicksal, destiny_type) %>%
           str_remove(destiny_location) %>%
           str_trim(),
         destiny_when = ifelse(destiny_when == "",
                               NA,
                               destiny_when),
         destiny_when = ifelse(str_detect(destiny_when, DGT),
                               destiny_when,
                               NA),
         contains_klammer = str_detect(destiny_when, or(OPEN_PAREN, CLOSE_PAREN)),
         destiny_when = str_remove(destiny_when, OPEN_PAREN) %>%
           str_remove(CLOSE_PAREN) %>%
           str_remove(SPACE %R% one_or_more(NOT_DGT)),
         destiny_when_date = lubridate::dmy(destiny_when),
         destiny_when_year = str_extract(destiny_when, repeated(DGT, lo = 4, hi = 4)) %>% as.numeric(),


         destiny_location = str_remove(destiny_location, START %R% or("in ", "im ")),
         destiny_location = str_remove(destiny_location, START %R% or("dem ", "der ", "dem ")),
         destiny_location = ifelse(destiny_location == "",
                                   NA,
                                   destiny_location)
         ) %>%
  select(-contains_klammer)


## Summarise destiny location by using fuzzyjoin package ----

destiny_loc <- destiny %>%
  distinct(destiny_location)
  #count(destiny_location)

destiny_loc_regex <- destiny_loc %>%
  fuzzyjoin::regex_inner_join(destiny_loc, by = "destiny_location") %>%
  add_count(destiny_location.y) %>%
  group_by(destiny_location.x) %>%
  mutate(destiny_location_collapse = destiny_location.y[which.max(n)]) %>%
  ungroup() %>%
  select(destiny_location.x, destiny_location_collapse) %>%
  distinct() %>%
  arrange(destiny_location_collapse)

destiny_loc_regex <- destiny_loc_regex %>%
  fuzzyjoin::stringdist_inner_join(
    destiny_loc_regex %>% select(destiny_location_collapse),
    by = c("destiny_location_collapse" = "destiny_location_collapse"),
    max_dist = 2) %>%
  add_count(destiny_location_collapse.y) %>%
  group_by(destiny_location.x) %>%
  mutate(destiny_location_collapse = destiny_location_collapse.y[which.max(n)]) %>%
  ungroup() %>%
  select(destiny_location.x, destiny_location_collapse) %>%
  rename(destiny_location = destiny_location.x,
         destiny_loc_cat = destiny_location_collapse) %>%
  distinct()

  destiny <- destiny %>%
  left_join(destiny_loc_regex, by = "destiny_location") %>%
  mutate(destiny_loc_fac = as_factor(destiny_loc_cat) %>%
           fct_explicit_na(na_level = "Unknown") %>%
           fct_collapse("Chełmno / Kulmhof" = c("Chelmno", "Kulmhof / Chełmno"),
                        "Unknown" = "unbekannt") %>%
           fct_lump_min(min = 100) %>%
           fct_infreq() %>%
           fct_relevel("Other", after = Inf) %>%
           fct_relevel("Unknown", after = Inf) %>%
           fct_rev()
           ) %>%
  select(schicksal,
         destiny_type, destiny_type_fac,
         destiny_when, destiny_when_date, destiny_when_year,
         destiny_location, destiny_loc_cat, destiny_loc_fac)

write_csv(destiny, "output/destiny.csv")


