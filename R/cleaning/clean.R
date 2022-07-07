library(tidyverse)
library(readxl)
library(rebus)
library(data.table)

#stolpis <- read_xlsx("output/stolpersteine_bios_geo_info_clean.xlsx")

stolpersteine <- readxl::read_excel("output/stolpersteine.xlsx")
bios_clean <- read_tsv("output/bios_clean.tsv")

stolpis <- stolpersteine %>% bind_cols(bios_clean)


#### Geburtsort ----

geburtsorte <- stolpis %>%
  #filter(!is.na(geburtsort)) %>%
  #distinct(geburtsort) %>%
  #select(geburtsort) %>%
  filter(!is.na(geburtsort)) %>%
  count(geburtsort) %>%
  mutate(region = str_extract(geburtsort, "\\(" %R% one_or_more(or(WRD, SPACE, "/")) %R% "\\)"),
         region = str_remove(region, "\\("),
         region = str_remove(region, "\\)"),
         geburtsort_clean = str_remove(geburtsort, "\\(" %R% one_or_more(or(WRD, SPACE, "/")) %R% "\\)"),
         geburtsort_clean = str_squish(geburtsort_clean))

geburtsort_clean <- geburtsorte %>%
  count(geburtsort_clean)

#geburtsort_clean %>%
#  stringdist_inner_join(geburtsort_clean, by = "geburtsort_clean", max_dist = 4)

geburtsort_regex <- geburtsort_clean %>%
  regex_inner_join(geburtsort_clean, by = "geburtsort_clean")

geburtsort_regex <- geburtsort_regex %>%
  #group_by(geburtsort_clean.x) %>%
  add_count(geburtsort_clean.y)
  #mutate(test = )

stolpis %>%
  mutate(geburtsort = as_factor(geburtsort),
         geburtsort = fct_collapse(geburtsort,
                                   "Aachen" = c("Aachen", "Aachen/Rhein"),
                                   "Adelnau (Posen) / Odolanów" = c("Adelnau (Posen) / Odolanów", "Adelnau / Odolanów") )




