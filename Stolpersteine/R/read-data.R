library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(tidyverse)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)

# babynames <- read_tsv("Stolpersteine/data/babynames.tsv")
# stolpersteine <- readxl::read_excel("Stolpersteine/data/stolpersteine.xlsx")
# stolpersteine_geo_coded <- readxl::read_excel("Stolpersteine/data/distinct_stolperstein_address_geocoded.xlsx")
# bios_clean <- read_tsv("Stolpersteine/data/bios_clean.tsv") %>% select(-deportation, -schicksal)
# destiny <- read_csv("Stolpersteine/data/destiny.csv")
# deportation <- read_tsv("Stolpersteine/data/deportation.tsv")

babynames <- read_tsv("data/babynames.tsv")
stolpersteine <- readxl::read_excel("data/stolpersteine.xlsx")
stolpersteine_geo_coded <- readxl::read_excel("data/distinct_stolperstein_address_geocoded.xlsx")
bios_clean <- read_tsv("data/bios_clean.tsv") %>% select(-deportation, -schicksal)
destiny <- read_csv("data/destiny.csv")
deportation <- read_tsv("data/deportation.tsv")

districts <- stolpersteine_geo_coded %>%
  distinct(sublocality_level_1) %>%
  pull(sublocality_level_1)

mypalette <- RColorBrewer::brewer.pal(12, "Set3")
pal <- colorFactor(palette = mypalette, levels = districts)

stolpis <- stolpersteine %>%
  mutate(query_address = paste(Strasse, paste0("Berlin-", Ortsteil), sep = ", ")) %>%
  full_join(stolpersteine_geo_coded, by = "query_address") %>%
  bind_cols(bios_clean) %>%
  mutate(Jahr = lubridate::year(date),
         Monat = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(Bezirk = as_factor(sublocality_level_1) %>%
           fct_infreq())

erinnerte <- stolpersteine %>%
  select(-Gebjahr, -Ortsteil) %>%
  bind_cols(bios_clean) %>%
  select(-ort, -geboren) %>%
  bind_cols(destiny) %>%
  bind_cols(deportation) %>%
  bind_cols(babynames %>% select(sex, beruf_cat)) %>%
  mutate(dep_fac = factor(nach_fct) %>%
           fct_collapse("Litzmannstadt" = c("Łódź / Litzmannstadt"),
                        "Kowno" = c("Kowno / Kaunas"),
                        "Sobibor" = c("Sobibór"),
                        #"Chelmno" = c("Chełmno / Kulmhof", "Kulmhof / Chełmno"),
                        "Bernburg" = c("Bernburg/Saale"),
                        #"Belzec" = c("Bełżec")
           ),
         dest_fac = factor(destiny_loc_cat) %>%
           fct_collapse(#"Litzmannstadt" = c("Łódź / Litzmannstadt"),
             #"Kowno" = c("Kowno / Kaunas"),
             #"Sobibor" = c("Sobibór"),
             "Chelmno" = c("Chełmno / Kulmhof", "Kulmhof / Chełmno"),
             #"Bernburg" = c("Bernburg/Saale"),
             #"Belzec" = c("Bełżec")
           )
  )

my_theme2 <- bs_theme(
  bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
  base_font = font_google("Inconsolata")
)

thematic_shiny(font = "auto")


sex_count <- erinnerte %>% count(sex)
males_count <- sex_count$n[1]
females_count <- sex_count$n[2]
median_gebjahr <- stolpis %>% pull(Gebjahr) %>% median(na.rm = TRUE)
murdered_count <- destiny %>% count(destiny_type_fac) %>% filter(destiny_type_fac == "murdered") %>% pull(n)
survived_count <- destiny %>% count(destiny_type_fac) %>% filter(destiny_type_fac == "survived") %>% pull(n)
deported_count <- deportation %>% filter(is.na(deportation_alt)) %>% nrow()
mean_death_age <- erinnerte %>%
  filter(destiny_type_fac == "murdered",
         !is.na(destiny_when_year)) %>%
  mutate(death_age = destiny_when_year - geburtsjahr) %>%
  summarise(mean_age = mean(death_age, na.rm = TRUE)) %>%
  pull(mean_age) %>%
  round()




erinnerte_time_from_dep_to_murder <- erinnerte %>%
  filter(destiny_type_fac == "murdered" &
           !is.na(deportation_date_exact) &
           !is.na(destiny_when_date)) %>%
  mutate(time_dep_murder = destiny_when_date - deportation_date_exact)


# unequal_dep_dest <- erinnerte_time_from_dep_to_murder %>%
#   filter(as.character(dest_fac) != as.character(dep_fac))
unequal_dep_dest <- erinnerte %>%
  filter(destiny_type_fac == "murdered" &
           !is.na(dest_fac) & !is.na(dep_fac) &
           as.character(dest_fac) != as.character(dep_fac)
        )


my_test_func <- function(input_ding, input_berufe){
  berufe_df <- erinnerte %>%
    filter(!is.na(beruf_cat))
  if(input_ding != "Alle"){
    berufe_df <- berufe_df %>%
      filter(sex == input_ding)
  }
  berufe_df %>%
    #filter(!is.na(sex), !is.na(beruf_cat)) %>%
    mutate(beruf_fct = as_factor(beruf_cat) %>%
             fct_collapse("Arzt" = "Ärzt") %>%
             fct_infreq() %>%
             fct_rev() %>%
             fct_lump_n(input_berufe, other_level = "Andere")) %>%
    count(beruf_fct, sex) %>%
    rename(Beruf = beruf_fct, Geschlecht = sex, Anzahl = n) %>%
    ggplot(aes(Beruf, Anzahl, fill = Geschlecht)) +
    geom_col() +
    coord_flip() +
    #facet_grid(cols = vars(sex)) +
    labs(title = "Meistausgeübte Berufe nach Geschlecht", x = "", y = "") +
    theme(legend.position = "none")
}


