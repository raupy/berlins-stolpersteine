

plot_vornamen_sex <- function(input_select_sex, input_slider_n) {
  babynames_subset <- babynames %>%
    filter(!is.na(middlename_0))
  if(input_select_sex != "Alle"){
    babynames_subset <- babynames_subset %>%
      filter(sex == input_select_sex)
  }
  babynames_subset %>%
    count(middlename_0, sex, sort = TRUE) %>%
    head(input_slider_n) %>%
    mutate(Vorname = as_factor(middlename_0) %>%
             fct_infreq() %>%
             fct_rev()) %>%
    rename(Geschlecht = sex, Anzahl = n) %>%
    ggplot(aes(Vorname, Anzahl, fill = Geschlecht)) +
    geom_col() +
    coord_flip() +
    #facet_grid(cols = vars(sex)) +
    labs(title = "Häufigste Vornamen nach Geschlecht", x = "", y = "") +
    theme(legend.position = "none")
}


plot_nachnamen <- function(input_nachnamen_oder_geburtsname, input_slider_n) {
  if(input_nachnamen_oder_geburtsname == "birthname"){
    babynames_subset <- babynames %>%
      filter(!is.na(birthname)) %>%
      count(birthname, sort = TRUE) %>%
      head(input_slider_n) %>%
      mutate(Geburtsname = as_factor(birthname) %>%
               fct_infreq() %>%
               fct_rev())
  } else {
    babynames_subset <- babynames %>%
      filter(!is.na(lastname)) %>%
      count(lastname, sort = TRUE) %>%
      head(input_slider_n) %>%
      mutate(Geburtsname = as_factor(lastname) %>%
               fct_infreq() %>%
               fct_rev())
  }
  babynames_subset %>%
    rename(Anzahl = n) %>%
    ggplot(aes(Geburtsname, Anzahl)) +
    geom_col() +
    coord_flip() +
    #facet_grid(cols = vars(sex)) +
    labs(title = "Häufigste Nachnamen", x = "", y = "") +
    theme(legend.position = "none")
}
