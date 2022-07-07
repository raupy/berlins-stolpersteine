# #### deportation ----

plot_deportated_where <- function() {
  erinnerte %>%
    filter(!is.na(dep_fac)) %>%
    mutate(dep_fac = fct_lump_min(dep_fac, min = 85) %>%
             fct_infreq() %>%
             fct_rev()
           ) %>%
    count(dep_fac) %>%
    rename(Anzahl = n, Ort = dep_fac) %>%
    ggplot(aes(Anzahl, Ort)) +
    geom_col() +
    labs(title = "KZs und Lager, in die deportiert wurde", x = "", y = "") +
    theme(legend.position = "none")
}



plot_deportated_when <- function() {
  erinnerte %>%
    filter(deportation_year >= 1937, deportation_year <= 1945) %>%
    count(deportation_year) %>%
    rename(Jahr = deportation_year, Anzahl = n) %>%
    ggplot(aes(Jahr, Anzahl)) +
    geom_col() +
    labs(title = "Deportationen pro Jahr", x = "", y = "") +
    theme(legend.position = "none")
}

plot_deportated_per_day <- function() {
  erinnerte %>%
    mutate(dep_fac = fct_lump_min(dep_fac, min = 1000, other_level = "Andere") %>%
             fct_infreq() %>%
             fct_rev() %>%
             fct_relevel("Andere") %>%
             fct_recode(Theresienst. = "Theresienstadt")
           ) %>%
    filter(deportation_date_exact >= lubridate::dmy("01.06.1941"),
           deportation_year <= 1945,
           !is.na(dep_fac)) %>%
    filter(!is.na(deportation_month)) %>%
    group_by(deportation_date_exact) %>%
    count(deportation_date_exact, dep_fac) %>%
    rename(Datum = deportation_date_exact,
           Ort = dep_fac,
           Anzahl = n) %>%
    ggplot(aes(Datum, Anzahl, color = Ort)) +
    geom_col() +
    facet_grid(rows = vars(Ort)) +
    labs(title = "Deportationen pro Tag", x = "", y = "") +
    theme(legend.position="none")
}


#
# deportation %>%
#   filter(deportation_year >= 1937, deportation_year <= 1945) %>%
#   filter(!is.na(deportation_month)) %>%
#   ggplot(aes(deportation_month)) + geom_bar()
#



#
#
#
# ## Plot schicksal ----
#

plot_murdered_where <- function() {
  erinnerte %>%
    filter(destiny_type_fac == "murdered",
           ) %>%
    filter(!is.na(dest_fac)) %>%
    mutate(dest_fac = fct_lump_min(dest_fac, min = 85) %>%
             fct_infreq() %>%
             fct_rev()
    ) %>%
    count(dest_fac) %>%
    rename(Anzahl = n, Ort = dest_fac) %>%
    ggplot(aes(Anzahl, Ort)) +
    geom_col() +
    labs(title = "Ort der Ermordung", x = "", y = "") +
    theme(legend.position = "none")
}



plot_murdered_when <- function() {
  erinnerte %>%
    filter(destiny_when_year >= 1933,
           destiny_when_year <= 1945,
           destiny_type_fac == "murdered",
           !is.na(dest_fac)) %>%
    count(destiny_when_year) %>%
    rename(Jahr = destiny_when_year, Anzahl = n) %>%
    ggplot(aes(Jahr, Anzahl)) +
    geom_col() +
    labs(title = "Ermordungen pro Jahr", x = "", y = "") +
    theme(legend.position = "none")
}




# destiny %>%
#   filter(destiny_type_fac == "murdered") %>%
#   plotly::plot_ly(x = ~ destiny_when_year) %>%
#   plotly::add_histogram()
#
# destiny %>%
#   count(destiny_loc_fac) %>%
#   plotly::plot_ly(x = ~n, y = ~destiny_loc_fac) %>%
#   plotly::add_bars()


## both ----









# erinnerte_time_from_dep_to_murder %>% count(dep_fac, sort = TRUE)
# erinnerte_time_from_dep_to_murder %>% count(dest_fac sort = TRUE)

