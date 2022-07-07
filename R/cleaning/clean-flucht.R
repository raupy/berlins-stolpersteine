clean_flucht <- function(flucht_df) {
  german_months <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                     "Juli", "August", "September", "Oktober", "November", "Dezember")

  or_test <- or(
    one_or_more(DGT) %R% DOT %R% one_or_more(DGT) %R% DOT %R% repeated(DGT, lo = 4, hi = 4), # dd.mm.yyyy
    one_or_more(DGT) %R% optional(DOT) %R% SPACE %R% or1(german_months) %R% SPACE %R% repeated(DGT, lo = 4, hi = 4), # dd. MM yyyy
    or1(german_months) %R% SPACE %R% repeated(DGT, lo = 4, hi = 4), # MM yyyy
    one_or_more(DGT) %R% "/" %R% repeated(DGT, lo = 4, hi = 4), # dd/yyyy
    repeated(DGT, lo = 4, hi = 4)
  )

  flucht_df <- flucht_df %>%
    mutate(flucht_alt = flucht,
           datum = str_extract(flucht, or_test),
           fluchtjahr = str_extract(datum, repeated(DGT, lo = 4, hi = 4)),
           flucht = str_remove(flucht, or_test) %>% str_trim())
  flucht_df
}

flucht <- stolpis %>%
  select(Name, flucht, Gebjahr) %>%
  filter(!is.na(flucht))

flucht <- clean_flucht(flucht)

flucht_sub <- flucht %>%
  filter(str_detect(flucht, repeated(DGT, lo = 4, hi = 4))) %>%
  mutate(flucht = str_remove_all(flucht, fixed("Flucht")),
         daten = str_extract_all(flucht, or_test),
         length_daten = sapply(daten, length),
         my_sep = sapply(daten, or1),
         my_split = map2(flucht, my_sep, ~ str_split(.x, .y)) )



flucht_sub_stationen <- flucht_sub %>%
  select(Name, Gebjahr, my_split, daten, flucht) %>%
  rename(name = Name, stationen = my_split) %>%
  pmap(function(name, stationen, daten, flucht, Gebjahr) data.frame(name = name,
                                                                    Gebjahr = Gebjahr,
                                                                    zeitpunkt = c(daten[1], daten),
                                                                    length_stationen = length(stationen[[1]]),
                                                                    stationen = stationen[[1]],
                                                                    flucht = flucht)
  ) %>%
  rbindlist() %>%
  as_tibble() %>%
  mutate(stationen = str_remove_all(stationen, or("nach", "in die", "- ", " -")) %>% str_trim())


emptys <- flucht_sub_stationen %>% filter(stationen == "") %>%
  group_by(name, Gebjahr) %>%
  mutate(dupe = n()>1) %>%
  ungroup() %>%
  filter(dupe == TRUE) %>%
  distinct() %>%
  mutate(stationen = NA)

final <- flucht_sub_stationen %>%
  filter(stationen != "") %>%
  bind_rows(emptys)


flucht_date_simple <- final %>%
  filter(str_detect(stationen, START %R% one_or_more(WRD) %R% END))

flucht_date_complicated <- final %>%
  filter(!str_detect(stationen, START %R% one_or_more(WRD) %R% END)) %>%
  mutate(stationen = str_remove(stationen, or(NOT_WRD, "und", "und ab", "am", "im", "ausgewandert") %R% END),
         stationen = str_remove(stationen, START %R% or(NOT_WRD, "Auswanderung", "Umzug", "Emigration", "Fluch", "FLucht", "in", "über", "dann")),
         stationen = str_trim(stationen))

flucht_date_simple <- flucht_date_simple %>%
  bind_rows(flucht_date_complicated %>% filter(str_detect(stationen, START %R% one_or_more(WRD) %R% END)))

flucht_date_complicated <- flucht_date_complicated %>%
  filter(!str_detect(stationen, START %R% one_or_more(WRD) %R% END)) %>%
  separate(stationen,
           into = c("a", "b", "c", "d"),
           sep = or("über", ", dann", ", weiter", ", später", ", anschließend", ",", "und anschließend", "und", "/"),
           remove = FALSE) %>%
  pivot_longer(a:d, names_to = "nr", values_to = "stationen_neu") %>%
  mutate(stationen_neu = str_trim(stationen_neu),
         stationen_neu = str_remove(stationen_neu, START %R% or("die ", "dann ")),
         stationen_neu = str_replace(stationen_neu, "BELGIEN", "Belgien"),
         stationen_neu = str_replace(stationen_neu, "ITALIEN", "Italien"),
         stationen_neu = str_replace(stationen_neu, "NL", "Niederlande"),
         extra = str_extract(stationen_neu, or("Kindertransport", "Kladovo-Transport")),
         stationen_neu = str_remove(stationen_neu, NOT_WRD %R% END),
         stationen_neu = str_remove(stationen_neu, START %R% or("per", "ab")),
         stationen_neu = str_remove(stationen_neu, or("Kindertransport", "Kladovo-Transport")) %>% str_trim())


## kein jahr
flucht_no_date <- flucht %>%
  filter(!str_detect(flucht, repeated(DGT, lo = 4, hi = 4))) %>%
  mutate(flucht = str_remove_all(flucht, or("Flucht", "nach")) %>% str_trim())

flucht_no_date_simple <- flucht_no_date %>%
  filter(str_detect(flucht, START %R% one_or_more(WRD) %R% END))

flucht_no_date_complicated <- flucht_no_date %>%
  filter(!str_detect(flucht, START %R% one_or_more(WRD) %R% END)) %>%
  mutate(aus = str_extract(flucht, "aus" %R% SPACE %R% one_or_more(WRD)) %>%
           str_remove("aus "),
         flucht = str_remove(flucht,  "aus" %R% SPACE %R% one_or_more(WRD) %R% optional(",") %R% SPACE),
         flucht = str_remove(flucht, "in die "))

flucht_no_date_simple <- flucht_no_date_simple %>%
  bind_rows(flucht_no_date_complicated %>% filter(str_detect(flucht, START %R% one_or_more(WRD) %R% END)))

flucht_no_date_complicated <- flucht_no_date_complicated %>%
  filter(!str_detect(flucht, START %R% one_or_more(WRD) %R% END)) %>%
  separate(flucht, into = c("a", "b"), sep = or(",", "über die", "und"), remove = FALSE) %>%
  pivot_longer(a:b, names_to = "nr", values_to = "stationen") %>%
  mutate(stationen = str_squish(stationen))

flucht_no_date_complicated[5, "stationen"] = "Wien"
flucht_no_date_complicated[6, "stationen"] = "Paris"

flucht_no_date_complicated <- flucht_no_date_complicated %>%
  filter(!is.na(stationen) & stationen != "") %>%
  select(-nr)

flucht_no_date_final <- flucht_no_date_complicated %>%
  bind_rows(flucht_no_date_simple)
