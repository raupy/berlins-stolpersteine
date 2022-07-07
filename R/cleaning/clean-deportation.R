library(tidyverse)
library(rebus)

bios_clean <- read_tsv("output/bios_clean.tsv")

deportation <- bios_clean %>%
  select(deportation)

german_months <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                   "Juli", "August", "September", "Oktober", "November", "Dezember")
months_abb <- str_sub(german_months, end = 3) %>%
  str_replace("ä", "a") %>%
  str_replace("i", "y") %>%
  str_replace("k", "c") %>%
  str_replace("z", "c")

replace_german_month_with_english_abb <- function(string) {
  detected <- str_detect(string, german_months)
  if (sum(detected, na.rm = TRUE) == 1) {
    i <- which(detected)
    str_replace(string, german_months[i], months_abb[i])
  } else {
    string
  }
}

clean_deportation <- function(deportation_df) {
  or_test <- or(
    one_or_more(DGT) %R% DOT %R% one_or_more(DGT) %R% DOT %R% repeated(DGT, lo = 2, hi = 4), # dd.mm.yyyy
    one_or_more(DGT) %R% optional(DOT) %R% SPACE %R% or1(german_months) %R% SPACE %R% repeated(DGT, lo = 4, hi = 4), # dd. MM yyyy
    or1(german_months) %R% SPACE %R% repeated(DGT, lo = 4, hi = 4), # MM yyyy
    one_or_more(DGT) %R% "/" %R% repeated(DGT, lo = 4, hi = 4), # dd/yyyy
    repeated(DGT, lo = 4, hi = 4)
  )

  deportation_df %>%
    mutate(deportation_alt = deportation,
           dep_datum = str_extract(deportation, or_test),
           dep_date = ifelse(str_detect(dep_datum, or1(german_months)),
                          map_chr(dep_datum, ~ replace_german_month_with_english_abb(.x)),
                          dep_datum),

           deportation_date_exact = dmy(dep_datum),
           deportation_my = parse_date_time(dep_date, orders = c("dmy", "bY")) %>% date(),
           deportation_month = month(deportation_my) %>%
             factor(levels = 1:12, labels = months_abb),
           deportation_year = str_extract(dep_datum, repeated(DGT, lo = 4, hi = 4)) %>%
             as.numeric(),
           deportation = str_remove(deportation, or("am ", "im Jahre ", "im ") %R% dep_datum) %>% # %R% optional(or(" nach ", " in das "))) %>%
             str_trim()) %>%
    extract(deportation, into = c("von", "nach"), regex = "von " %R% "(.*)" %R% or("nach ", "in das ") %R% "(.*)", remove = FALSE)  %>% #or1(c("von ", "nach ", "in das "))
    #separate(deportation, into = c("von", "nach"), sep = "von " %|% "nach " %|% "in das ", remove = FALSE) #or1(c("von ", "nach ", "in das "))
    mutate(nach = ifelse(is.na(nach) & str_detect(deportation, or("nach ", "in das ")),
                         str_remove(deportation, or("nach ", "in das ")),
                         nach),
           von = ifelse(is.na(von) & str_detect(deportation, "von "),
                        str_remove(deportation, "von "),
                        von))
}

deportation2 <- deportation %>% clean_deportation()




#### clean nach ----

deportation2 %>%
  filter(!is.na(nach)) %>%
  count(nach, sort = TRUE)


deport_nach_collapse <- deportation2 %>%
  filter(!is.na(nach)) %>%
  count(nach) %>%
  fuzzyjoin::regex_inner_join(deportation2 %>% filter(!is.na(nach)) %>% count(nach), by = "nach") %>%
  group_by(nach.x) %>%
  mutate(nach_collapse = nach.y[which.max(n.y)]) %>%
  ungroup() %>%
  select(nach.x, nach_collapse) %>%
  rename(nach_org = nach.x) %>%
  distinct()


deport_nach_test <- deportation2 %>%
  filter(!is.na(nach)) %>%
  count(nach) %>%
  inner_join(deport_nach_collapse, by = c("nach" = "nach_org")) %>%
  fuzzyjoin::stringdist_inner_join(deport_nach_collapse, by = "nach_collapse")

deport_nach_test <- deport_nach_collapse %>%
  fuzzyjoin::stringdist_inner_join(deport_nach_collapse, by = "nach_collapse") %>%
  select(-nach_org.x) %>%
  left_join(deportation2 %>%
              filter(!is.na(nach)) %>%
              count(nach),
            by = c("nach_collapse.y" = "nach")) %>%
  group_by(nach_collapse.x) %>%
  mutate(nach_collapse_string_dist = nach_collapse.y[which.max(n)]) %>%
  ungroup() %>%
  distinct() %>%
  select(nach_org.y, nach_collapse_string_dist) %>%
  distinct()

deport_nach_test <- deportation2 %>%
  select(nach) %>%
  left_join(deport_nach_test, by = c("nach" = "nach_org.y")) %>%
  rename(nach_og = nach) %>%
  mutate(nach_fct = as_factor(nach_collapse_string_dist) %>%
           fct_collapse("Kowno / Kaunas" = c("Kowno", "Kowno Fort IX"),
                        "Raasiku" = c("Raasiku (b. Reval)"),
                        "Łódź / Litzmannstadt" = c("Litzmannstadt",
                                                   "Litzmannstadt / Łódź",
                                                   "Lodz / Litzmannstadt",
                                                   "Lodz",
                                                   "Ghetto Lodz / Lizmannstadt"),
                        "Bernburg/Saale" = c("Bernburg"),
                        "Unknown" = c("unbekannt", "unbekannten Deportationsort")) %>%
           fct_infreq()) %>%
  select(nach_og, nach_fct) %>%
  distinct()
  #bind_cols(deportation2)

deportation2 <- deportation2 %>%
  left_join(deport_nach_test, by = c("nach" = "nach_og")) %>%
  select(deportation, von, nach, nach_fct, everything())


write_tsv(deportation2, "output/deportation.tsv")





