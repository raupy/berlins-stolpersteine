library(tidyverse)
library(rebus)

berufe <- read_tsv("output/bios_clean.tsv") %>% select(beruf)

berufe_count <- berufe %>%
  mutate(beruf = ifelse(str_detect(beruf, OPEN_BRACKET),
                         str_replace(beruf, "Schuhmacher " %R% OPEN_BRACKET %R% "1935-1941" %R% CLOSE_BRACKET, "Schuhmacher"),
                         beruf)) %>%
  count(beruf) %>%
  filter(!is.na(beruf))

berufe_regex <- berufe_count %>%
  fuzzyjoin::regex_inner_join(berufe_count, by = "beruf") %>%
  group_by(beruf.x) %>%
  mutate(collapse = beruf.y[which.max(n.y)]) %>%
  ungroup() %>%
  select(beruf.x, collapse, n.y) %>%
  rename(beruf = beruf.x, n = n.y)

berufe_fuzzy <- berufe_regex %>%
  fuzzyjoin::stringdist_inner_join(berufe_regex, by = "collapse", max_dist = 2) %>%
  group_by(beruf.x) %>%
  mutate(beruf_cat = collapse.y[which.max(n.y)]) %>%
  ungroup() %>%
  select(beruf.x, beruf_cat) %>%
  rename(beruf = beruf.x) %>%
  distinct()

berufe_count <- berufe_count %>%
  left_join(berufe_fuzzy, by = "beruf") %>%
  mutate(beruf_cat = ifelse(is.na(beruf_cat),
                           beruf,
                           beruf_cat)) %>%
  select(beruf, beruf_cat)

berufe <- berufe %>%
  left_join(berufe_count, by = "beruf") %>%
  mutate(beruf_cat = str_remove(beruf_cat, "in" %R% END))

write_tsv(berufe, "output/berufe.tsv")

