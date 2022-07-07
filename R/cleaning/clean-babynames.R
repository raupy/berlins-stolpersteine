library(tidyverse)
library(rebus)


babynames <- readxl::read_excel("output/stolpersteine.xlsx") %>%
  mutate(Name = str_squish(Name),
         double_name = str_locate(Name, LOWER %R% optional(CLOSE_PAREN) %R% UPPER) %>%
           as_tibble() %>%
           pull(end),
         Name = ifelse(!is.na(double_name),
                       str_sub(Name, double_name),
                       Name),
         von = str_locate(Name,
                          or(optional(or("Graf", "Freiherr", "Ritter", "Freifrau", "Bontjes",
                                      "Baronin", "Mumm", "Meyer", "Liebermann")) %R%
                            or(" von", " van"),
                            UPPER %R% one_or_more(LOWER) %R% "," %R% " von")
                          ) %>%
           as_tibble() %>%
           pull(start)
         ) %>%
  select(-double_name) %>%
  bind_cols(read_tsv("output/bios_clean.tsv") %>% select(beruf))



test <- babynames %>%
  mutate(
         maidenname = str_locate(Name, OPEN_PAREN %R% "geb. ") %>%
           as_tibble() %>%
           pull(start),
         maidenname = ifelse(!is.na(maidenname),
                             str_sub(Name, maidenname),
                             NA),
         name = ifelse(!is.na(maidenname),
                       str_remove(Name, maidenname) %>%
                         str_remove(OPEN_PAREN %R% CLOSE_PAREN) %>%
                         str_trim(),
                       Name),
         maidenname = str_remove_all(maidenname, or(OPEN_PAREN, CLOSE_PAREN, "geb. ")),

         lastname = str_sub(name, von),
         lastname = ifelse(is.na(lastname),
                           str_extract(name, one_or_more(or(WRD, "-")) %R% END),
                           lastname),
         maidenname = ifelse(!is.na(maidenname) & maidenname == lastname,
                             NA,
                             maidenname),

         firstname = str_remove(name, lastname) %>%
           str_remove(or("auch", "genannt", "gen" %R% DOT)) %>%
           str_trim(),


         female = ifelse(str_detect(beruf, or("mann", "Arzt", "arzt",
                                              "nwalt", "Apotheker", "rbeiter", "Bäcker",
                                              "Chemiker", "Bankier", "Diplomat", "Jurist",
                                              "chneider", "chlosser", "beamter",
                                              "ngestellter", "vertreter", "Architekt", "Lehrer",
                                              "Notar", "Dreher", "Buchbinder", "ediziner",
                                              "Maler", "meister", "Rentner", "rat", "händler",
                                              "Journalist", "Klempner", "Mechaniker",
                                              "Fotograf", "Künstler", "Schriftsteller",
                                              "Unternehmer", "Schüler", "Jurist", "macher",
                                              "halter", "spieler", "or", "eur", "ändler",
                                              "ner", "är", "ler", "rer", "ant", "ist", "ger",
                                              "ter", "ker", "mer", "auer", "ier") %R% END),
                FALSE,
                NA),
         female = ifelse(is.na(female) & (!is.na(maidenname) |
                           str_detect(beruf, or("in", "frau", "schwester", "Diakonisse") %R% END)),
                         TRUE,
                         female)) %>%
  separate(firstname, paste0("middlename_", 0:4), sep = "[^a-zA-ZäöüÄÖÜß]", remove = FALSE) %>%
  mutate(across(starts_with("middlename"), ~ ifelse(.x == "" | startsWith(.x, LOWER) , NA, .x))) %>%
  select(-von)




get_names <- function(df) {
  names_vec <- c(df$middlename_0, df$middlename_1, df$middlename_2,
                 df$middlename_3, df$middlename_4)
  names_vec <- names_vec[!is.na(names_vec)]
  unique(names_vec)
}

get_names_from_firstname <- function(first_name, df) {
  name_df <- df %>% filter(firstname == first_name)
  get_names(name_df)
}



detect_sex <- function(first_name, df, female_names, male_names) {
  names_from_firstname <- get_names_from_firstname(first_name, df)
  female <- NA
  if (!is.na(first_name)) {
    if(sum(names_from_firstname %in% female_names) > 0 ) {
      female <- TRUE
    } else if (sum(names_from_firstname %in% male_names) > 0 ) {
      female <- FALSE
    }
  }
  female
}

mutate_sex <- function(test){
  female_names <- c(get_names(test %>% filter(female == TRUE)),
                              "Katalin", "Clothilde", "Georgine", "Elenore", "Ellinor",
                              "Liese", "Susanna", "Barbara", "Mary", "Hertha", "Perla",
                              "Judis", "Ursel", "Zilla", "Stephanie", "Tamara", "Cato",
                    "Marjanna",  "Mia",  "Helen", "Milda",     "Milly",  "Minni" ,    "Mira",
                    "Clarissa",  "Cora" ,     "Ela", "Camilla", "Rosemarie", "Sonia", "Norma",
                    "Alegrina",  "Amalia",    "Any", "Giesa", "Olena", "Lizzy", "Karla", "Netti",
                    "Waltraud", "Peppi", "Bärbel", "Colette"  , "Edeltraut", "Friedrike", "Liliane",
                    "Sigrid", "Noemi", "Louisette", "Jeanne", "Balbina",
                    "Evelyne", "Regi", "Jachaiwet")
  male_names <- c(get_names(test %>% filter(female == FALSE)),
                  "Dan", "Hanns", "Siegmar", "Stefan", "Freddie", "Charlie", "Levy", "Josua",
                  "Aladar",    "Alfried",   "Arnhold" ,  "Arvid" ,    "Avner"  ,   "Awigdor"  , "Benzion",
                  "Detmar"  ,  "Don"    ,   "Eberhard", "Guido" ,    "Haimann",   "Heinemann",
                  "Heymann"  , "Iro"  ,     "Isodor" ,   "Ivan"  ,    "Jonathan" ,
                  "Kajetan" ,  "Leser"  ,   "Liebmann" , "Maier"  ,   "Meinhard" , "Mordo"  ,   "Nachman" ,
                  "Oswald",    "Ottomar" ,  "Raimund" ,  "Rainer", "Axel",
                  "Reinhard",  "Rubin" ,    "Salomo" ,"Sigfried","Sigurd"  ,  "Stephan",   "Theo"  ,    "Valentin",
                  "Bezalel" ,  "Boris"  ,   "Charles"  , "Denis", "Tobias", "Uriel"  ,   "Yisroel",
                  "Hillel"  ,  "Jüdel" ,    "Judes" ,    "Manes"  ,   "Matthias" , "Stanislaw", "Leonhardt",
                  "Mechel"  ,  "Milius"  ,  "Moises"  ,  "Motel"  ,   "Rosenthal", "Scheindel", "Mordechai", "Rene",
                  "Nikolaj", "Ingolf", "Eike", "Egmont", "Hubert", "Ingolf", "Laib", "Morduch" ,  "Moschek", "Micha")
  test %>% mutate(female = ifelse(is.na(female),
                                          map_lgl(firstname, detect_sex, df = test, female_names, male_names),
                                          female))
}
test <- test %>%
  mutate_sex() %>%
  mutate_sex() %>%
  mutate_sex()

test <- test %>%
  mutate(birthname = if_else(!is.na(maidenname),
                            maidenname,
                            lastname)) %>%
  select(Name, beruf, female, name, lastname, birthname, maidenname, firstname,
         middlename_0, middlename_1, middlename_2, middlename_3, middlename_4,
         everything())

test <- test %>%
  mutate(sex = factor(as.numeric(female), labels = c("m", "w")))


write_tsv(test, "output/babynames.tsv")

test <- test %>%
  mutate()
## Dump----



babynames_work <- babynames %>%
  filter(str_detect(Name, LOWER %R% UPPER %R% LOWER))

clean_names <- babynames_work %>%
  bind_cols(str_locate(babynames_work$Name, LOWER %R% UPPER)
            %>% as_tibble()) %>%
  pmap_chr(function(Name, end, ...) str_sub(Name, end))
