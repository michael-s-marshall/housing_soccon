pacman::p_load(readxl)

# wales --------------------------------------------------------------------

wales_2021 <- read_csv(
  "wales_tenure_data/wales_tenure_2021.csv",
  skip = 6
)

wales_2020 <- read_csv(
  "wales_tenure_data/wales_tenure_2020.csv",
  skip = 6
)

wales_2019 <- read_csv(
  "wales_tenure_data/wales_tenure_2019.csv",
  skip = 6
)

wales_2018 <- read_csv(
  "wales_tenure_data/wales_tenure_2018.csv",
  skip = 6
)

wales_2017 <- read_csv(
  "wales_tenure_data/wales_tenure_2017.csv",
  skip = 6
)

wales_2016 <- read_csv(
  "wales_tenure_data/wales_tenure_2016.csv",
  skip = 6
)

wales_clean <- function(df, year_num){
  
  df <- df %>% 
    rename(la_name = `...3`) %>% 
    select(la_name:`Local Authority (%)`) %>% 
    filter(!is.na(la_name)) %>% 
    mutate(`Local Authority (%)` = ifelse(
      `Local Authority (%)` == ".", 0, `Local Authority (%)`) %>% 
        parse_double,
      la_name = str_remove_all(la_name, "[:digit:]|[:punct:]") %>% str_trim
    )
  
  names(df) <- c("la_name", "rsl_perc", "prs_perc",
                 "own_occ_perc", "la_perc")
  
  df <- df %>% 
    mutate(soc_hous_perc = la_perc + rsl_perc,
           year = year_num)
  
  return(df)
}

wales_2021 <- wales_clean(wales_2021, 2021)
wales_2020 <- wales_clean(wales_2020, 2020)
wales_2019 <- wales_clean(wales_2019, 2019)
wales_2018 <- wales_clean(wales_2018, 2018)
wales_2017 <- wales_clean(wales_2017, 2017)
wales_2016 <- wales_clean(wales_2016, 2016)

wales_tenure <- bind_rows(wales_2021, wales_2020, wales_2019, 
          wales_2018, wales_2017, wales_2016) %>% 
  select(-rsl_perc, -la_perc)

rm(wales_2021, wales_2020, wales_2019, wales_2018, wales_2017, wales_2016, 
   wales_clean)

# england ----------------------------------------------------------------------

eng_tenure <- read_excel("subnationaldwellingsbytenure2021.xlsx",
                         sheet = "2b",
                         range = "C4:AR312")

eng_tenure <- eng_tenure %>% 
  rename(la_code = `Local authority code`) %>% 
  select(-`Local authority name`)

names(eng_tenure) <- names(eng_tenure) %>% 
  str_to_lower() %>% 
  str_remove_all("[:punct:]") %>% 
  str_trim()

eng_tenure <- eng_tenure %>% 
  pivot_longer(
    `2012 owned outright`:`2021 social rent`,
    names_to = c("year", "tenure"),
    values_to = "tenure_perc",
    names_sep = 5
  ) %>%  
  pivot_wider(names_from = "tenure", values_from = "tenure_perc") %>%
  mutate(year = parse_double(year),
         own_occ_perc = `owned outright` + `owned with mortgage or loan`) %>%
  filter(year %in% seq(2016, 2021, 1)) %>% 
  rename(
    oslaua_code = lacode,
    prs_perc = `private rent`,
    soc_hous_perc = `social rent`
  ) %>%
  select(-`owned outright`, -`owned with mortgage or loan`)

save(wales_tenure, file = "wales_tenure.RData")
save(eng_tenure, file = "eng_tenure.RData")

