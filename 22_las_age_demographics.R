pacman::p_load(tidyverse)

all_ages <- read_csv("la_all_ages.csv",
                     na = c("-","NA"))

all_ages_lad <- all_ages %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ladu")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "total_pop"
  ) %>% 
  mutate(year = parse_double(year))

all_ages_lad_pre19 <- all_ages %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ualad19")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "total_pop"
  ) %>% 
  mutate(year = parse_double(year))


all_ages <- full_join(
  all_ages_lad,
  all_ages_lad_pre19,
  by = c("oslaua_code","year"),
  suffix = c("_post19","_pre19")
)

over_65 <- read_csv("la_aged_65plus.csv",
                     na = c("-","NA"))

over_65_lad <- over_65 %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ladu")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "over_65"
  ) %>% 
  mutate(year = parse_double(year))

over_65_lad_pre19 <- over_65 %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ualad19")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "over_65"
  ) %>% 
  mutate(year = parse_double(year))


over_65 <- full_join(
  over_65_lad,
  over_65_lad_pre19,
  by = c("oslaua_code","year"),
  suffix = c("_post19","_pre19")
)

under_15 <- read_csv("la_aged_0_to_15.csv",
                     na = c("-","NA"))

under_15_lad <- under_15 %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ladu")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "under_15"
  ) %>% 
  mutate(year = parse_double(year))

under_15_lad_pre19 <- under_15 %>% 
  rename(oslaua_code = mnemonic) %>%
  filter(str_detect(Area, "ualad19")) %>% 
  select(oslaua_code, one_of(as.character(seq(2016,2021,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = "year",
    values_to = "under_15"
  ) %>% 
  mutate(year = parse_double(year))


under_15 <- full_join(
  under_15_lad,
  under_15_lad_pre19,
  by = c("oslaua_code","year"),
  suffix = c("_post19","_pre19")
)

las_by_age <- all_ages %>% 
  left_join(over_65, by = c("oslaua_code","year")) %>% 
  left_join(under_15, by = c("oslaua_code","year")) %>% 
  mutate(
    over_65_pct_post19 = over_65_post19 / total_pop_post19,
    over_65_pct_pre19 = over_65_pre19 / total_pop_pre19,
    under_15_pct_post19 = under_15_post19 / total_pop_post19,
    under_15_pct_pre19 = under_15_pre19 / total_pop_pre19
  ) %>% 
  select(oslaua_code, year, contains("pct"))

save(las_by_age, file = "las_by_age.RData")
