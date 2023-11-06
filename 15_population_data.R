pacman::p_load(tidyverse, readxl)

rm(list = ls())

pop <- read_excel("population_data/ukpopestimatesmid2020on2021geography.xlsx",
                sheet = "MYE 5",
                range = "A8:AR428")

names(pop)

pop <- pop %>% 
  mutate(Code = str_trim(Code)) %>% 
  select(Code, Name, `2020 people per sq. km`,
         `2019 people per sq. km`, `2018 people per sq. km`,
         `2017 people per sq. km`, `2016 people per sq. km`,
         `2015 people per sq. km`, `2014 people per sq. km`)

names(pop) <- c("oslaua_code", "la_name",
                "2020", "2019", "2018","2017","2016","2015","2014")

pop_2021 <- read_excel("population_data/ukpopestimatesmid2021on2021geographyfinal.xlsx",
                       sheet = "MYE 5",
                       range = "A8:J428")

pop_2021 <- pop_2021 %>% 
  rename(oslaua_code = Code,
         `2021` = `2021 people per sq. km`) %>% 
  select(oslaua_code, `2021`)

pop <- pop %>% 
  left_join(pop_2021, by = "oslaua_code") %>%
  select(oslaua_code, la_name, `2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`) %>% 
  pivot_longer(`2014`:`2021`,
               names_to = "year",
               values_to = "pop_density") %>% 
  mutate(year = as.double(year))

save(pop, file = "pop.RData")
