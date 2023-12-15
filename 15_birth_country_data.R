bc_2016 <- read_csv("Birth country data/birth_country_2016.csv",
                    na = c(":",""))
map_chr(bc_2016, class)

bc_2017 <- read_csv("Birth country data/birth_country_2017.csv",
                    na = c(":",""))
map_chr(bc_2017, class)

bc_2018 <- read_csv("Birth country data/birth_country_2018.csv",
                    na = c(":",""))
map_chr(bc_2018, class)

bc_2019 <- read_csv("Birth country data/birth_country_2019.csv",
                    na = c(":",""))
map_chr(bc_2019, class)

bc_2020 <- read_csv("Birth country data/birth_country_2020.csv",
                    na = c(":",""))
map_chr(bc_2020, class)

bc_clean <- function(df, year_dbl){
  
  out <- df %>% 
    rename(
      oslaua_code = 1,
      total_pop = `All estimate`,
      uk_pop = `United Kingdom estimate`
    ) %>% 
    mutate(
      total_pop = total_pop * 1000,
      uk_pop = uk_pop * 1000,
      foreign_pop = total_pop - uk_pop,
      pop_1000 = total_pop / 1000,
      foreign_per_1000 = foreign_pop / pop_1000,
      year = year_dbl
    ) %>% 
    select(oslaua_code, year, total_pop, uk_pop, pop_1000, foreign_pop, foreign_per_1000)
  return(out)

  }

bc_2016 <- bc_clean(bc_2016, 2016)
bc_2017 <- bc_clean(bc_2017, 2017)
bc_2018 <- bc_clean(bc_2018, 2018)
bc_2019 <- bc_clean(bc_2019, 2019)
bc_2020 <- bc_clean(bc_2020, 2020)

bc_2021 <- read_csv("Birth country data/birth_country_2021.csv",
                    na = c("Not Available",""))
bc_2021 %>% map_chr(class)
bc_2021 <- bc_2021 %>% 
  rename(oslaua_code = `Area Code`,
         total_pop = `All\nEstimate`,
         uk_pop = `United Kingdom\nEstimate`) %>% 
  mutate(
    foreign_pop = total_pop - uk_pop,
    pop_1000 = total_pop / 1000,
    foreign_per_1000 = foreign_pop / pop_1000,
    year = 2021) %>% 
  select(oslaua_code, year, total_pop, uk_pop, pop_1000, foreign_pop, foreign_per_1000)

bc <- bind_rows(bc_2016, bc_2017, bc_2018, bc_2019, bc_2020, bc_2021) %>% 
  arrange(oslaua_code, year)

old_codes <- read_csv("Birth country data/old_la_codes.csv") %>% 
  select(oslaua_code, old_code) %>% 
  rename(new_code = oslaua_code)

old_codes <- old_codes %>% 
  left_join(bc, by = c("old_code" = "oslaua_code")) %>% 
  group_by(new_code, year) %>% 
  summarise(total_pop = sum(total_pop),
            uk_pop = sum(uk_pop),
            .groups = "drop") %>% 
  mutate(pop_1000 = total_pop / 1000,
         foreign_pop = total_pop - uk_pop,
         foreign_per_1000 = foreign_pop / pop_1000) %>% 
  rename(oslaua_code = new_code)

bc <- bc %>%
  bind_rows(old_codes)

save(bc, file = "bc.RData")
