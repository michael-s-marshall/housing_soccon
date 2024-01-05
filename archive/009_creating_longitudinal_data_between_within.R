################################################################
# creating longitudinal dataset -------------------------------
################################################################

pacman::p_load(tidyverse, haven)

rm(list = ls())

# scaling function ------------------------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

# datasets -------------------------------------------------------------------
# w10 nov-dec 2016
# w11 april-may 2017
# w14 may 2018
# w17 nov 2019
# w20 jun 2020
# w22 nov-dec 2021

wave_year <- seq(2016, 2021, 1)
data_files <- c("BES2015_W10_v24.0.dta",
                "BES2015_W11_v24.0.dta",
                "BES2017_W14_v24.0.dta",
                "BES2019_W17_v24.0.dta",
                "BES2019_W20_v24.0.dta",
                "BES2019_W22_v24.0.dta")

file_path <- "panel_data/"
vars <- c("id", "immigSelf", "redistSelf", "p_edlevel", 
          "p_ethnicity", "p_religion", "p_socgrade", 
          "p_country_birth", "p_gross_household",
          "p_housing", "age", "oslaua_code","gor","year")

w10 <- read_dta(file = str_c(file_path, data_files[1]))
w10$year <- wave_year[1]
w10 <- w10 %>% 
  select(all_of(vars))

w11 <- read_dta(file = str_c(file_path, data_files[2]))
w11$year <- wave_year[2]
w11 <- w11 %>% 
  select(all_of(vars))

w14 <- read_dta(file = str_c(file_path, data_files[3]))
w14$year <- wave_year[3]
w14 <- w14 %>% 
  select(all_of(vars))

w17 <- read_dta(file = str_c(file_path, data_files[4]))
w17$year <- wave_year[4]
w17 <- w17 %>% 
  select(all_of(vars))

w20 <- read_dta(file = str_c(file_path, data_files[5]))
w20$year <- wave_year[5]
w20 <- w20 %>% 
  select(all_of(vars))

w22 <- read_dta(file = str_c(file_path, data_files[6]))
w22$year <- wave_year[6]
w22 <- w22 %>% 
  select(all_of(vars))

df <- bind_rows(w10, w11, w14, w17, w20, w22)

rm(w10, w11, w14, w17, w20, w22)

# filtering out those without la -------------------------------------------------------

df <- df %>% filter(oslaua_code!= "")

# people who moved -------------------------------------------------------------------

# filters for presence in each wave
ids_2016 <- df$id[df$year == 2016]
ids_2017 <- df$id[df$year == 2017]
ids_2018 <- df$id[df$year == 2018]
ids_2019 <- df$id[df$year == 2019]
ids_2020 <- df$id[df$year == 2020]
ids_2021 <- df$id[df$year == 2021]

# people in all at least three waves
df <- df %>% 
  mutate(
    in_2016 = ifelse(id %in% ids_2016, 1, 0),
    in_2017 = ifelse(id %in% ids_2017, 1, 0),
    in_2018 = ifelse(id %in% ids_2018, 1, 0),
    in_2019 = ifelse(id %in% ids_2019, 1, 0),
    in_2020 = ifelse(id %in% ids_2020, 1, 0),
    in_2021 = ifelse(id %in% ids_2021, 1, 0)
  ) %>% 
  mutate(wave_n = select(., in_2016:in_2021) %>%  rowSums(na.rm = T),
         gor = as_factor(gor)) %>% 
  arrange(id, year)

# affordability ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

year_range <- as.character(seq(2016,2021,1))
afford <- afford %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "affordability") %>% 
  group_by(oslaua_code) %>% 
  mutate(year = as.double(year),
         affordability_log = log(affordability),
         affordability_mean = mean(affordability, na.rm = T),
         affordability_within = affordability - affordability_mean,
         affordability_log_mean = mean(affordability_log, na.rm = T),
         affordability_log_within = affordability_log - affordability_log_mean) %>% 
  ungroup()

prices <- read_csv("median_house_prices.csv")
names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "prices") %>% 
  group_by(oslaua_code) %>%
  mutate(year = as.double(year),
         prices = log(prices),
         prices_mean = mean(prices, na.rm = T),
         prices_within = prices - prices_mean) %>% 
  ungroup(oslaua_code) 

df <- df %>% 
  left_join(afford, by = c("oslaua_code","year")) %>% 
  left_join(prices, by = c("oslaua_code","year"))

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp <- gdp %>% 
  rename(oslaua_code = `LA code`) %>%
  pivot_longer(
    cols = all_of(year_range),
    names_to = "year",
    values_to = "gdp_capita"
  ) %>%
  group_by(oslaua_code) %>% 
  mutate(year = parse_double(year),
         gdp_capita_mean = mean(gdp_capita, na.rm = T),
         gdp_capita_within = gdp_capita - gdp_capita_mean) %>%
  ungroup() %>% 
  select(oslaua_code, year, gdp_capita, gdp_capita_mean, gdp_capita_within)

df <- df %>% 
  left_join(gdp, by = c("oslaua_code", "year"))

# population data -----------------------------------------------------------

load("pop.RData")

pop <- pop %>%
  filter(year != 2014 & year != 2015) %>% 
  select(-la_name) %>% 
  group_by(oslaua_code) %>% 
  mutate(pop_density_mean = mean(pop_density, na.rm = T),
         pop_density_within = pop_density - pop_density_mean) %>% 
  ungroup()
  
df <- df %>% 
  left_join(pop, by = c("oslaua_code", "year"))

# birth country data -----------------------------------------------------------

load("bc.RData")

bc <- bc %>% 
  select(oslaua_code, year, foreign_per_1000) %>% 
  group_by(oslaua_code) %>% 
  mutate(foreign_per_1000_mean = mean(foreign_per_1000, na.rm = T),
         foreign_per_1000_within = foreign_per_1000 - foreign_per_1000_mean) %>% 
  ungroup()

df <- df %>% left_join(bc, by = c("oslaua_code","year"))

# age data -------------------------------------------------------------

load("las_by_age.RData")

df <- df %>% 
  left_join(las_by_age, by = c("oslaua_code","year"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19) %>% 
  group_by(oslaua_code) %>% 
  mutate(over_65_pct_mean = mean(over_65_pct, na.rm = T),
         over_65_pct_within = over_65_pct - over_65_pct_mean,
         under_15_pct_mean = mean(under_15_pct, na.rm = T),
         under_15_pct_within = under_15_pct - under_15_pct_mean) %>% 
  ungroup()

## education data -------------------------------------------------------

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(oslaua_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(oslaua_code, degree_pct)

edu_2011 <- read_csv("education_census_2011.csv")

edu_2011 <- edu_2011 %>% 
  rename(oslaua_code = 1,
         degree_pct = 8) %>% 
  select(oslaua_code, degree_pct)

edu <- edu %>% 
  left_join(
    edu_2011, by = "oslaua_code", suffix = c("2021","2011")
  ) %>% 
  mutate(degree_pct_change = degree_pct2021 - degree_pct2011) %>% 
  rename(degree_pct = degree_pct2021) %>% 
  select(-degree_pct2011)

df <- df %>% 
  left_join(edu, by = "oslaua_code")

# percentage manufacturing employment --------------------------------------

indus_2016 <- read_csv("2016_industry_employment.csv")

indus_clean <- function(df, year_dbl){
  out <- df %>% 
    filter(str_detect(Area,"ladu")) %>% 
    rename(oslaua_code = mnemonic,
           manufacturing = 5) %>% 
    select(oslaua_code:23) %>% 
    pivot_longer(
      cols = 2:22,
      names_to = "industry",
      values_to = "employment"
    ) %>% 
    group_by(oslaua_code) %>% 
    mutate(total_employment = sum(employment),
           manuf_pct = employment / total_employment,
           year = year_dbl) %>% 
    ungroup() %>% 
    filter(industry == "manufacturing")
  return(out)
}

indus_2016 <- indus_clean(indus_2016, 2016)

indus_2017 <- read_csv("2017_industry_employment.csv")
indus_2017 <- indus_clean(indus_2017, 2017)

indus_2018 <- read_csv("2018_industry_employment.csv")
indus_2018 <- indus_clean(indus_2018, 2018)

indus_2019 <- read_csv("2019_industry_employment.csv")
indus_2019 <- indus_clean(indus_2019, 2019)

indus_2020 <- read_csv("2020_industry_employment.csv")
indus_2020 <- indus_clean(indus_2020, 2020)

indus_2021 <- read_csv("2021_industry_employment.csv")
indus_2021 <- indus_clean(indus_2021, 2021)

manuf_pct <- bind_rows(
  indus_2016, indus_2017, indus_2018,
  indus_2019, indus_2020, indus_2021
  ) %>% 
  select(oslaua_code, manuf_pct, year) %>% 
  group_by(oslaua_code) %>% 
  mutate(manuf_pct_mean = mean(manuf_pct, na.rm = T),
         manuf_pct_within = manuf_pct - manuf_pct_mean) %>% 
  ungroup()

df <- df %>% 
  left_join(manuf_pct, by = c("oslaua_code","year"))

rm(indus_2016, indus_2017, indus_2018, indus_2019, indus_2020, indus_2021)

## centering, scaling and between-within calculations LA vars -------------------

# centering year
df <- df %>% 
  mutate(year_c = year - 2016)

scaling_vars <- df %>% 
  select(affordability:manuf_pct_within) %>% 
  names()

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

df[scaling_vars] <- df[scaling_vars] %>%
  map_df(scale_this)

## creating person variables ------------------------------------------------

df$uni <- ifelse(
  df$p_edlevel == 4|df$p_edlevel == 5, 1, 0
)

df$white <- ifelse(
  df$p_ethnicity == 1|df$p_ethnicity == 2, 1, 0
)

df$white[df$p_ethnicity == 16] <- NA

df$no_religion <- ifelse(
  df$p_religion == 1, 1, 0
)

df$c1_c2 <- ifelse(
  df$p_socgrade == 3 | df$p_socgrade == 4, 1, 0
)
df$c1_c2[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df$d_e <- ifelse(
  df$p_socgrade == 5 | df$p_socgrade == 6, 1, 0
)
df$d_e[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df$non_uk_born <- ifelse(
  df$p_country_birth != 1, 1, 0
)
df$non_uk_born[df$p_country_birth == 9999] <- NA

df$own_outright <- ifelse(
  df$p_housing == 1, 1, 0
)

df$own_mortgage <- ifelse(
  df$p_housing == 2, 1, 0
)

df$homeowner <- ifelse(
  df$p_housing == 1|df$p_housing == 2, 1, 0
)

df$private_renting <- ifelse(
  df$p_housing == 4, 1, 0
)

df$social_housing <- ifelse(
  df$p_housing == 5|df$p_housing == 6, 1, 0
)

# scaling age
df$age <- scale_this(df$age)

df %>% 
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T))

# removing don't knows -------------------------------------------------------

df %>% 
  count(immigSelf)

df$immigSelf[df$immigSelf == 9999] <- NA

df %>% 
  count(immigSelf)

# reordering immigSelf -----------------------------------

df <- df %>%
  mutate(
    immigpro = immigSelf,
    immigSelf = 10 - immigpro
  )

df %>% count(immigSelf, immigpro)

df$id <- as.factor(df$id)

# selecting vars and saving ---------------------------------------------

# producing dataset
immig_df <- df %>% 
  select(immigSelf, all_of(scaling_vars), uni, white, no_religion, 
         c1_c2, d_e, non_uk_born, homeowner, private_renting, 
         social_housing, year_c, oslaua_code, id) 

names(immig_df)

save(immig_df, file = "longitudinal_df.RData")


