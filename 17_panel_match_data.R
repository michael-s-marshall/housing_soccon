pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, gghighlight, survey,
               srvyr)

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

file_path <- "U:/housing_soccon/panel_data/"
vars <- c("id", "immigSelf", "redistSelf", "p_edlevel", 
          "p_ethnicity", "p_religion", "p_socgrade", 
          "p_country_birth", "p_gross_household",
          "p_housing", "age", "oslaua_code", "wt", "year")

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
three_waves <- df %>% 
  mutate(
    in_2016 = ifelse(id %in% ids_2016, 1, 0),
    in_2017 = ifelse(id %in% ids_2017, 1, 0),
    in_2018 = ifelse(id %in% ids_2018, 1, 0),
    in_2019 = ifelse(id %in% ids_2019, 1, 0),
    in_2020 = ifelse(id %in% ids_2020, 1, 0),
    in_2021 = ifelse(id %in% ids_2021, 1, 0)
  ) %>% 
  mutate(wave_n = select(., in_2016:in_2021) %>%  rowSums(na.rm = T)) %>% 
  filter(wave_n >= 3) %>% 
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
  mutate(year = as.double(year))

three_waves <- three_waves %>% 
  left_join(afford, by = c("oslaua_code","year")) %>% 
  filter(!is.na(affordability))

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp <- gdp %>% 
  rename(oslaua_code = `LA code`) %>%
  pivot_longer(
    cols = all_of(year_range),
    names_to = "year",
    values_to = "gdp_capita"
  ) %>% 
  mutate(year = parse_double(year)) %>% 
  select(oslaua_code, year, gdp_capita)

three_waves <- three_waves %>% 
  left_join(gdp, by = c("oslaua_code", "year"))

three_waves %>% 
  map_int(~sum(is.na(.)))

# population data -----------------------------------------------------------

load("pop.RData")

pop <- pop %>% 
  filter(!year %in% c(2014, 2015))

three_waves <- three_waves %>% 
  left_join(pop, by = c("oslaua_code", "year"))

three_waves %>% 
  map_int(~sum(is.na(.)))

## education data -------------------------------------------------------

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(oslaua_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(oslaua_code, degree_pct)

three_waves <- three_waves %>% 
  left_join(edu, by = "oslaua_code")

three_waves %>% map_int(~sum(is.na(.)))

## centering, scaling and between-within calculations LA vars -------------------

# centering year
three_waves <- three_waves %>% 
  mutate(year_c = year - 2016)

# between and within calculations
within_between <- function(df, group_var, mutate_vars){
  
  df <- df %>% 
    group_by(pick({{group_var}})) %>% 
    mutate(
      across({{mutate_vars}}, \(x) x - mean(x, na.rm = T), .names = "{.col}_within"),
      across({{mutate_vars}}, \(x) mean(x, na.rm = T), .names = "{.col}_mean")
    ) %>% 
    ungroup()
  
  return(df)
  
}

three_waves <- three_waves %>% 
  within_between(oslaua_code, c(affordability, gdp_capita, pop_density))

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

to_scale <- three_waves %>% 
  select(contains("within")|contains("mean")) %>% 
  names()

to_scale <- c(to_scale, "degree_pct")

three_waves[to_scale] <- three_waves[to_scale] %>%
  map_df(scale_this)

## creating person variables ------------------------------------------------

three_waves %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(three_waves %>% select(starts_with("p_")), .data[[.x]]))

three_waves$uni <- ifelse(
  three_waves$p_edlevel == 4|three_waves$p_edlevel == 5, 1, 0
)

three_waves %>% 
  count(p_edlevel, uni)

three_waves$white <- ifelse(
  three_waves$p_ethnicity == 1|three_waves$p_ethnicity == 2, 1, 0
)

three_waves$white[three_waves$p_ethnicity == 16] <- NA

three_waves %>% 
  count(p_ethnicity, white)

three_waves$no_religion <- ifelse(
  three_waves$p_religion == 1, 1, 0
)

three_waves %>% 
  count(p_religion, no_religion)

three_waves$c1_c2 <- ifelse(
  three_waves$p_socgrade == 3 | three_waves$p_socgrade == 4, 1, 0
)
three_waves$c1_c2[three_waves$p_socgrade == 7|three_waves$p_socgrade == 8] <- NA

three_waves$d_e <- ifelse(
  three_waves$p_socgrade == 5 | three_waves$p_socgrade == 6, 1, 0
)
three_waves$d_e[three_waves$p_socgrade == 7|three_waves$p_socgrade == 8] <- NA

three_waves %>% 
  count(p_socgrade, c1_c2, d_e)

three_waves$non_uk_born <- ifelse(
  three_waves$p_country_birth != 1, 1, 0
)
three_waves$non_uk_born[three_waves$p_country_birth == 9999] <- NA

three_waves %>% 
  count(p_country_birth, non_uk_born)

three_waves$own_outright <- ifelse(
  three_waves$p_housing == 1, 1, 0
)

three_waves$own_mortgage <- ifelse(
  three_waves$p_housing == 2, 1, 0
)

three_waves$homeowner <- ifelse(
  three_waves$p_housing == 1|three_waves$p_housing == 2, 1, 0
)

three_waves$private_renting <- ifelse(
  three_waves$p_housing == 4, 1, 0
)

three_waves$social_housing <- ifelse(
  three_waves$p_housing == 5|three_waves$p_housing == 6, 1, 0
)

three_waves %>% 
  count(p_housing, own_outright, own_mortgage, 
        homeowner, private_renting, social_housing)

# scaling age
three_waves$age <- scale_this(three_waves$age)

three_waves %>% 
  map_int(~sum(is.na(.)))

three_waves %>% summarise(mean_age = mean(age), sd_age = sd(age))

## immig longitudinal ---------------------------------------------------------

immi_long <- lmer(immigSelf ~ affordability_within + affordability_mean +
                    pop_density_within + pop_density_mean +
                    gdp_capita_within + gdp_capita_mean + degree_pct +
                    uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                    homeowner + private_renting +
                    social_housing +
                    year_c + (1|id) + (1|oslaua_code),
                  data = three_waves, REML = FALSE)

summary(immi_long)
summ(immi_long)

# redist longitudinal -------------------------------------------------------

redist_long <- lmer(redistSelf ~ affordability_within + affordability_mean +
                      pop_density_within + pop_density_mean +
                      gdp_capita_within + gdp_capita_mean + degree_pct +
                      uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                      homeowner + private_renting +
                      social_housing +
                      year_c + (1|id) + (1|oslaua_code),
                    data = three_waves, REML = FALSE)

summary(redist_long)
summ(redist_long)
