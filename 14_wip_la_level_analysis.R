# packages ----------------------------------------------------------------------
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
vars <- c("immigSelf", "redistSelf", "p_edlevel", 
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

# observations per la -------------------------------------------------------

df <- df %>% filter(oslaua_code!= "")

la_obs <- df %>% 
  count(oslaua_code, year) %>% 
  group_by(oslaua_code) %>% 
  summarise(mean_obs = mean(n))

mean(la_obs$mean_obs)

bottom_5 <- quantile(la_obs$mean_obs, 0.05)
bottom_5

las_to_filter <- la_obs %>% 
  filter(mean_obs < 30) %>% 
  select(oslaua_code) %>% 
  as_vector()

rm(df)

## survey averages -----------------------------------------------------

dk_to_nas <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) na_if(x, 9999), .names = "{.col}"),
           across({{vars}}, \(x) na_if(x, -9989), .names = "{.col}"))
  return(df)
}

my_svy_mean <- function(df, group_vars, summ_vars, wts){
  
  svy_df <- df %>% 
    as_survey_design(ids = 1, weights = {{wts}})
  
  svy_means <- svy_df %>% 
    group_by(pick({{group_vars}})) %>% 
    summarise(
      across({{summ_vars}}, \(x) survey_mean(x, na.rm = T), 
             .names = "{.col}_mean"),
      across({{summ_vars}}, \(x) unweighted(mean(x, na.rm = T)),
             .names = "{.col}_unwt"),
      .groups = "drop")
  
  return(svy_means)
  
}

dat_list <- list(w10, w11, w14, w17, w20, w22)

dat_list <- dat_list %>% 
  map(mutate, immigSelf2 = 10 - immigSelf, .before = 2)

dat_list %>% 
  map(count, immigSelf2, immigSelf)

dat_list <- dat_list %>% 
  map(select, -immigSelf) %>% 
  map(rename, immigSelf = immigSelf2)

dat_list <- dat_list %>% 
  map(dk_to_nas, vars = c(immigSelf, redistSelf))

dat_list %>% 
  map(count, immigSelf)

wave_means <- dat_list %>% 
  map(my_svy_mean, group_vars = c(oslaua_code, year),
      summ_vars = c(immigSelf, redistSelf),
      wts = wt)

df <- bind_rows(wave_means) %>%
  filter(!oslaua_code %in% las_to_filter &
           oslaua_code != "")

df <- df %>% 
  rename(immigSelf = immigSelf_mean,
         redistSelf = redistSelf_mean)

rm(dat_list, wave_means, w10, w11, w14, w17, w20, w22, la_obs, bottom_5)

## affordability ------------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

year_range <- as.character(seq(2014,2021,1))
afford <- afford %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "affordability") %>% 
  mutate(year = as.double(year))

afford <- afford %>% 
  group_by(oslaua_code) %>% 
  arrange(oslaua_code, year) %>% 
  mutate(
    lag_one = lag(year),
    lag_two = lag(year, 2)
  ) %>% 
  left_join(
    afford, by = c("oslaua_code", "lag_one" = "year"),
    suffix = c("_current","_lag_one")
  ) %>% 
  left_join(
    afford, by = c("oslaua_code", "lag_two" = "year")
  ) %>% 
  rename(affordability_lag_two = affordability) %>%
  ungroup() %>% 
  select(-lag_one, -lag_two) %>% 
  filter(!year %in% c(2014, 2015))

# datasets by la -------------------------------------------------------------

df <- df %>% 
  left_join(afford, by = c("oslaua_code", "year")) %>% 
  filter(!is.na(affordability_current))
  
df <- df %>% 
  group_by(oslaua_code) %>% 
  mutate(afford_current_within = affordability_current - mean(affordability_current),
         afford_lag_one_within = affordability_lag_one - mean(affordability_lag_one),
         afford_lag_two_within = affordability_lag_two - mean(affordability_lag_two),
         immigSelf_within = immigSelf - mean(immigSelf),
         redistSelf_within = redistSelf - mean(redistSelf),
         afford_mean = mean(affordability_current),
         afford_lag_one_mean = mean(affordability_lag_one),
         afford_lag_two_mean = mean(affordability_lag_two)) %>% 
  ungroup()

# null models for VPCs -------------------------------------------------

immi_null <- lmer(immigSelf ~ (1|oslaua_code),
                  data = df, REML = FALSE)
summ(immi_null) # VPC = 53%

redist_null <- lmer(redistSelf ~ (1|oslaua_code),
                    data = df, REML = FALSE)
summ(redist_null) # VPC = 54%

# fixed effects regression, affordability and lagged affordability ------------

immi_fixed <- lm(immigSelf_within ~ -1 + afford_current_within, 
                 data = df)

summary(immi_fixed)

immi_fixed_lag_one <- lm(immigSelf_within ~ -1 + afford_lag_one_within, 
                          data = df)

summary(immi_fixed_lag_one)

immi_fixed_lag_two <- lm(immigSelf_within ~ -1 + afford_lag_two_within,
                          data = df)

summary(immi_fixed_lag_two)

# redistSelf
redist_fixed <- lm(redistSelf_within ~ -1 + afford_current_within, 
                 data = df)

summary(redist_fixed)

redist_fixed_lag_one <- lm(redistSelf_within ~ -1 + afford_lag_one_within, 
                         data = df)

summary(redist_fixed_lag_one)

redist_fixed_lag_two <- lm(redistSelf_within ~ -1 + afford_lag_two_within,
                         data = df)

summary(redist_fixed_lag_two)

# multilevel models -----------------------------------------------------

immi_mlm <- lmer(immigSelf ~ afford_lag_two_within +
                   afford_lag_two_mean + (1|oslaua_code),
                 data = df, REML = FALSE)

summ(immi_mlm)

redist_mlm <- lmer(redistSelf ~ afford_lag_two_within +
                     afford_lag_two_mean + (1|oslaua_code),
                 data = df, REML = FALSE)

summ(redist_mlm)

# population data ---------------------------------------------

load("pop.RData")

pop <- pop %>% 
  group_by(oslaua_code) %>% 
  arrange(oslaua_code, year) %>% 
  mutate(pop_density_lag_one = lag(pop_density),
         pop_density_lag_two = lag(pop_density, 2)) %>%
  ungroup() %>% 
  filter(!year %in% c(2014, 2015))

df <- df %>% 
  left_join(pop, by = c("oslaua_code", "year"))

# gdp per capita ---------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp <- gdp %>% 
  rename(oslaua_code = `LA code`) %>% 
  select(oslaua_code, `2014`:`2021`) %>% 
  pivot_longer(
    cols = `2014`:`2021`,
    names_to = "year",
    values_to = "gdp"
  ) %>% 
  group_by(oslaua_code) %>% 
  mutate(
    year = as.double(year)
  ) %>% 
  arrange(oslaua_code, year) %>% 
  mutate(
    gdp_lag_one = lag(gdp),
    gdp_lag_two = lag(gdp, 2)
  ) %>% 
  ungroup() %>% 
  filter(!year %in% c(2014, 2015))

df <- df %>% 
  left_join(gdp, by = c("oslaua_code", "year"))

## within and between gdp and pop ------------------------------------

df <- df %>% 
  group_by(oslaua_code) %>% 
  mutate(
    pop_within = pop_density - mean(pop_density),
    pop_lag_one_within = pop_density_lag_one - mean(pop_density_lag_one),
    pop_lag_two_within = pop_density_lag_two - mean(pop_density_lag_two),
    pop_mean = mean(pop_density),
    pop_lag_one_mean = mean(pop_density_lag_one),
    pop_lag_two_mean = mean(pop_density_lag_two),
    gdp_within = gdp - mean(gdp),
    gdp_lag_one_within = gdp_lag_one - mean(gdp_lag_one),
    gdp_lag_two_within = gdp_lag_two - mean(gdp_lag_two),
    gdp_mean = mean(gdp),
    gdp_lag_one_mean = mean(gdp_lag_one),
    gdp_lag_two_mean = mean(gdp_lag_two)
  ) %>% 
  ungroup()

# tenure data -----------------------------------------------

load("wales_tenure.RData")
load("eng_tenure.RData")

df <- df %>% 
  left_join(
    wales_tenure, by = c("la_name", "year")
  ) %>% 
  left_join(
    eng_tenure, by = c("oslaua_code", "year"),
    suffix=c("wales", "eng")
  ) %>% 
  mutate(
    prs_perc = ifelse(is.na(prs_percwales), prs_perceng, prs_percwales),
    own_occ_perc = ifelse(is.na(own_occ_percwales), own_occ_perceng, own_occ_percwales),
    soc_hous_perc = ifelse(is.na(soc_hous_percwales), soc_hous_perceng, soc_hous_percwales)
  ) %>% 
  select(-prs_percwales, -prs_perceng, -own_occ_percwales, -own_occ_perceng,
         -soc_hous_percwales, -soc_hous_perceng) %>% 
  group_by(oslaua_code) %>% 
  mutate(
    soc_hous_within = soc_hous_perc - mean(soc_hous_perc, na.rm = T),
    own_occ_within = own_occ_perc - mean(own_occ_perc, na.rm = T),
    soc_hous_mean = mean(soc_hous_perc, na.rm = T),
    own_occ_mean = mean(own_occ_perc, na.rm = T)
  ) %>% 
  ungroup()

# # scaling variables ----------------------------------------

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

to_scale <- df %>% 
  select(contains("within")|contains("mean")) %>% 
  names()

to_scale <- to_scale[-c(4, 5)]

df[to_scale] <- df[to_scale] %>%
  map_df(scale_this)

df[to_scale] %>% 
  map_dbl(mean, na.rm = T)

df[to_scale] %>% 
  map_dbl(sd, na.rm = T)

df %>% 
  map_int(~sum(is.na(.)))

# mlm for immi -----------------------------------------------

df %>% 
  ggplot(aes(x = year, y = immigSelf)) +
  geom_line(aes(group = oslaua_code), alpha = 1/4) +
  geom_smooth(method = "lm", formula = y~ poly(x,2),
              linewidth = 2)

immig_mlm_curr <- lmer(immigSelf ~ afford_current_within +
                         afford_mean +
                         gdp_within +
                         gdp_mean +
                         pop_within +
                         pop_mean +
                         soc_hous_within +
                         soc_hous_mean +
                         own_occ_within +
                         own_occ_mean +
                         poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(immig_mlm_curr)

immig_mlm_lag1 <- lmer(immigSelf ~ afford_lag_one_within +
                         afford_lag_one_mean +
                         gdp_lag_one_within +
                         gdp_lag_one_mean +
                         pop_lag_one_within +
                         pop_lag_one_mean +
                         soc_hous_within +
                         soc_hous_mean +
                         own_occ_within +
                         own_occ_mean +
                         poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(immig_mlm_lag1)

immig_mlm_lag2 <- lmer(immigSelf ~ afford_lag_two_within +
                         afford_lag_two_mean +
                         gdp_lag_two_within +
                         gdp_lag_two_mean +
                         pop_lag_two_within +
                         pop_lag_two_mean +
                         soc_hous_within +
                         soc_hous_mean +
                         own_occ_within +
                         own_occ_mean +
                         poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(immig_mlm_lag2)

# mlm redist -----------------------------------------------------

df %>% 
  ggplot(aes(x = year, y = redistSelf)) +
  geom_line(aes(group = oslaua_code), alpha = 1/4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              linewidth = 2)

redist_mlm_curr <- lmer(redistSelf ~ afford_current_within +
                          afford_mean +
                          gdp_within +
                          gdp_mean +
                          pop_within +
                          pop_mean +
                          soc_hous_within +
                          soc_hous_mean +
                          own_occ_within +
                          own_occ_mean +
                          poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(redist_mlm_curr)

redist_mlm_lag1 <- lmer(redistSelf ~ afford_lag_one_within +
                          afford_lag_one_mean +
                          gdp_lag_one_within +
                          gdp_lag_one_mean +
                          pop_lag_one_within +
                          pop_lag_one_mean +
                          soc_hous_within +
                          soc_hous_mean +
                          own_occ_within +
                          own_occ_mean +
                          poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(redist_mlm_lag1)

redist_mlm_lag2 <- lmer(redistSelf ~ afford_lag_two_within +
                          afford_lag_two_mean +
                          gdp_lag_two_within +
                          gdp_lag_two_mean +
                          pop_lag_two_within +
                          pop_lag_two_mean +
                          soc_hous_within +
                          soc_hous_mean +
                          own_occ_within +
                          own_occ_mean +
                          poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(redist_mlm_lag2)

# interactions ------------------------------------------------

immig_mlm_cint <- lmer(immigSelf ~ (afford_mean * own_occ_mean) +
                         afford_current_within +
                         gdp_within +
                         gdp_mean +
                         pop_within +
                         pop_mean +
                         soc_hous_within +
                         soc_hous_mean +
                         own_occ_within +
                         #own_occ_mean +
                         poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(immig_mlm_cint)
AIC(immig_mlm_cint)
AIC(immig_mlm_curr)

redist_mlm_cint <- lmer(redistSelf ~ (afford_mean * own_occ_mean) +
                         afford_current_within +
                         gdp_within +
                         gdp_mean +
                         pop_within +
                         pop_mean +
                         soc_hous_within +
                         soc_hous_mean +
                         own_occ_within +
                         #own_occ_mean +
                         poly(year, 2) +
                         (1|oslaua_code),
                       data = df, REML = FALSE)

summary(redist_mlm_cint)
AIC(redist_mlm_cint)
AIC(redist_mlm_curr)

redist_mlm_cint <- lmer(redistSelf ~ (afford_mean * own_occ_mean) +
                          afford_current_within +
                          gdp_within +
                          gdp_mean +
                          pop_within +
                          pop_mean +
                          soc_hous_within +
                          soc_hous_mean +
                          own_occ_within +
                          #own_occ_mean +
                          poly(year, 2) +
                          (1|oslaua_code),
                        data = df, REML = FALSE)

summary(redist_mlm_cint)
AIC(redist_mlm_cint)
AIC(redist_mlm_curr)
