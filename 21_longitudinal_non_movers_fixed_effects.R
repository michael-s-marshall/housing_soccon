pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, lfe)

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
vars <- c("id", "immigSelf", "oslaua_code","gor", "year")

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
  # filter(wave_n >= 3) %>% 
  arrange(id, year)

counts <- df %>% 
  count(id, oslaua_code) %>% 
  arrange(id) %>% 
  group_by(id) %>% 
  mutate(running_id = row_number()) %>% 
  ungroup()

run_counts <- counts %>% 
  filter(running_id > 1)

df <- df %>% 
  filter(!id %in% run_counts$id)

# affordability ---------------------------------------------------------

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
  arrange(oslaua_code, year) %>% 
  group_by(oslaua_code) %>% 
  mutate(afford_lag_one = lag(affordability, n = 1),
         afford_lag_two = lag(affordability, n = 2)) %>% 
  ungroup(oslaua_code) %>% 
  filter(year != 2014 & year != 2015)

df <- df %>% 
  left_join(afford, by = c("oslaua_code","year"))

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

df <- df %>% 
  left_join(gdp, by = c("oslaua_code", "year"))

df %>% 
  map_int(~sum(is.na(.)))

# population data -----------------------------------------------------------

load("pop.RData")

#pop <- pop %>% 
#  filter(!year %in% c(2014, 2015))

pop <- pop %>% 
  arrange(oslaua_code, year) %>% 
  group_by(oslaua_code) %>% 
  mutate(pop_lag_one = lag(pop_density, n = 1),
         pop_lag_two = lag(pop_density, n = 2)) %>% 
  ungroup(oslaua_code) %>% 
  filter(year != 2014 & year != 2015)

df <- df %>% 
  left_join(pop, by = c("oslaua_code", "year"))

df %>% 
  map_int(~sum(is.na(.)))

# birth country data -----------------------------------------------------------

load("bc.RData")

bc <- bc %>% select(oslaua_code, year, foreign_per_1000)

df <- df %>% left_join(bc, by = c("oslaua_code","year"))

df %>% 
  map_int(~sum(is.na(.)))

# age data -------------------------------------------------------------

load("las_by_age.RData")

df <- df %>% 
  left_join(las_by_age, by = c("oslaua_code","year"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19))

df %>% 
  map_int(~sum(is.na(.)))

## education data -------------------------------------------------------

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(oslaua_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(oslaua_code, degree_pct)

df <- df %>% 
  left_join(edu, by = "oslaua_code")

df %>% map_int(~sum(is.na(.)))

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

manuf_pct <- bind_rows(indus_2016, indus_2017, indus_2018,
                       indus_2019, indus_2020, indus_2021)

df <- df %>% 
  left_join(manuf_pct %>% 
              select(oslaua_code, manuf_pct, year),
            by = c("oslaua_code","year"))

rm(indus_2016, indus_2017, indus_2018, indus_2019, indus_2020, indus_2021)

## centering, scaling and between-within calculations LA vars -------------------

# centering year
df <- df %>% 
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

scaling_vars <- c("affordability", "gdp_capita", "pop_density", 
                  "foreign_per_1000", "over_65_pct", 
                  "under_15_pct", "manuf_pct")

df <- df %>% 
  within_between(oslaua_code, all_of(scaling_vars))

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

to_scale <- df %>% 
  select(contains("within")|contains("mean")) %>% 
  names()

to_scale <- c(to_scale, scaling_vars, "degree_pct")

df[to_scale] <- df[to_scale] %>%
  map_df(scale_this)

# removing don't knows -------------------------------------------------------

df %>% 
  count(immigSelf)

df$immigSelf[df$immigSelf == 9999] <- NA

df %>% 
  count(immigSelf)

# region --------------------------------------------------------------------

region <- read_csv("lasregionew2021lookup.csv")

region <- region %>% 
  rename(oslaua_code = `LA code`,
         region_code = `Region code`) %>% 
  select(oslaua_code, region_code)

df <- df %>% 
  left_join(region, by = "oslaua_code")

###############################################################################
# immigration -----------------------------------------------------------------
###############################################################################

# producing dataset
immig_df <- df %>% 
  select(immigSelf, affordability, affordability_within,
         pop_density, pop_density_within, 
         foreign_per_1000, foreign_per_1000_within, 
         over_65_pct, over_65_pct_within, 
         under_15_pct, under_15_pct_within, 
         gdp_capita, gdp_capita_within, 
         manuf_pct, manuf_pct_within, 
         degree_pct, year_c, oslaua_code, year_c,
         region_code, id) %>%  
  na.omit()

nrow(df) - nrow(immig_df)

## fixed effects ------------------------------------------------------------------

immig_df <- immig_df %>%
  mutate(
    immigpro = immigSelf,
    immigSelf = 10 - immigpro
    )

immig_df %>% count(immigSelf, immigpro)

immig_df$id <- as.factor(immig_df$id)

immi_fe <- felm(immigSelf ~ affordability +
                  pop_density + foreign_per_1000 +
                  over_65_pct + under_15_pct +
                  gdp_capita + manuf_pct + year_c | 
                  id + oslaua_code + region_code,
                data = immig_df)

summary(immi_fe)

immi_fe2 <- lmer(immigSelf ~ affordability_within +
                   pop_density_within + foreign_per_1000_within +
                   over_65_pct_within + under_15_pct_within +
                   gdp_capita_within + manuf_pct_within + #degree_pct +
                   year_c + (1|region_code) + (1|region_code:oslaua_code) +
                   (1|id:oslaua_code:region_code),
                 data = immig_df, REML = FALSE)
summary(immi_fe2)

# comparison of coefficients
coef(immi_fe)
fixef(immi_fe2)[-1]

# barplot of coefficients per model
tibble(
  felm = coef(immi_fe),
  lmer = fixef(immi_fe2)[-1],
  variable = names(coef(immi_fe))
) %>% 
  pivot_longer(
    felm:lmer,
    names_to = "model",
    values_to = "estimate"
  ) %>% 
  ggplot(aes(x = estimate, y = variable, fill = model)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "top") +
  drop_y_gridlines()
