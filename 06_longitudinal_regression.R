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
          "p_housing", "age", "oslaua_code","gor","wt", "year")

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
  #filter(wave_n >= 3) %>% 
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

pop <- pop %>% 
  filter(!year %in% c(2014, 2015))

df <- df %>% 
  left_join(pop, by = c("oslaua_code", "year"))

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

df <- df %>% 
  within_between(oslaua_code, c(affordability, gdp_capita, pop_density,
                                over_65_pct, under_15_pct, manuf_pct))

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

to_scale <- df %>% 
  select(contains("within")|contains("mean")) %>% 
  names()

to_scale <- c(to_scale, "degree_pct")

df[to_scale] <- df[to_scale] %>%
  map_df(scale_this)

## creating person variables ------------------------------------------------

df %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(df %>% select(starts_with("p_")), .data[[.x]]))

df$uni <- ifelse(
  df$p_edlevel == 4|df$p_edlevel == 5, 1, 0
)

df %>% 
  count(p_edlevel, uni)

df$white <- ifelse(
  df$p_ethnicity == 1|df$p_ethnicity == 2, 1, 0
)

df$white[df$p_ethnicity == 16] <- NA

df %>% 
  count(p_ethnicity, white)

df$no_religion <- ifelse(
  df$p_religion == 1, 1, 0
)

df %>% 
  count(p_religion, no_religion)

df$c1_c2 <- ifelse(
  df$p_socgrade == 3 | df$p_socgrade == 4, 1, 0
)
df$c1_c2[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df$d_e <- ifelse(
  df$p_socgrade == 5 | df$p_socgrade == 6, 1, 0
)
df$d_e[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df %>% 
  count(p_socgrade, c1_c2, d_e)

df$non_uk_born <- ifelse(
  df$p_country_birth != 1, 1, 0
)
df$non_uk_born[df$p_country_birth == 9999] <- NA

df %>% 
  count(p_country_birth, non_uk_born)

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

df %>% 
  count(p_housing, own_outright, own_mortgage, 
        homeowner, private_renting, social_housing)

# scaling age
df$age <- scale_this(df$age)

df %>% 
  map_int(~sum(is.na(.)))

df %>% 
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T))

# removing don't knows -------------------------------------------------------

df %>% 
  count(immigSelf)

df$immigSelf[df$immigSelf == 9999] <- NA

df %>% 
  count(immigSelf)

df %>% 
  count(redistSelf)

df$redistSelf[df$redistSelf == 9999] <- NA

df %>% 
  count(redistSelf)

# region --------------------------------------------------------------------

region <- read_csv("lasregionew2021lookup.csv")

region <- region %>% 
  rename(oslaua_code = `LA code`,
         region_code = `Region code`) %>% 
  select(oslaua_code, region_code)

df <- df %>% 
  left_join(region, by = "oslaua_code")

# collinearity between degrees and affordability -----------------------------

afford_degrees <- df %>% 
  filter(year == 2021) %>%
  select(degree_pct, affordability) %>% 
  unique()

afford_degrees %>% 
  ggplot(aes(x = degree_pct, y = affordability)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method = "lm")

cor.test(afford_degrees$degree_pct, afford_degrees$affordability)

afford_degree_lm <- lm(affordability ~ degree_pct,
                       data = afford_degrees)

summary(afford_degree_lm)
par(mfrow = c(2,2))
plot(afford_degree_lm)

# looking at NAs ----------------------------------------------------------

df %>% 
  select(immigSelf, affordability_within, affordability_mean,
         pop_density_within, pop_density_mean,
         over_65_pct_within, over_65_pct_mean, under_15_pct_within, 
         under_15_pct_mean, degree_pct,
         gdp_capita_mean, gdp_capita_within, manuf_pct_mean, manuf_pct_within,
         uni, white, no_religion, c1_c2, d_e, non_uk_born, homeowner, 
         private_renting, social_housing, year_c, oslaua_code,
         gor, region_code, id) %>% 
  map_int(~sum(is.na(.)))

missing_las <- function(df, var){
  miss <- df %>% 
    filter(is.na({{var}})) %>% 
    count(oslaua_code, sort = T)
  miss_las <- miss$n
  names(miss_las) <- miss$oslaua_code
  return(miss_las)
}

# affordability NAs are Scotland, and some missing values from City of London
missing_las(df, affordability)

# degree_pct NAs are Scotland
missing_las(df, degree_pct)

# region NAs are Scotland
missing_las(df, region_code)

###############################################################################
# immigration -----------------------------------------------------------------
###############################################################################

# producing dataset
immig_df <- df %>% 
  select(immigSelf, affordability_within, affordability_mean,
         pop_density_within, pop_density_mean,
         over_65_pct_within, over_65_pct_mean, under_15_pct_within, 
         under_15_pct_mean, degree_pct,
         gdp_capita_mean, gdp_capita_within, manuf_pct_mean, manuf_pct_within,
         uni, white, no_religion, c1_c2, d_e, non_uk_born, homeowner, 
         private_renting, social_housing, year_c, oslaua_code,
         region_code, id) %>%  
  na.omit()

nrow(df) - nrow(immig_df)

## immi null ------------------------------------------------------------------

immig_df <- immig_df %>% 
  mutate(immigSelf = 10 - immigSelf)

immi_null <- lmer(immigSelf ~ (1|id) + (1|region_code) + 
                    (1|region_code:oslaua_code),
                  data = immig_df, REML=FALSE)

summary(immi_null)
summ(immi_null, r.squared = FALSE)

## immig longitudinal ---------------------------------------------------------

immi_long <- lmer(immigSelf ~ affordability_within + affordability_mean +
                    pop_density_within + pop_density_mean +
                    over_65_pct_within + over_65_pct_mean +
                    under_15_pct_within + under_15_pct_mean +
                    degree_pct + 
                    gdp_capita_within + gdp_capita_mean +
                    manuf_pct_within + manuf_pct_mean +
                    uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                    homeowner + private_renting +
                    social_housing +
                    year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                  data = immig_df, REML = FALSE)

summary(immi_long)
summ(immi_long, r.squared=F)

# immig cross level interactions ---------------------------------------------

immi_cint <- lmer(immigSelf ~ (affordability_mean * social_housing) +
                    affordability_within +
                    pop_density_within + pop_density_mean +
                    over_65_pct_within + over_65_pct_mean +
                    under_15_pct_within + under_15_pct_mean + 
                    degree_pct + 
                    gdp_capita_within + gdp_capita_mean +
                    manuf_pct_within + manuf_pct_mean +
                    uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                    homeowner + private_renting +
                    #social_housing +
                    year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                  data = immig_df, REML = FALSE)

summary(immi_cint)
summ(immi_cint, r.squared=F)

immi_cint2 <- lmer(immigSelf ~ (affordability_mean * social_housing) +
                     (affordability_mean * homeowner) +
                     affordability_within +
                     pop_density_within + pop_density_mean +
                     over_65_pct_within + over_65_pct_mean +
                     under_15_pct_within + under_15_pct_mean + 
                     degree_pct + 
                     gdp_capita_within + gdp_capita_mean +
                     manuf_pct_within + manuf_pct_mean +
                     uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                     #homeowner + 
                     private_renting + #social_housing +
                     year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                   data = immig_df, REML = FALSE)

summary(immi_cint2)
summ(immi_cint2, r.squared=F)

(0.13583  - 0.021025) / 0.13583  # 84.5% of LA variance explained by level 2 fixed effects

#############################################################################
# redistribution ------------------------------------------------------------
#############################################################################

# producing dataset
redist_df <- df %>% 
  select(redistSelf, affordability_within, affordability_mean,
         pop_density_within, pop_density_mean,
         over_65_pct_within, over_65_pct_mean, under_15_pct_within, 
         under_15_pct_mean, degree_pct, 
         gdp_capita_mean, gdp_capita_within, manuf_pct_mean, manuf_pct_within,
         uni, white, no_religion, c1_c2, d_e, non_uk_born, homeowner, 
         private_renting, social_housing, year_c, oslaua_code,
         region_code, id) %>% 
  na.omit()

nrow(df) - nrow(redist_df)

# redist null model -------------------------------------------------------

redist_null <- lmer(redistSelf ~ (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                    data = redist_df, REML = FALSE)

summary(redist_null)
summ(redist_null, r.squared=F)

# redist longitudinal ---------------------------------------------------------

redist_long <- lmer(redistSelf ~ affordability_within + affordability_mean +
                      pop_density_within + pop_density_mean +
                      over_65_pct_within + over_65_pct_mean +
                      under_15_pct_within + under_15_pct_mean + 
                      degree_pct + 
                      gdp_capita_within + gdp_capita_mean +
                      manuf_pct_within + manuf_pct_mean +
                      uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                      homeowner + private_renting +
                      social_housing +
                      year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                    data = redist_df, REML = FALSE)

summary(redist_long)
summ(redist_long, r.squared=F)

(0.08423 - 0.02078) / 0.08423 # 75.3% level two variation explained by fixed effects

# redist cross level interactions ------------------------------------------------

redist_cint <- lmer(redistSelf ~ (affordability_mean * social_housing) +
                      affordability_within + 
                      pop_density_within + pop_density_mean +
                      over_65_pct_within + over_65_pct_mean +
                      under_15_pct_within + under_15_pct_mean + 
                      degree_pct + 
                      gdp_capita_within + gdp_capita_mean +
                      manuf_pct_within + manuf_pct_mean +
                      uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                      homeowner + private_renting +
                      #social_housing +
                      year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                    data = redist_df, REML = FALSE)

summary(redist_cint)
summ(redist_cint, r.squared=F)

redist_cint2 <- lmer(redistSelf ~ (affordability_mean * homeowner) +
                       (affordability_mean * social_housing) +
                       affordability_within + 
                       pop_density_within + pop_density_mean +
                       over_65_pct_within + over_65_pct_mean +
                       under_15_pct_within + under_15_pct_mean + 
                       degree_pct + 
                       gdp_capita_within + gdp_capita_mean +
                       manuf_pct_within + manuf_pct_mean +
                       uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                       #homeowner + 
                       private_renting + # social_housing +
                       year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                     data = redist_df, REML = FALSE)

summary(redist_cint2)
summ(redist_cint2, r.squared=F)

###############################################################################
# principal components regression ---------------------------------------------
###############################################################################

pacman::p_load(AMR)

# pca on level two variables

pca_df <- df %>% 
  select(year, oslaua_code, affordability, degree_pct, pop_density, 
         over_65_pct, under_15_pct,
         gdp_capita, manuf_pct) %>% 
  na.omit() %>%
  unique()

pca_mat <- pca_df %>%
  select(-year, -oslaua_code) %>% 
  as.matrix() %>% 
  scale()

# PCA
pca_level_2 <- prcomp(pca_mat)

# biplot
ggplot_pca(pca_level_2)

# extracting PC1 and PC2 
pca_df$PC1 <- pca_level_2$x[,1]
pca_df$PC2 <- pca_level_2$x[,2]

# binding to df
df <- df %>% 
  left_join(
    pca_df %>% select(oslaua_code, year, PC1, PC2),
    by = c("oslaua_code","year")
  ) %>% 
  within_between(oslaua_code, c(PC1, PC2))

immig_df <- immig_df %>% 
  left_join(df %>% 
              select(oslaua_code, year_c, PC1_mean,
                     PC1_within, PC2_mean, PC2_within) %>% 
              unique(),
            by = c("oslaua_code","year_c"))

immi_cint3 <- lmer(immigSelf ~ (PC1_mean * social_housing) +
                     (PC2_mean * homeowner) +
                     PC1_within +
                     PC2_within +
                     uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                     #homeowner + 
                     private_renting + #social_housing +
                     year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                   data = immig_df, REML = FALSE)

summary(immi_cint3)
summ(immi_cint3, r.squared=F)

redist_df <- redist_df %>% 
  left_join(df %>% 
              select(oslaua_code, year_c, PC1_mean,
                     PC1_within, PC2_mean, PC2_within) %>% 
              unique(),
            by = c("oslaua_code","year_c"))

redist_cint3 <- lmer(redistSelf ~ (PC1_mean * social_housing) +
                       (PC2_mean * homeowner) +
                       PC1_within +
                       PC2_within +
                       uni + white + no_religion + c1_c2 + d_e + non_uk_born +
                       #homeowner +
                       private_renting + #social_housing +
                       year_c + (1|id) + (1|region_code) + (1|region_code:oslaua_code),
                     data = redist_df, REML = FALSE)

summary(redist_cint3)
summ(redist_cint3, r.squared=F)
