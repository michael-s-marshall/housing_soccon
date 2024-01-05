##########################################################################
# creating cross-sectional dataset ---------------------------------------
##########################################################################

# packages ---------------------------------------------------------------

pacman::p_load(tidyverse, haven)

rm(list = ls())

# BES wave 22 ------------------------------------------------------------

df <- read_dta("BES2019_W22_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

##############################################################
# level 1 vars -----------------------------------------------
##############################################################

df %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(df %>% select(starts_with("p_")), .data[[.x]]))

# p_edlevel, p_ethnicity, p_religion, p_housing, age, 
# p_socgrade, p_country_birth, p_past_vote_2019
# ind vars
df <- df %>% 
  mutate(
    uni = fct_collapse(
      as.factor(p_edlevel),
      "1" = c("4","5"),
      "0" = c("0","1","2","3")
    ) %>% 
      as.character() %>% 
      parse_double(),
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    soc_class = fct_collapse(
      as.factor(p_socgrade),
      "A-B" = c("1","2"),
      "C1-C2" = c("3","4"),
      "D-E" = c("5","6"),
      "Other" = c("7","8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1)
  ) %>% 
  rename(la_code = oslaua_code)

df$soc_class[df$soc_class == "Other"] <- NA
df$c1_c2 <- ifelse(df$soc_class == "C1-C2", 1, 0)
df$d_e <- ifelse(df$soc_class == "D-E", 1, 0)
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                    NA, df$p_gross_household)
df$income <- rescale01(df$income, na.rm = T)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$homeowner <- ifelse(df$own_outright == 1 | df$own_mortgage == 1, 1, 0)
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age_raw <- df$age
df$age <- scale_this(df$age)

df %>% 
  count(tory_2019, p_turnout_2019, p_past_vote_2019)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(immigSelf)

# university full, incl. predictions
load("df_uni.RData")

df <- df %>% 
  left_join(df_uni, by = "id")

##############################################################
# level 2 vars -----------------------------------------------
##############################################################

# affordability and prices data --------------------------------------

# affordability ratio
afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2021`) %>% 
  select(la_code, affordability) %>% 
  mutate(affordability_log = log(affordability))

# prices
prices <- read_csv("median_house_prices.csv")

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = log(`2021`)) %>% 
  select(la_code, prices)

# merging
df <- df %>% 
  left_join(afford, by = "la_code") %>% 
  left_join(prices, by = "la_code")

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
  select(la_code, gdp_capita)

df <- df %>% 
  left_join(gdp_capita, by = "la_code")

# population data -----------------------------------------------------------

pop <- read_csv("population_change.csv")

names(pop) <- c("la_code", "name", "geography", "area_sqm",
                "pop_2021", "pop_sqm_2021", "pop_2011", "pop_sqm_2011",
                "pop_2001", "pop_sqm_2001")

pop <- pop %>% 
  select(la_code, pop_sqm_2021)

df <- df %>% 
  left_join(pop, by = "la_code")

# birth country ---------------------------------------------------

load("bc.RData")

bc <- bc %>%
  filter(year == 2021) %>%
  select(oslaua_code, foreign_per_1000) 

df <- df %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))

## age of LAs --------------------------------------------------------

load("las_by_age.RData")

las_by_age <- las_by_age %>% 
  filter(year == 2021) %>% 
  select(-year)

df <- df %>% 
  left_join(las_by_age, by = c("la_code" = "oslaua_code"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19)

# education ---------------------------------------

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(la_code, degree_pct)

df <- df %>% 
  left_join(edu, by = "la_code")

# manufacturing percentage ------------------------------------------------

manuf <- read_csv("2021_industry_employment.csv")

indus_clean <- function(df){
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
           manuf_pct = employment / total_employment) %>% 
    ungroup() %>% 
    filter(industry == "manufacturing")
  return(out)
}

manuf <- indus_clean(manuf) %>% 
  rename(la_code = oslaua_code) %>% 
  select(la_code, manuf_pct)

df <- df %>% 
  left_join(manuf, by = "la_code")

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df %>% select(affordability:manuf_pct) %>% names()
rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}

df  <- df %>% rename_raw(all_of(level_twos))

# scaling
df[level_twos] <- df[level_twos] %>%
  map_df(scale_this)

# selecting variables --------------------------------------------------

df <- df %>% 
  select(id, immigSelf, tory_2019, la_code, uni, uni_propensity,
         uni_pred, uni_full, white_british,  no_religion, c1_c2, d_e, 
         own_outright, own_mortgage, social_housing, private_renting, 
         age, age_raw, non_uk_born, homeowner, 
         all_of(level_twos), contains("raw"))

# reordering immigSelf ------------------------------------------------

df <- df %>% 
  rename(immigSelf_pro = immigSelf) %>% 
  mutate(immigSelf = 10 - immigSelf_pro)

df %>% 
  count(immigSelf, immigSelf_pro)

df <- df %>% select(-immigSelf_pro)

# saving data -------------------------------------------------------

save(df, file = "cross_sectional_df.RData")
