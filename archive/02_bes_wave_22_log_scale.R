pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest)

rm(list = ls())

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W22_v24.0.dta")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

df %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(df %>% select(starts_with("p_")), .data[[.x]]))

# p_edlevel, p_ethnicity, p_religion, p_gross_household, p_housing, age, 
# p_socgrade, p_country_birth, p_past_vote_2019, p_turnout_2019
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
df$redistSelf[df$redistSelf == 9999] <- NA
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
df %>% count(redistSelf)
df %>% count(immigSelf)

# missing values -------------------------------

df %>% 
  select(la_code, uni, white_british, no_religion,
         c1_c2, d_e, income, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf, homeowner) %>% 
  map_int(~sum(is.na(.)))

df %>% 
  select(la_code, uni, white_british, no_religion,
         c1_c2, d_e, income, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf, homeowner) %>% 
  na.omit() %>% 
  nrow()

# df_immi
df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner) %>% 
  na.omit() %>% 
  nrow()

df_immi <- df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner) %>% 
  na.omit()

# df_tory
df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner) %>% 
  na.omit() %>% 
  nrow()

df_tory <- df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner) %>% 
  na.omit()

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

year_range <- as.character(seq(2019,2021,1))
afford <- afford %>% 
  rename(la_code = `Local authority code`) %>% 
  select(la_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "affordability") %>% 
  mutate(year = as.double(year))

afford <- afford %>% 
  arrange(la_code, year) %>% 
  group_by(la_code) %>% 
  mutate(afford_lag_one = lag(affordability, n = 1),
         afford_lag_two = lag(affordability, n = 2)) %>% 
  ungroup(la_code) %>% 
  filter(year == 2021) %>% 
  select(-year)

# merging
df_immi <- df_immi %>% 
  left_join(afford, by = "la_code")

df_tory <- df_tory %>% 
  left_join(afford, by = "la_code")

df_immi %>% 
  map_int(~sum(is.na(.)))

# removing scotland
df_immi <- df_immi %>% na.omit()
df_tory <- df_tory %>% na.omit()

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
  select(la_code, gdp_capita)

df_immi <- df_immi %>% 
  left_join(gdp_capita, by = "la_code")
df_tory <- df_tory %>% 
  left_join(gdp_capita, by = "la_code")

df_immi %>% 
  map_int(~sum(is.na(.)))

df_tory %>% 
  map_int(~sum(is.na(.)))

# population data -----------------------------------------------------------

pop <- read_csv("population_change.csv")

names(pop) <- c("la_code", "name", "geography", "area_sqm",
                "pop_2021", "pop_sqm_2021", "pop_2011", "pop_sqm_2011",
                "pop_2001", "pop_sqm_2001")

pop <- pop %>% 
  select(la_code, pop_sqm_2021)

df_immi <- df_immi %>% 
  left_join(pop, by = "la_code")
df_tory <- df_tory %>% 
  left_join(pop, by = "la_code")

df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

# birth country ---------------------------------------------------

load("bc.RData")

bc <- bc %>%
  filter(year %in% c(2019:2021)) %>%
  select(oslaua_code, year, foreign_per_1000) %>%
  na.omit() %>% 
  pivot_wider(
    names_from = "year", values_from = "foreign_per_1000"
  ) %>% 
  rename(
    foreign_per_1000 = `2021`,
    foreign_lag_one = `2020`,
    foreign_lag_two = `2019`
  )

df_immi <- df_immi %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))
df_tory <- df_tory %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))

df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

## age of LAs --------------------------------------------------------

load("las_by_age.RData")

las_by_age <- las_by_age %>% 
  filter(year == 2021) %>% 
  select(-year)

df_immi <- df_immi %>% 
  left_join(las_by_age, by = c("la_code" = "oslaua_code"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19)

df_tory <- df_tory %>% 
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

df_immi <- df_immi %>% 
  left_join(edu, by = "la_code")
df_tory <- df_tory %>% 
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

df_immi <- df_immi %>% 
  left_join(manuf, by = "la_code")
df_tory <- df_tory %>% 
  left_join(manuf, by = "la_code")

df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

# region --------------------------------------------------------------------

region <- read_csv("lasregionew2021lookup.csv")

region <- region %>% 
  rename(la_code = `LA code`,
         region_code = `Region code`) %>% 
  select(la_code, region_code)

df_immi <- df_immi %>% 
  left_join(region, by = "la_code")
df_tory <- df_tory %>% 
  left_join(region, by = "la_code")

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df_immi %>% select(gdp_capita:manuf_pct) %>% names()
rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}

df_immi  <- df_immi %>% rename_raw(all_of(level_twos))
df_tory  <- df_tory %>% rename_raw(all_of(level_twos))

# scaling
df_immi[level_twos] <- df_immi[level_twos] %>%
  map_df(scale_this)
df_tory[level_twos] <- df_tory[level_twos] %>%
  map_df(scale_this)

# log scale for affordability
df_immi$affordability_raw <- df_immi$affordability
df_immi$afford_lag_one_raw <- df_immi$afford_lag_one
df_immi$afford_lag_two_raw <- df_immi$afford_lag_two
df_tory$affordability_raw <- df_tory$affordability
df_tory$afford_lag_one_raw <- df_tory$afford_lag_one
df_tory$afford_lag_two_raw <- df_tory$afford_lag_two

df_immi$affordability <- log(df_immi$affordability)
df_immi$afford_lag_one <- log(df_immi$afford_lag_one)
df_immi$afford_lag_two <- log(df_immi$afford_lag_two)
df_tory$affordability <- log(df_tory$affordability)
df_tory$afford_lag_one <- log(df_tory$afford_lag_one)
df_tory$afford_lag_two <- log(df_tory$afford_lag_two)

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# reordering immigSelf
df_immi <- df_immi %>% 
  rename(immigSelf_pro = immigSelf) %>% 
  mutate(immigSelf = 10 - immigSelf_pro)

df_immi %>% 
  count(immigSelf, immigSelf_pro)

# ols null model
immi_fit <- lm(immigSelf ~ 1, data = df_immi)

# lmer null model
immi_lmer <- lmer(immigSelf ~ (1|la_code), data = df_immi)

logLik(immi_fit)
logLik(immi_lmer)
2 * (logLik(immi_lmer) - logLik(immi_fit))

# lmer null model with region
immi_reg <- lmer(immigSelf ~ (1|region_code) + (1|region_code:la_code),
                 data = df_immi, REML = FALSE)

anova(immi_lmer, immi_reg)

# multivariate ------------------------------------------------

immi_multi <- lmer(immigSelf ~ white_british + 
                     no_religion + uni +
                     social_housing + private_renting + 
                     homeowner + age + 
                     c1_c2 + d_e + non_uk_born +
                     (1|region_code) + (1|region_code:la_code),
                   data = df_immi, REML = FALSE)

summary(immi_multi)

# including level 2 predictors  ------------------------------

immi_con <- lmer(immigSelf ~ white_british + 
                   no_religion + uni +
                   social_housing + private_renting + 
                   homeowner + age + 
                   c1_c2 + d_e + non_uk_born + 
                   affordability + gdp_capita +
                   pop_sqm_2021 + foreign_per_1000  + 
                   over_65_pct + under_15_pct + 
                   degree_pct + 
                   manuf_pct +
                   (1|region_code) + (1|region_code:la_code),
                 data = df_immi, REML = FALSE)
summary(immi_con)

anova(immi_multi, immi_con)

# cross level interaction ------------------------------------------------------

immi_int <- lmer(immigSelf ~ (social_housing * affordability) +
                   (homeowner * affordability) +
                   white_british + 
                   no_religion + uni +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita +
                   pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + 
                   manuf_pct +
                   (1|region_code) + (1|region_code:la_code),
                 data = df_immi, REML = FALSE)
summary(immi_int)

anova(immi_con, immi_int)

# cross level interaction not incl. population due to collinearity
immi_int2 <- lmer(immigSelf ~ (social_housing * affordability) +
                    (homeowner * affordability) +
                    white_british + 
                    no_religion + uni +
                    private_renting + age + 
                    c1_c2 + d_e + non_uk_born + 
                    gdp_capita + foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + 
                    manuf_pct +
                    (1|region_code) + (1|region_code:la_code),
                  data = df_immi, REML = FALSE)
summary(immi_int2)

anova(immi_con, immi_int2)

AIC(immi_con, immi_int, immi_int2)

## immigration lagged effects ------------------------------------------------

immi_lag1 <- lmer(immigSelf ~ (social_housing * afford_lag_one) +
                    (homeowner * afford_lag_one) +
                    white_british +
                    no_religion + uni +
                    private_renting + age +
                    c1_c2 + d_e + non_uk_born +
                    gdp_capita +
                    pop_sqm_2021 + foreign_lag_one +
                    over_65_pct + under_15_pct +
                    degree_pct +
                    manuf_pct +
                    (1|region_code) + (1|region_code:la_code),
                  data = df_immi, REML = FALSE)
summary(immi_lag1)

immi_lag2 <- lmer(immigSelf ~ (social_housing * afford_lag_two) +
                    (homeowner * afford_lag_two) +
                    white_british + no_religion + uni +
                    private_renting + age +
                    c1_c2 + d_e + non_uk_born +
                    gdp_capita + pop_sqm_2021 + foreign_lag_two +
                    over_65_pct + under_15_pct +
                    degree_pct + manuf_pct +
                    (1|region_code) + (1|region_code:la_code),
                  data = df_immi, REML = FALSE)
summary(immi_lag2)

AIC(immi_int, immi_lag1, immi_lag2)

#############################################################################
# vote tory 2019 -----------------------------------------------------------
#############################################################################

df_tory$tory_2019 <- as.factor(df_tory$tory_2019)
levels(df_tory$tory_2019)

# ols null model
tory_fit <- glm(tory_2019 ~ 1, data = df_tory, family = "binomial")

# lmer null model
tory_lmer <- glmer(tory_2019 ~ (1|la_code), data = df_tory, 
                   family = binomial("logit"))

logLik(tory_fit)
logLik(tory_lmer)
2 * (logLik(tory_lmer) - logLik(tory_fit))

tory_reg <- glmer(tory_2019 ~ (1|region_code) + (1|region_code:la_code), 
                  data = df_tory, 
                  family = binomial("logit"))

anova(tory_lmer, tory_reg)

# cross level interaction ------------------------------------------------------

tory_int <- glmer(tory_2019 ~ (social_housing * affordability) +
                    (homeowner * affordability) +
                    white_british + 
                    no_religion + uni +
                    private_renting + age + 
                    c1_c2 + d_e + non_uk_born + 
                    gdp_capita +
                    pop_sqm_2021 + foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + 
                    manuf_pct +
                    (1|region_code) + (1|region_code:la_code),
                  data = df_tory, family = binomial("logit"))
summary(tory_int)

anova(tory_con, tory_int)
