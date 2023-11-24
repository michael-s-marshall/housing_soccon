pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest)

rm(list = ls())

# lmer_coefs function ---------------------------------------------------------

source("05_lmer_coefs_function.R")

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

df$not_disabled <- ifelse(df$p_disability == 3, 1, 0)
df$not_disabled[is.na(df$p_disability)] <- NA
df %>% count(not_disabled, p_disability)
df$broadsheet <- ifelse(df$p_paper_read %in% c(6,7,8,9,10), 1, 0)
df$broadsheet[is.na(df$p_paper_read)] <- NA
df$tabloid <- ifelse(df$p_paper_read %in% c(1,2,3,4,5), 1, 0)
df$tabloid[is.na(df$p_paper_read)] <- NA
df %>% count(broadsheet, tabloid, p_paper_read)
df$full_time <- ifelse(df$p_work_stat == 1, 1, 0)
df$edu_20plus <- ifelse(df$p_education_age == 5, 1, 0)
df$edu_20plus[is.na(df$p_education_age)] <- NA
df %>% count(edu_20plus, p_education_age)

uni_df <- df %>% 
  select(uni, white_british, c1_c2, d_e, own_mortgage,
         own_outright, private_renting, social_housing, non_uk_born,
         non_voter, broadsheet, tabloid, full_time,
         edu_20plus, age_raw) %>%
  mutate(age_sq = age_raw ^ 2,
         age_cub = age_raw ^ 3) %>% 
  na.omit()

uni_df <- uni_df %>% 
  mutate(id = row_number())

set.seed(123)
uni_df_train <- sample_frac(uni_df, size = 0.9)
uni_df_test <- uni_df %>% 
  filter(!id %in% uni_df_train$id)

uni_df_train <- uni_df_train %>% select(-id)
uni_df_test <- uni_df_test %>% select(-id)

uni_logit <- glm(uni ~ ., data = uni_df_train, family = "binomial")
summary(uni_logit)

uni_df_train <- uni_df_train %>% select(-full_time)

uni_logit <- glm(uni ~ ., data = uni_df_train, family = "binomial")
summary(uni_logit)

uni_probs <- predict(uni_logit, type = "response")
uni_preds <- ifelse(uni_probs > 0.5, 1, 0)
table(uni_df_train$uni, uni_preds)
mean(uni_df_train$uni == uni_preds)

test_probs <- predict(uni_logit, type = "response", newdata = uni_df_test)
test_preds <- ifelse(test_probs > 0.5, 1, 0)
table(uni_df_test$uni, test_preds)
mean(uni_df_test$uni == test_preds)

df <- df %>% 
  mutate(age_sq = age_raw ^ 2,
         age_cub = age_raw ^ 3)

df$uni_propensity <- predict(uni_logit, type = "response", newdata = df)
summary(df$uni_propensity)

# datasets -------------------------------------------------------------------

df_redist <- df %>% 
  select(redistSelf, la_code, white_british, no_religion, uni,
         c1_c2, d_e, social_housing, private_renting, age, 
         age_raw, non_uk_born, homeowner, uni_propensity) %>% 
  mutate(uni_pred = ifelse(uni_propensity > 0.5, 1, 0),
         uni_full = ifelse(is.na(uni), uni_pred, uni))

df_redist %>% count(uni, uni_full, uni_pred)

to_drop <- df_redist %>% select(-uni, -uni_propensity, -uni_pred) %>% names()

df_redist <- df_redist %>% 
  drop_na(all_of(to_drop))

df_redist %>% map_int(~sum(is.na(.)))

# df_immi
df_immi <- df %>% 
  select(immigSelf, la_code, white_british, no_religion, uni,
         c1_c2, d_e, social_housing, private_renting, age, 
         age_raw, non_uk_born, homeowner, uni_propensity) %>% 
  mutate(uni_pred = ifelse(uni_propensity > 0.5, 1, 0),
         uni_full = ifelse(is.na(uni), uni_pred, uni))

df_immi %>% count(uni, uni_full, uni_pred)

to_drop2 <- df_immi %>% select(-uni, -uni_propensity, -uni_pred) %>% names()

df_immi <- df_immi %>% 
  drop_na(all_of(to_drop2))

df_immi %>% map_int(~sum(is.na(.)))

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2021`) %>% 
  select(la_code, affordability)

# merging
df_redist <- df_redist %>% 
  left_join(afford, by = "la_code")

df_immi <- df_immi %>% 
  left_join(afford, by = "la_code")

df_redist %>% 
  map_int(~sum(is.na(.)))

df_immi %>% 
  map_int(~sum(is.na(.)))

# removing scotland
df_redist <- df_redist %>% na.omit()
df_immi <- df_immi %>% na.omit()

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
  select(la_code, gdp_capita)

df_redist <- df_redist %>% 
  left_join(gdp_capita, by = "la_code")
df_immi <- df_immi %>% 
  left_join(gdp_capita, by = "la_code")

# population data -----------------------------------------------------------

pop <- read_csv("population_change.csv")

names(pop) <- c("la_code", "name", "geography", "area_sqm",
                "pop_2021", "pop_sqm_2021", "pop_2011", "pop_sqm_2011",
                "pop_2001", "pop_sqm_2001")

pop <- pop %>% 
  select(la_code, pop_sqm_2021)

df_redist <- df_redist %>% 
  left_join(pop, by = "la_code")
df_immi <- df_immi %>% 
  left_join(pop, by = "la_code")

# ethnic diversity ----------------------------------------------------------

ethnic <- read_csv("population-by-ethnicity-and-local-authority-2021.csv")

ethnic <- ethnic %>% 
  filter(Ethnicity == "White") %>%
  rename(
    la_code = Geography_code,
    white_perc = Value1
  ) %>% 
  mutate(white_perc = white_perc / 100) %>% 
  select(la_code, white_perc)

df_redist <- df_redist %>% 
  left_join(ethnic, by = "la_code")
df_immi <- df_immi %>% 
  left_join(ethnic, by = "la_code")

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

df_redist <- df_redist %>% 
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
df_redist <- df_redist %>% 
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
df_redist <- df_redist %>% 
  left_join(manuf, by = "la_code")

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df_immi %>% select(affordability:manuf_pct) %>% names()
rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}

df_redist  <- df_redist %>% rename_raw(all_of(level_twos))
df_immi  <- df_immi %>% rename_raw(all_of(level_twos))

# scaling
df_redist[level_twos] <- df_redist[level_twos] %>%
  map_df(scale_this)
df_immi[level_twos] <- df_immi[level_twos] %>%
  map_df(scale_this)

###############################################################################
# redistself ------------------------------------------------------------------
###############################################################################

# multivariate ------------------------------------------------

redist_multi <- lmer(redistSelf ~ white_british + 
                       no_religion + uni_full +
                       social_housing + private_renting + 
                       homeowner + age + 
                       c1_c2 + d_e + non_uk_born +
                       (1|la_code),
                     data = df_redist, REML = FALSE)

summary(redist_multi)

# including level 2 predictors  ------------------------------

redist_con <- lmer(redistSelf ~ white_british + 
                     no_religion + uni_full +
                     social_housing + private_renting + 
                     homeowner + age + 
                     c1_c2 + d_e + non_uk_born + 
                     affordability + gdp_capita +
                     pop_sqm_2021 + white_perc + 
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     manuf_pct +
                     (1|la_code),
                   data = df_redist, REML = FALSE)

summary(redist_con)

anova(redist_multi, redist_con)

# cross level interaction ------------------------------------------------------

redist_int <- lmer(redistSelf ~ (social_housing * affordability) +
                     (homeowner * affordability) +
                     white_british + 
                     no_religion + uni_full +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     gdp_capita +
                     pop_sqm_2021 + white_perc + 
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     manuf_pct +
                     (1|la_code),
                   data = df_redist, REML = FALSE)
summary(redist_int)

anova(redist_con, redist_int)

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# reordering immigSelf
df_immi <- df_immi %>% 
  rename(immigSelf_pro = immigSelf) %>% 
  mutate(immigSelf = 10 - immigSelf_pro)

df_immi %>% 
  count(immigSelf, immigSelf_pro)

# multivariate ------------------------------------------------

immi_multi <- lmer(immigSelf ~ white_british + 
                     no_religion + uni_full +
                     social_housing + private_renting + 
                     homeowner + age + 
                     c1_c2 + d_e + non_uk_born +
                     (1|la_code),
                   data = df_immi, REML = FALSE)

summary(immi_multi)

# including level 2 predictors  ------------------------------

immi_con <- lmer(immigSelf ~ white_british + 
                   no_religion + uni_full +
                   social_housing + private_renting + 
                   homeowner + age + 
                   c1_c2 + d_e + non_uk_born + 
                   affordability + gdp_capita +
                   pop_sqm_2021 + white_perc + 
                   over_65_pct + under_15_pct + 
                   degree_pct + 
                   manuf_pct +
                   (1|la_code),
                 data = df_immi, REML = FALSE)
summary(immi_con)

anova(immi_multi, immi_con)

# cross level interaction ------------------------------------------------------

immi_int <- lmer(immigSelf ~ (social_housing * affordability) +
                   (homeowner * affordability) +
                   white_british + 
                   no_religion + uni_full +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita +
                   pop_sqm_2021 + white_perc + 
                   over_65_pct + under_15_pct + 
                   degree_pct + 
                   manuf_pct +
                   (1|la_code),
                 data = df_immi, REML = FALSE)
summary(immi_int)

anova(immi_con, immi_int)
