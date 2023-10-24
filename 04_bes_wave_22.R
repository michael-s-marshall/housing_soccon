pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest)

rm(list = ls())

# lmer_coefs function ---------------------------------------------------------

source("05_lmer_coefs_function.R")

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W22_v24.0.dta")

df %>% 
  select(lr_scale, al_scale) %>% 
  ggplot(aes(x = lr_scale, y = al_scale)) +
  geom_boxplot(mapping = aes(group = cut_width(lr_scale, 0.25))) +
  geom_smooth()

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
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
    ),
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1),
    soc_class = fct_collapse(
      as.factor(p_socgrade),
      "A-B" = c("1","2"),
      "C1-C2" = c("3","4"),
      "D-E" = c("5","6"),
      "Other" = c("7","8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1)
  ) %>% 
  rename(la_code = oslaua_code)

df$soc_class[df$soc_class == "Other"] <- NA
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                    NA, df$p_gross_household)
df$income <- rescale01(df$income, na.rm = T)
df$own_outright <- df$p_housing == 1
df$private_renting <- df$p_housing == 4
df$social_housing <- df$p_housing %in% c(5, 6)
df$own_mortgage <- df$p_housing == 2
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$redistSelf[df$redistSelf == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age <- rescale01(df$age, na.rm = T)

df %>% 
  count(tory_2019, p_turnout_2019, p_past_vote_2019)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(redistSelf)
df %>% count(immigSelf)

# missing values -------------------------------

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         soc_class, income, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf) %>% 
  map_int(~sum(is.na(.)))

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         soc_class, income, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf) %>% 
  na.omit() %>% 
  nrow()

# df_con
df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_con <- df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

# df_econ
df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_econ <- df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

# df_redist
df %>% 
  select(redistSelf, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_redist <- df %>% 
  select(redistSelf, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

# df_immi
df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_immi <- df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

# df_tory
df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_tory <- df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

###############################################################################
# authoritarianism ------------------------------------------------------------
##############################################################################

# ols null model
ols_fit <- lm(al_scale ~ 1, data = df_con)

# lmer null model
lmer_fit <- lmer(al_scale ~ (1|la_code), data = df_con)

logLik(ols_fit)
logLik(lmer_fit)
2 * (logLik(lmer_fit) - logLik(ols_fit))

# random intercepts
ranef(lmer_fit)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

con_multi <- lmer(al_scale ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born +
                    (1|la_code),
                  data = df_con, REML = FALSE)

summary(con_multi)

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2021`) %>% 
  select(la_code, affordability)

# merging
df_con <- df_con %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_econ <- df_econ %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_redist <- df_redist %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_immi <- df_immi %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_tory <- df_tory %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

df_redist %>% 
  map_int(~sum(is.na(.)))

df_immi %>% 
  map_int(~sum(is.na(.)))

# removing scotland
df_con <- df_con %>% na.omit()

df_econ <- df_econ %>% na.omit()

df_redist <- df_redist %>% na.omit()

df_immi <- df_immi %>% na.omit()

df_tory <- df_tory %>% na.omit()

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
  select(la_code, gdp_capita)

gdp_growth <- gdp %>% 
  rename(la_code = `LA code`) %>% 
  select(la_code, `2010`:`2021`) %>% 
  pivot_longer(cols = `2010`:`2021`,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.integer(year)) %>% 
  arrange(la_code, year) %>% 
  group_by(la_code) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = gdp - lag(gdp),
         gdp_growth_pct = ((diff_growth / diff_year)/gdp)*100) %>% 
  summarise(gdp_growth_pct = mean(gdp_growth_pct, na.rm = T),
            .groups = "drop")

gdp_growth_5 <- gdp %>% 
  rename(la_code = `LA code`) %>% 
  select(la_code, `2016`:`2021`) %>% 
  pivot_longer(cols = `2016`:`2021`,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.integer(year)) %>% 
  arrange(la_code, year) %>% 
  group_by(la_code) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = gdp - lag(gdp),
         gdp_growth_pct = ((diff_growth / diff_year)/gdp)*100) %>% 
  summarise(gdp_growth_pct_5 = mean(gdp_growth_pct, na.rm = T),
            .groups = "drop")

df_con <- df_con %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>%
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_econ <- df_econ %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_redist <- df_redist %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_immi <- df_immi %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_tory <- df_tory %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

df_redist %>% 
  map_int(~sum(is.na(.)))

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
  mutate(pop_growth = ((pop_2021 - pop_2011) / pop_2011) * 100,
         pop_2021 = rescale01(pop_2021, na.rm = T),
         pop_sqm_2021 = rescale01(pop_sqm_2021, na.rm = T)) %>% 
  select(la_code, pop_2021, pop_growth, pop_sqm_2021, pop_sqm_growth)

pop$pop_growth <- rescale01(pop$pop_growth, na.rm = T)

df_con <- df_con %>% 
  left_join(pop, by = "la_code")
df_econ <- df_econ %>% 
  left_join(pop, by = "la_code")
df_redist <- df_redist %>% 
  left_join(pop, by = "la_code")
df_immi <- df_immi %>% 
  left_join(pop, by = "la_code")
df_tory <- df_tory %>% 
  left_join(pop, by = "la_code")

df_con %>% map_int(~sum(is.na(.)))
df_econ %>% map_int(~sum(is.na(.)))
df_redist %>% map_int(~sum(is.na(.)))
df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

# rerunning minus scotland ---------------------------------------------------

con_multi <- lmer(al_scale ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born +
                    (1|la_code),
                  data = df_con, REML = FALSE)

summary(con_multi)

# including level 2 predictors  ------------------------------

con_con <- lmer(al_scale ~ white_british + 
                  no_religion + uni +
                  own_outright + social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + affordability + gdp_capita +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_con)

anova(con_multi, con_con)

con_con2 <- lmer(al_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_growth_pct +
                   (1|la_code),
                 data = df_con, REML = FALSE)
summary(con_con2)

anova(con_multi, con_con2)

con_con3 <- lmer(al_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                   (1|la_code),
                 data = df_con, REML = FALSE)
summary(con_con3)

anova(con_multi, con_con3)

con_con4 <- lmer(al_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + pop_2021 +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_con, REML = FALSE)
summary(con_con4)

anova(con_multi, con_con4)

# cross level interaction ------------------------------------------------------

con_out <- lmer(al_scale ~ (own_outright * affordability) +
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_out)

anova(con_con4, con_out)

lmer_coefs(con_out)

con_own <- lmer(al_scale ~ (own_mortgage * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_outright + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_own)

anova(con_con4, con_own)

lmer_coefs(con_own)

con_soc <- lmer(al_scale ~ (social_housing * affordability) + 
                  white_british + 
                  no_religion + uni +
                  own_outright +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_soc)

anova(con_con4, con_soc)

lmer_coefs(con_soc)

con_pri <- lmer(al_scale ~ (private_renting * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  own_outright + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 + 
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_pri)

anova(con_con4, con_pri)

lmer_coefs(con_pri)

###############################################################################
# econ_right dimension --------------------------------------------------------
###############################################################################

# ols null model
econ_fit <- lm(lr_scale ~ 1, data = df_econ)

# lmer null model
econ_lmer <- lmer(lr_scale ~ (1|la_code), data = df_econ)

logLik(econ_fit)
logLik(econ_lmer)
2 * (logLik(econ_lmer) - logLik(econ_fit))

# random intercepts
ranef(econ_lmer)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# multivariate ------------------------------------------------

econ_multi <- lmer(lr_scale ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born +
                    (1|la_code),
                  data = df_econ, REML = FALSE)

summary(econ_multi)

# including level 2 predictors  ------------------------------

econ_con <- lmer(lr_scale ~ white_british + 
                  no_religion + uni +
                  own_outright + social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + affordability + gdp_capita +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_con)

anova(econ_multi, econ_con)

econ_con2 <- lmer(lr_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_growth_pct +
                   (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_con2)

anova(econ_multi, econ_con2)

econ_con3 <- lmer(lr_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                   (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_con3)

anova(econ_multi, econ_con3)

econ_con4 <- lmer(lr_scale ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                    pop_2021 + pop_sqm_2021 + pop_growth +
                    (1|la_code),
                  data = df_econ, REML = FALSE)
summary(econ_con4)

anova(econ_multi, econ_con4)

lmer_coefs(econ_con4)

# cross level interaction ------------------------------------------------------

econ_out <- lmer(lr_scale ~ (own_outright * affordability) +
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_out)

anova(econ_con4, econ_out)

lmer_coefs(econ_out)

econ_own <- lmer(lr_scale ~ (own_mortgage * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_outright + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_own)

anova(econ_con4, econ_own)

lmer_coefs(econ_own)

econ_soc <- lmer(lr_scale ~ (social_housing * affordability) + 
                  white_british + 
                  no_religion + uni +
                  own_outright +
                  private_renting + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 + 
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_soc)

anova(econ_con4, econ_soc)

lmer_coefs(econ_soc)

econ_pri <- lmer(lr_scale ~ (private_renting * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  own_outright + own_mortgage + age + 
                  soc_class + non_uk_born + gdp_growth_pct_5 + 
                  pop_sqm_2021 + pop_growth +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_pri)

anova(econ_con4, econ_pri)

lmer_coefs(econ_pri)

###############################################################################
# redistself ------------------------------------------------------------------
###############################################################################

# ols null model
redist_fit <- lm(redistSelf ~ 1, data = df_redist)

# lmer null model
redist_lmer <- lmer(redistSelf ~ (1|la_code), data = df_redist)

logLik(redist_fit)
logLik(redist_lmer)
2 * (logLik(redist_lmer) - logLik(redist_fit))

# random intercepts
ranef(redist_lmer)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# multivariate ------------------------------------------------

redist_multi <- lmer(redistSelf ~ white_british + 
                     no_religion + uni +
                     own_outright + social_housing +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born +
                     (1|la_code),
                   data = df_redist, REML = FALSE)

summary(redist_multi)

# including level 2 predictors  ------------------------------

redist_con <- lmer(redistSelf ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_capita +
                   (1|la_code),
                 data = df_redist, REML = FALSE)
summary(redist_con)

anova(redist_multi, redist_con)

redist_con2 <- lmer(redistSelf ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability + gdp_growth_pct +
                    (1|la_code),
                  data = df_redist, REML = FALSE)
summary(redist_con2)

anova(redist_multi, redist_con2)

redist_con3 <- lmer(redistSelf ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                    (1|la_code),
                  data = df_redist, REML = FALSE)
summary(redist_con3)

anova(redist_multi, redist_con3)

redist_con4 <- lmer(redistSelf ~ white_british + 
                      no_religion + uni +
                      own_outright + social_housing +
                      private_renting + own_mortgage + age + 
                      soc_class + non_uk_born + affordability + 
                      pop_sqm_2021 + pop_growth +
                      (1|la_code),
                    data = df_redist, REML = FALSE)
summary(redist_con4)

anova(redist_multi, redist_con4)

lmer_coefs(redist_con4)

# cross level interaction ------------------------------------------------------

redist_out <- lmer(redistSelf ~ (own_outright * affordability) +
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_growth_pct_5 +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_redist, REML = FALSE)
summary(redist_out)

anova(redist_con4, redist_out)

lmer_coefs(redist_out)

redist_own <- lmer(redistSelf ~ (own_mortgage * affordability) + 
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   private_renting + own_outright + age + 
                   soc_class + non_uk_born + gdp_growth_pct_5 +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_redist, REML = FALSE)
summary(redist_own)

anova(redist_con4, redist_own)

lmer_coefs(redist_own)

redist_soc <- lmer(redistSelf ~ (social_housing * affordability) + 
                   white_british + 
                   no_religion + uni +
                   own_outright +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_growth_pct_5 +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_redist, REML = FALSE)
summary(redist_soc)

anova(redist_con4, redist_soc)

lmer_coefs(redist_soc)

redist_pri <- lmer(redistSelf ~ (private_renting * affordability) + 
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   own_outright + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_growth_pct_5 + 
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_redist, REML = FALSE)
summary(redist_pri)

anova(redist_con4, redist_pri)

lmer_coefs(redist_pri)

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

# random intercepts
ranef(immi_lmer)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# multivariate ------------------------------------------------

immi_multi <- lmer(immigSelf ~ white_british + 
                       no_religion + uni +
                       own_outright + social_housing +
                       private_renting + own_mortgage + age + 
                       soc_class + non_uk_born +
                       (1|la_code),
                     data = df_immi, REML = FALSE)

summary(immi_multi)

# including level 2 predictors  ------------------------------

immi_con <- lmer(immigSelf ~ white_british + 
                     no_religion + uni +
                     own_outright + social_housing +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born + affordability + gdp_capita +
                     (1|la_code),
                   data = df_immi, REML = FALSE)
summary(immi_con)

anova(immi_multi, immi_con)

immi_con2 <- lmer(immigSelf ~ white_british + 
                      no_religion + uni +
                      own_outright + social_housing +
                      private_renting + own_mortgage + age + 
                      soc_class + non_uk_born + affordability + gdp_growth_pct +
                      (1|la_code),
                    data = df_immi, REML = FALSE)
summary(immi_con2)

anova(immi_multi, immi_con2)

immi_con3 <- lmer(immigSelf ~ white_british + 
                      no_religion + uni +
                      own_outright + social_housing +
                      private_renting + own_mortgage + age + 
                      soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                      (1|la_code),
                    data = df_immi, REML = FALSE)
summary(immi_con3)

anova(immi_multi, immi_con3)

immi_con4 <- lmer(immigSelf ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability +
                    pop_sqm_2021 + pop_growth +
                    (1|la_code),
                  data = df_immi, REML = FALSE)
summary(immi_con4)

anova(immi_multi, immi_con4)

lmer_coefs(immi_con4)

# cross level interaction ------------------------------------------------------

immi_out <- lmer(immigSelf ~ (own_outright * affordability) +
                     white_british + 
                     no_religion + uni +
                     social_housing +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born + gdp_growth_pct_5 +
                     pop_sqm_2021 + pop_growth +
                     (1|la_code),
                   data = df_immi, REML = FALSE)
summary(immi_out)

anova(immi_con4, immi_out)

lmer_coefs(immi_out)

immi_own <- lmer(immigSelf ~ (own_mortgage * affordability) + 
                     white_british + 
                     no_religion + uni +
                     social_housing +
                     private_renting + own_outright + age + 
                     soc_class + non_uk_born + gdp_growth_pct_5 +
                     pop_sqm_2021 + pop_growth +
                     (1|la_code),
                   data = df_immi, REML = FALSE)
summary(immi_own)

anova(immi_con4, immi_own)

lmer_coefs(immi_own)

immi_soc <- lmer(immigSelf ~ (social_housing * affordability) + 
                     white_british + 
                     no_religion + uni +
                     own_outright +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born + gdp_growth_pct_5 +
                     pop_sqm_2021 + pop_growth +
                     (1|la_code),
                   data = df_immi, REML = FALSE)
summary(immi_soc)

anova(immi_con4, immi_soc)

lmer_coefs(immi_soc)

immi_pri <- lmer(immigSelf ~ (private_renting * affordability) + 
                     white_british + 
                     no_religion + uni +
                     social_housing +
                     own_outright + own_mortgage + age + 
                     soc_class + non_uk_born + gdp_growth_pct_5 +
                     pop_sqm_2021 + pop_growth +
                     (1|la_code),
                   data = df_immi, REML = FALSE)
summary(immi_pri)

anova(immi_con4, immi_pri)

lmer_coefs(immi_pri)

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

# random intercepts
ranef(tory_lmer)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# multivariate ------------------------------------------------

tory_multi <- glmer(tory_2019 ~ white_british + 
                     no_religion + uni +
                     own_outright + social_housing +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born +
                     (1|la_code),
                   data = df_tory, family = binomial("logit"))

summary(tory_multi)

# including level 2 predictors  ------------------------------

tory_con <- glmer(tory_2019 ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + affordability + gdp_capita +
                   (1|la_code),
                 data = df_tory, family = binomial("logit"))
summary(tory_con)

anova(tory_multi, tory_con)

tory_con2 <- glmer(tory_2019 ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability + gdp_growth_pct +
                    (1|la_code),
                  data = df_tory, family = binomial("logit"))
summary(tory_con2)

anova(tory_multi, tory_con2)

tory_con3 <- glmer(tory_2019 ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class + non_uk_born + affordability + gdp_growth_pct_5 +
                    (1|la_code),
                  data = df_tory, family = binomial("logit"))
summary(tory_con3)

anova(tory_multi, tory_con3)

tory_con4 <- glmer(tory_2019 ~ white_british + 
                     no_religion + uni +
                     own_outright + social_housing +
                     private_renting + own_mortgage + age + 
                     soc_class + non_uk_born + affordability + 
                     pop_sqm_2021 + pop_growth +
                     (1|la_code),
                   data = df_tory, family = binomial("logit"))
summary(tory_con4)

anova(tory_multi, tory_con4)

# cross level interaction ------------------------------------------------------

tory_out <- glmer(tory_2019 ~ (own_outright * affordability) +
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_capita +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_tory, family = binomial("logit"))
summary(tory_out)

anova(tory_con4, tory_out)

tory_own <- glmer(tory_2019 ~ (own_mortgage * affordability) + 
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   private_renting + own_outright + age + 
                   soc_class + non_uk_born + gdp_capita +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_tory, family = binomial("logit"))
summary(tory_own)

anova(tory_con4, tory_own)

tory_soc <- glmer(tory_2019 ~ (social_housing * affordability) + 
                   white_british + 
                   no_religion + uni +
                   own_outright +
                   private_renting + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_capita +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_tory, family = binomial("logit"))
summary(tory_soc)

anova(tory_con4, tory_soc)

tory_pri <- glmer(tory_2019 ~ (private_renting * affordability) + 
                   white_british + 
                   no_religion + uni +
                   social_housing +
                   own_outright + own_mortgage + age + 
                   soc_class + non_uk_born + gdp_capita +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_tory, family = binomial("logit"))
summary(tory_pri)

anova(tory_con4, tory_pri)
