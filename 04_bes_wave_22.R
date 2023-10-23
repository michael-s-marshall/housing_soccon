pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest)

rm(list = ls())

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

# p_edlevel, p_ethnicity, p_religion, p_gross_household, p_housing, age, p_socgrade
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
    )
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

df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, social_housing, private_renting)

# missing values -------------------------------

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         soc_class, income, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  map_int(~sum(is.na(.)))

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         soc_class, income, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit() %>% 
  nrow()

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit() %>% 
  nrow()

# df_con
df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit() %>% 
  nrow()

# df_econ
df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit() %>% 
  nrow()

df_con <- df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit()

df_econ <- df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         soc_class, own_outright, own_mortgage, social_housing,
         private_renting, age) %>% 
  na.omit()

# social conservatism --------------------------------------------------

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
                    soc_class +
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

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

# removing scotland
df_con <- df_con %>% na.omit()

df_econ <- df_econ %>% na.omit()

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

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

# rerunning minus scotland ---------------------------------------------------

con_multi <- lmer(al_scale ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    soc_class +
                    (1|la_code),
                  data = df_con, REML = FALSE)

summary(con_multi)

# including level 2 predictors  ------------------------------

con_con <- lmer(al_scale ~ white_british + 
                  no_religion + uni +
                  own_outright + social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + affordability + gdp_capita +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_con)

anova(con_multi, con_con)

con_con2 <- lmer(al_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + affordability + gdp_growth_pct +
                   (1|la_code),
                 data = df_con, REML = FALSE)
summary(con_con2)

anova(con_multi, con_con2)

con_con3 <- lmer(al_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + affordability + gdp_growth_pct_5 +
                   (1|la_code),
                 data = df_con, REML = FALSE)
summary(con_con3)

anova(con_multi, con_con3)

# cross level interaction ------------------------------------------------------

con_out <- lmer(al_scale ~ (own_outright * affordability) +
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_out)

anova(con_con, con_out)

con_own <- lmer(al_scale ~ (own_mortgage * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_outright + age + 
                  soc_class + gdp_growth_pct_5 +
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_own)

anova(con_con, con_own)

con_soc <- lmer(al_scale ~ (social_housing * affordability) + 
                  white_british + 
                  no_religion + uni +
                  own_outright +
                  private_renting + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 + 
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_soc)

anova(con_con, con_soc)

con_pri <- lmer(al_scale ~ (private_renting * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  own_outright + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 + 
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_pri)

anova(con_con, con_pri)

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
                    soc_class +
                    (1|la_code),
                  data = df_econ, REML = FALSE)

summary(econ_multi)

# including level 2 predictors  ------------------------------

econ_con <- lmer(lr_scale ~ white_british + 
                  no_religion + uni +
                  own_outright + social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + affordability + gdp_capita +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_con)

anova(econ_multi, econ_con)

econ_con2 <- lmer(lr_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + affordability + gdp_growth_pct +
                   (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_con2)

anova(econ_multi, econ_con2)

econ_con3 <- lmer(lr_scale ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   soc_class + affordability + gdp_growth_pct_5 +
                   (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_con3)

anova(econ_multi, econ_con3)

# cross level interaction ------------------------------------------------------

econ_out <- lmer(lr_scale ~ (own_outright * affordability) +
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_out)

anova(econ_con, econ_out)

econ_own <- lmer(lr_scale ~ (own_mortgage * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_outright + age + 
                  soc_class + gdp_growth_pct_5 +
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_own)

anova(econ_con, econ_own)

econ_soc <- lmer(lr_scale ~ (social_housing * affordability) + 
                  white_british + 
                  no_religion + uni +
                  own_outright +
                  private_renting + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 + 
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_soc)

anova(econ_con, econ_soc)

econ_pri <- lmer(lr_scale ~ (private_renting * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  own_outright + own_mortgage + age + 
                  soc_class + gdp_growth_pct_5 + 
                  (1|la_code),
                data = df_econ, REML = FALSE)
summary(econ_pri)

anova(econ_con, econ_pri)
