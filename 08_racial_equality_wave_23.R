pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest)

rm(list = ls())

# lmer_coefs function ---------------------------------------------------------

source("05_lmer_coefs_function.R")

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W23_v24.0.dta")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

# ind vars ------------------------------------------

df %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(df %>% 
               select(starts_with("p_")),
             .data[[.x]]))

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
      "Other" = c("8")
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
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$redistSelf[df$redistSelf == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age <- rescale01(df$age, na.rm = T)
df$blackEquality[df$blackEquality == 9999] <- NA

df %>% 
  count(tory_2019, p_turnout_2019, p_past_vote_2019)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(redistSelf)
df %>% count(immigSelf)
df %>% 
  count(blackEquality) %>% 
  mutate(perc = n / sum(n),
         cum_perc = cumsum(perc))

# black equality by immigSelf and al_scale ------------------------------

df %>% 
  mutate(immigSelf = 10-immigSelf) %>% 
  filter(blackEquality != 9999 & !is.na(immigSelf)) %>% 
  ggplot(aes(x = immigSelf,
             y = blackEquality)) +
  geom_jitter() +
  geom_smooth()

df %>% 
  filter(!is.na(blackEquality) & !is.na(al_scale)) %>% 
  ggplot(aes(x = al_scale,
             y = blackEquality)) +
  geom_jitter() +
  geom_smooth()

# missing values -------------------------------

# df_con
df %>% 
  select(blackEquality, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_blk <- df %>% 
  select(blackEquality, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit()

###############################################################################
# black equality ------------------------------------------------------------
##############################################################################

# ols null model
ols_fit <- lm(blackEquality ~ 1, data = df_blk)

# lmer null model
lmer_fit <- lmer(blackEquality ~ (1|la_code), data = df_blk)

logLik(ols_fit)
logLik(lmer_fit)
2 * (logLik(lmer_fit) - logLik(ols_fit))

# random intercepts
ranef(lmer_fit)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

blk_multi <- lmer(blackEquality ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    c1_c2 + d_e  + non_uk_born +
                    (1|la_code),
                  data = df_blk, REML = FALSE)

summary(blk_multi)

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2022`) %>% 
  select(la_code, affordability)

# merging
df_blk <- df_blk %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_blk %>% 
  map_int(~sum(is.na(.)))

df_blk <- df_blk %>% na.omit()

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

names(gdp)

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

df_blk <- df_blk %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>%
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_blk %>% 
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
  select(la_code, pop_2021, pop_growth, pop_sqm_2021)

pop$pop_growth <- rescale01(pop$pop_growth, na.rm = T)

df_blk <- df_blk %>% 
  left_join(pop, by = "la_code")

df_blk %>% map_int(~sum(is.na(.)))

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

df_blk <- df_blk %>% 
  left_join(ethnic, by = "la_code")

df_blk %>% map_int(~sum(is.na(.)))

# rerunning minus scotland ---------------------------------------------------

blk_multi <- lmer(blackEquality ~ white_british + 
                    no_religion + uni +
                    own_outright + social_housing +
                    private_renting + own_mortgage + age + 
                    c1_c2 + d_e + non_uk_born +
                    (1|la_code),
                  data = df_blk, REML = FALSE)

summary(blk_multi)

# including level 2 predictors  ------------------------------

blk_con <- lmer(blackEquality ~ white_british + 
                  no_religion + uni +
                  own_outright + social_housing +
                  private_renting + own_mortgage + age + 
                  c1_c2 + d_e + non_uk_born + affordability + gdp_capita +
                  (1|la_code),
                data = df_blk, REML = FALSE)
summary(blk_con)

anova(blk_multi, blk_con)

blk_con2 <- lmer(blackEquality ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   c1_c2 + d_e + non_uk_born + affordability + gdp_growth_pct +
                   (1|la_code),
                 data = df_blk, REML = FALSE)
summary(blk_con2)

anova(blk_multi, blk_con2)

blk_con3 <- lmer(blackEquality ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   c1_c2 + d_e + non_uk_born + affordability + gdp_growth_pct_5 +
                   (1|la_code),
                 data = df_blk, REML = FALSE)
summary(blk_con3)

anova(blk_multi, blk_con3)

blk_con4 <- lmer(blackEquality ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   c1_c2 + d_e + non_uk_born + affordability + #pop_2021 +
                   pop_sqm_2021 + pop_growth +
                   (1|la_code),
                 data = df_blk, REML = FALSE)
summary(blk_con4)

anova(blk_multi, blk_con4)

blk_con5 <- lmer(blackEquality ~ white_british + 
                   no_religion + uni +
                   own_outright + social_housing +
                   private_renting + own_mortgage + age + 
                   c1_c2 + d_e + non_uk_born + affordability + 
                   pop_sqm_2021 + 
                   pop_growth + 
                   white_perc +
                   (1|la_code),
                 data = df_blk, REML = FALSE)
summary(blk_con5)

anova(blk_multi, blk_con5)

lmer_coefs(blk_con5)

# cross level interaction ------------------------------------------------------

df_blk <- df_blk %>% 
  mutate(own_outright.affordability = own_outright * affordability)

blk_out <- lmer(blackEquality ~ own_outright + affordability +
                  own_outright.affordability +
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_mortgage + age + 
                  c1_c2 + d_e + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth + 
                  white_perc +
                  (1|la_code),
                data = df_blk, REML = FALSE)
summary(blk_out)

anova(blk_con5, blk_out)

lmer_coefs(blk_out)

blk_own <- lmer(blackEquality ~ (own_mortgage * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  private_renting + own_outright + age + 
                  c1_c2 + d_e + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth + white_perc +
                  (1|la_code),
                data = df_blk, REML = FALSE)
summary(blk_own)

anova(blk_con5, blk_own)

lmer_coefs(blk_own)

blk_soc <- lmer(blackEquality ~ (social_housing * affordability) + 
                  white_british + 
                  no_religion + uni +
                  own_outright +
                  private_renting + own_mortgage + age + 
                  c1_c2 + d_e + non_uk_born + gdp_growth_pct_5 +
                  pop_sqm_2021 + pop_growth + white_perc +
                  (1|la_code),
                data = df_blk, REML = FALSE)
summary(blk_soc)

anova(blk_con4, blk_soc)

lmer_coefs(blk_soc)

blk_pri <- lmer(blackEquality ~ (private_renting * affordability) + 
                  white_british + 
                  no_religion + uni +
                  social_housing +
                  own_outright + own_mortgage + age + 
                  c1_c2 + d_e + non_uk_born + gdp_growth_pct_5 + 
                  pop_sqm_2021 + pop_growth + white_perc +
                  (1|la_code),
                data = df_blk, REML = FALSE)
summary(blk_pri)

anova(blk_con4, blk_pri)

lmer_coefs(blk_pri)

## data viz ----------------------------

blk_dummy <- expand.grid(
  white_british = c(mean(df_blk$white_british)),
  no_religion = c(mean(df_blk$no_religion)),
  uni = c(mean(df_blk$uni)),
  social_housing = c(mean(df_blk$social_housing)),
  private_renting = c(mean(df_blk$private_renting)),
  own_mortgage = c(mean(df_blk$own_mortgage)),
  age = mean(df_blk$age),
  c1_c2 = c(mean(df_blk$c1_c2)),
  d_e = c(mean(df_blk$d_e)),
  non_uk_born = c(mean(df_blk$non_uk_born)),
  gdp_growth_pct_5 = c(mean(df_blk$gdp_growth_pct_5)),
  pop_sqm_2021 = c(mean(df_blk$pop_sqm_2021)),
  pop_growth = c(mean(df_blk$pop_growth)),
  white_perc = c(mean(df_blk$white_perc)),
  own_outright = c(0,1),
  affordability = seq(0, 1, 0.2)
) %>% 
  mutate(own_outright.affordability = own_outright * affordability)

acov <- vcov(blk_out)

fixed <- summary(blk_out)$coefficients[,"Estimate"]

vars_order <- names(fixed)[-1]

xmat <- blk_dummy %>%
  mutate(int = 1, .before = 1) %>%
  select(int, all_of(vars_order)) %>%
  as.matrix()

blk_dummy$fit <- xmat %*% fixed

blk_dummy$SE <- xmat %*% acov %*% t(xmat) %>%
  diag() %>%
  sqrt()

blk_dummy <- blk_dummy %>%
  mutate(LL = fit - qnorm(0.975)*SE,
         UL = fit + qnorm(0.975)*SE)

pacman::p_load(patchwork)

p1 <- blk_dummy %>% 
  ggplot(aes(x = affordability, y = fit,
             colour = as.factor(own_outright))) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = as.factor(own_outright)),
              colour = "lightgrey", alpha = 0.25) +
  geom_line(linewidth = 2.5) +
  theme_bw() +
  drop_gridlines() +
  scale_colour_viridis_d(option = "D") +
  labs(x = "Affordability ratio",
       y = "Racial equality (predicted values)",
       colour = "Own outright")

p2 <- df_blk %>% 
  ggplot(aes(x = affordability)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

p1 / p2  
