# script to run efa and cfa on soccon and left-right variables ----------------

# loading libraries -----------------------------------------------------------

pacman::p_load(tidyverse, lavaan, psych, haven)

# loading data ----------------------------------------------------------------

df <- read_dta("BES_2019/bes_rps_2019_1.1.1.dta")

df %>% 
  select(starts_with("f01")) %>%
  names() %>% 
  map(~count(
    df %>%
      select(starts_with("f01")),
    .data[[.x]])
    )

# missing values as NAs ---------------------------------------------------

missval <- c(-999, -1, -2)

miss_to_na <- function(x, missvals){
  out <- ifelse(x %in% missvals, NA, x)
}

df_nas <- df %>% 
  map_df(miss_to_na, missvals = missval)

# efa --------------------------------------------------------------------

# selecting vars and removing NAs
efa_vars <- df_nas %>% 
  select(starts_with("f01")) %>% 
  na.omit()

# scree plots
scree(efa_vars, pc=FALSE)
fa.parallel(efa_vars, fa="fa")

# fit with 2 factors
fit2 <- factanal(efa_vars, 2, rotation="promax")
print(fit2, digits=2, cutoff=0.32, sort = T)

# fit with 3 factors
fit3 <- factanal(efa_vars, 3, rotation="promax")
print(fit3, digits=2, cutoff=0.32, sort = T)

# fit with 4 factors
fit4 <- factanal(efa_vars, 4, rotation="promax")
print(fit4, digits=2, cutoff=0.32, sort = T)

# cfa -----------------------------------------------------------------

pol_model <- 'soc_con =~ f01_3 + f01_11 + f01_12
              right_left =~ f01_1 + f01_5 + f01_6'

cfa_2 <- cfa(pol_model, data = efa_vars, ordered = T)

summary(cfa_2, fit.measures = T, standardized = T)

# making scales -----------------------------------------------------

keys_list <- list(soc_con = c("f01_3","f01_11","f01_12"),
                  econ_right = c("f01_1","f01_5","f01_6"))
scores <- scoreItems(keys_list, df_nas, min=1, max=5)
scores$alpha

rm(keys_list, scores)

keys_list <- list(soc_con = c("f01_3","f01_11","f01_12"))
scores <- scoreItems(keys_list, df_nas, min=1, max=5)
scores$alpha
scores$scores[scores$missing > 0] <- NA  #get rid of cases with missing data
describe(scores$scores)
df_nas$soc_con <- c(scale(scores$scores))

# visualisation of soc_con with independent vars ----------------------------

df_nas %>% 
  mutate(
    tenure = fct_recode(as.factor(y03),
                        "Own outright" = "1",
                        "Own with mortgage" = "2",
                        "Local authority" = "3",
                        "Private renting" = "4",
                        "Housing association" = "5",
                        "Other" = "6")
  ) %>% 
  ggplot(aes(x = tenure, y = soc_con)) +
  geom_boxplot()

df_nas %>% 
  ggplot(aes(x = as.factor(y01_Annual), y = soc_con)) +
  geom_boxplot()

df_nas %>% 
  ggplot(aes(x = as.factor(y06), y = soc_con)) +
  geom_boxplot()

df_nas %>% 
  ggplot(aes(x = as.factor(y09), y = soc_con)) +
  geom_boxplot()

df_nas %>% 
  ggplot(aes(x = Age, y = soc_con)) +
  geom_boxplot(mapping = aes(group = cut_width(Age, 5))) +
  geom_smooth()

df_nas %>% 
  mutate(
    white_british = y11 == 1
  ) %>% 
  ggplot(aes(x = white_british, y = soc_con)) +
  geom_boxplot()

df_nas %>% 
  mutate(
    edlevel = fct_recode(as.factor(edlevel),
                         "No qualifications" = "0",
                         "Below GCSE" = "1",
                         "GCSE" = "2",
                         "A-level" = "3",
                         "Undergraduate" = "4",
                         "Postgrad" = "5")
  ) %>% 
  ggplot(aes(x = edlevel, y = soc_con)) +
  geom_boxplot()

pacman::p_load(ggridges)

df %>% count(region)

df_nas %>% 
  mutate(
    region = fct_recode(
      as.factor(region),
      "East midlands" = "1",
      "Eastern" = "2",
      "London" = "3",
      "North East" = "4",
      "North West" = "5",
      "Scotland" = "6",
      "South East" = "7",
      "South West" = "8",
      "Wales" = "9",
      "West Midlands" = "10",
      "Yorkshire & Humber" = "11"
    )
  ) %>% 
  ggplot(aes(x = soc_con, y = fct_reorder(region, soc_con))) +
  geom_density_ridges(scale = 1, quantile_lines = T)

# making predictor variables ---------------------------------------

df_nas$white_british <- df_nas$y11 == 1
df_nas$religion <- df_nas$y06 == 1
df_nas$male <- df_nas$y09 == 1
df_nas$higher_ed <- df_nas$edlevel %in% c(4,5)
df_nas <- df_nas %>% 
  mutate(
    tenure = fct_collapse(as.factor(y03),
                        "Own outright" = "1",
                        "Own with mortgage" = "2",
                        "Social housing" = c("3","5"),
                        "Private renting" = "4",
                        "Other" = "6")
  )

df_nas %>% 
  count(tenure, y03)

df_nas %>% 
  ggplot(aes(x = tenure, y = soc_con)) +
  geom_boxplot()

# lm  and lmer null models ----------------------------------------------------

pacman::p_load(lme4, lmerTest)

df_lmer <- df_nas %>% 
  select(soc_con, white_british, y01_Annual,
         religion, male, higher_ed, tenure, Age, region,
         LA_UA_Code, e01, g01_1) %>% 
  mutate(
    region = fct_recode(
      as.factor(region),
      "East midlands" = "1",
      "Eastern" = "2",
      "London" = "3",
      "North East" = "4",
      "North West" = "5",
      "Scotland" = "6",
      "South East" = "7",
      "South West" = "8",
      "Wales" = "9",
      "West Midlands" = "10",
      "Yorkshire & Humber" = "11"
    ),
    la_code = as.factor(LA_UA_Code)
  ) %>% 
  rename(income = y01_Annual,
         left_right = e01,
         tax_spend = g01_1)

df_lmer %>% map_int(~sum(is.na(.)))

df_lmer <- df_lmer %>% na.omit()

# ols null model
ols_fit <- lm(soc_con ~ 1, data = df_lmer)

# lmer null model
lmer_fit <- lmer(soc_con ~ (1|la_code), data = df_lmer)

logLik(ols_fit)
logLik(lmer_fit)
2 * (logLik(lmer_fit) - logLik(ols_fit))

# random intercepts
ranef(lmer_fit)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# lmer model with predictors ------------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

df_lmer$income <- rescale01(df_lmer$income, na.rm = T)

lmer_multi <- lmer(soc_con ~ white_british + income +
                     religion + male + higher_ed +
                     tenure + Age + (1|la_code),
                   data = df_lmer, REML = FALSE)

summary(lmer_multi)
summ(lmer_multi)

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2019`) %>% 
  select(la_code, affordability)

# merging
df_lmer <- df_lmer %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_lmer %>% 
  map_int(~sum(is.na(.)))

df_lmer %>% 
  group_by(region) %>% 
  summarise(mean_affordability = mean(affordability, na.rm = T))

# removing scotland
df_lmer <- df_lmer %>% na.omit()

# rerunning minus scotland ---------------------------------------------------

lmer_multi <- lmer(soc_con ~ white_british + income +
                     religion + male + higher_ed +
                     tenure + Age + (1|la_code),
                   data = df_lmer, REML = FALSE)

summary(lmer_multi)

df_lmer$own_mortgage <- df_lmer$tenure == "Own with mortgage"
df_lmer$social_housing <- df_lmer$tenure == "Social housing"
df_lmer$private_renting <- df_lmer$tenure == "Private renting"

lmer_multi2 <- lmer(soc_con ~ white_british + income +
                     religion + male + higher_ed +
                     own_mortgage + social_housing + 
                     Age + (1|la_code),
                   data = df_lmer, REML = FALSE)

summary(lmer_multi2)

# including level 2 predictor for affordability ------------------------------

lmer_con <- lmer(soc_con ~ white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + social_housing + 
                   Age + affordability + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_con)

anova(lmer_multi2, lmer_con)

# cross level interaction ------------------------------------------------------

lmer_mor <- lmer(soc_con ~ (own_mortgage * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   social_housing + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_mor)

anova(lmer_con, lmer_mor)

lmer_soc <- lmer(soc_con ~ (social_housing * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_soc)

anova(lmer_con, lmer_soc)

###############################################################################
# left_right dimension --------------------------------------------------------
###############################################################################

# ols null model
ols_fit <- lm(left_right ~ 1, data = df_lmer)

# lmer null model
lmer_fit <- lmer(left_right ~ (1|la_code), data = df_lmer)

logLik(ols_fit)
logLik(lmer_fit)
2 * (logLik(lmer_fit) - logLik(ols_fit))

# random intercepts
ranef(lmer_fit)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# lmer model with predictors ------------------------------------------------

lmer_multi <- lmer(left_right ~ white_british + income +
                     religion + male + higher_ed +
                     tenure + Age + (1|la_code),
                   data = df_lmer, REML = FALSE)

summary(lmer_multi)
summ(lmer_multi)

# including level 2 predictor for affordability ------------------------------

lmer_con <- lmer(left_right ~ white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + social_housing + 
                   Age + affordability + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_con)

anova(lmer_multi, lmer_con)

# cross level interaction ------------------------------------------------------

lmer_mor <- lmer(left_right ~ (own_mortgage * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   social_housing + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_mor)

anova(lmer_con, lmer_mor)

lmer_soc <- lmer(left_right ~ (social_housing * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_soc)

anova(lmer_con, lmer_soc)

###############################################################################
# tax_spend dimension --------------------------------------------------------
###############################################################################

# ols null model
ols_fit <- lm(tax_spend ~ 1, data = df_lmer)

# lmer null model
lmer_fit <- lmer(tax_spend ~ (1|la_code), data = df_lmer)

logLik(ols_fit)
logLik(lmer_fit)
2 * (logLik(lmer_fit) - logLik(ols_fit))

# random intercepts
ranef(lmer_fit)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# lmer model with predictors ------------------------------------------------
lmer_multi <- lmer(tax_spend ~ white_british + income +
                     religion + male + higher_ed +
                     tenure + Age + (1|la_code),
                   data = df_lmer, REML = FALSE)

summary(lmer_multi)
summ(lmer_multi)

# including level 2 predictor for affordability ------------------------------
lmer_con <- lmer(tax_spend ~ white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + social_housing + 
                   Age + affordability + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_con)

anova(lmer_multi, lmer_con)

# cross level interaction ------------------------------------------------------
lmer_mor <- lmer(tax_spend ~ (own_mortgage * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   social_housing + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_mor)

anova(lmer_con, lmer_mor)

lmer_soc <- lmer(tax_spend ~ (social_housing * affordability) + 
                   white_british + income +
                   religion + male + higher_ed +
                   own_mortgage + 
                   Age + (1|la_code),
                 data = df_lmer, REML = FALSE)
summary(lmer_soc)

anova(lmer_con, lmer_soc)
