# script to run efa and cfa on soccon and left-right variables ----------------

# loading libraries -----------------------------------------------------------

pacman::p_load(tidyverse, lavaan, psych, haven, jtools)

rm(list = ls())

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

# making scales -----------------------------------------------------

keys_list <- list(soc_con = c("f01_3","f01_11","f01_12"),
                  econ_right = c("f01_1","-f01_2","f01_5","f01_6","-f01_7"))
scores <- scoreItems(keys_list, df_nas, min=1, max=5)
scores$alpha
scores$scores[scores$missing > 0] <- NA  #get rid of cases with missing data
describe(scores$scores)
df_nas$soc_con <- c(scale(scores$scores[,"soc_con"]))
df_nas$econ_right <- c(scale(scores$scores[,"econ_right"]))

# making predictor variables ---------------------------------------

df_nas$supervisor <- df_nas$y22 == 1
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
                          "Other" = "6"),
    home_owner = tenure %in% c("Own outright", "Own with mortgage")
  )

# lm  and lmer null models ----------------------------------------------------

pacman::p_load(lme4, lmerTest)

df_lmer <- df_nas %>% 
  select(soc_con, white_british, y01_Annual,
         religion, male, higher_ed, tenure, Age, region,
         LA_UA_Code, e01, g01_1, econ_right, home_owner,
         supervisor) %>% 
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

df_con <- df_lmer %>%
  select(-income, -econ_right, -supervisor) %>% 
  na.omit()

df_econ <- df_lmer %>%
  select(-income, -soc_con, -supervisor) %>% 
  na.omit()

# ols null model
ols_fit <- lm(soc_con ~ 1, data = df_con)

# lmer null model
lmer_fit <- lmer(soc_con ~ (1|la_code), data = df_con)

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

df_con$own_mortgage <- df_con$tenure == "Own with mortgage"
df_con$social_housing <- df_con$tenure == "Social housing"
df_con$private_renting <- df_con$tenure == "Private renting"
df_con$own_outright <- df_con$tenure == "Own outright"
df_con$tenure_other <- df_con$tenure == "Other"

df_econ$own_mortgage <- df_econ$tenure == "Own with mortgage"
df_econ$social_housing <- df_econ$tenure == "Social housing"
df_econ$private_renting <- df_econ$tenure == "Private renting"
df_econ$own_outright <- df_econ$tenure == "Own outright"
df_econ$tenure_other <- df_econ$tenure == "Other"

con_multi <- lmer(soc_con ~ white_british + 
                    religion + male + higher_ed +
                    own_outright + social_housing +
                    private_renting + tenure_other + Age + 
                    (1|la_code),
                  data = df_con, REML = FALSE)

summary(con_multi)

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2019`) %>% 
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

df_con %>% 
  group_by(region) %>% 
  summarise(mean_affordability = mean(affordability, na.rm = T))

# removing scotland
df_con <- df_con %>% na.omit()

df_econ <- df_econ %>% na.omit()

# rerunning minus scotland ---------------------------------------------------

con_multi <- lmer(soc_con ~ white_british + 
                    religion + male + higher_ed +
                    own_outright + social_housing +
                    private_renting + tenure_other + Age + 
                    (1|la_code),
                  data = df_con, REML = FALSE)

summary(con_multi)

con_multi2 <- lmer(soc_con ~ white_british +
                     religion + male + higher_ed +
                     home_owner + social_housing + tenure_other +
                     Age + (1|la_code),
                   data = df_con, REML = FALSE)

summary(con_multi2)

# including level 2 predictor for affordability ------------------------------

con_con <- lmer(soc_con ~ white_british + 
                  religion + male + higher_ed +
                  own_outright + social_housing +
                  private_renting + tenure_other + Age + 
                  affordability + 
                  (1|la_code),
                data = df_con, REML = FALSE)
summary(con_con)

anova(con_multi, con_con)

# cross level interaction ------------------------------------------------------

con_out <- lmer(soc_con ~ (own_outright * affordability) +
                  white_british +
                  religion + male + higher_ed +
                  social_housing +
                  private_renting + tenure_other +
                  Age + (1|la_code),
                data = df_con, REML = FALSE)
summary(con_out)

anova(con_con, con_out)

con_own <- lmer(soc_con ~ (home_owner * affordability) + 
                  white_british +
                  religion + male + higher_ed +
                  social_housing + tenure_other +
                  Age + (1|la_code),
                data = df_con, REML = FALSE)
summary(con_own)

anova(con_con, con_own)

con_soc <- lmer(soc_con ~ (social_housing * affordability) + 
                  white_british +
                  religion + male + higher_ed +
                  own_outright + private_renting + tenure_other + 
                  Age + (1|la_code),
                data = df_con, REML = FALSE)
summary(con_soc)

anova(con_con, con_soc)

con_pri <- lmer(soc_con ~ (private_renting * affordability) + 
                  white_british +
                  religion + male + higher_ed +
                  own_outright + social_housing + tenure_other + 
                  Age + (1|la_code),
                data = df_con, REML = FALSE)
summary(con_pri)

anova(con_con, con_pri)

###############################################################################
# econ_right dimension --------------------------------------------------------
###############################################################################

# ols null model
econ_fit <- lm(econ_right ~ 1, data = df_econ)

# lmer null model
econ_lmer <- lmer(econ_right ~ (1|la_code), data = df_econ)

logLik(econ_fit)
logLik(econ_lmer)
2 * (logLik(econ_lmer) - logLik(econ_fit))

# random intercepts
ranef(econ_lmer)$la_code %>% 
  as_tibble() %>% 
  ggplot(aes(x = `(Intercept)`)) +
  geom_histogram(bins = 50, colour = "black", fill = "lightgrey")

# lmer model with predictors ------------------------------------------------

econ_multi <- lmer(econ_right ~ white_british +
                     religion + male + higher_ed +
                     own_outright + social_housing +
                     private_renting + tenure_other + 
                     Age + (1|la_code),
                   data = df_econ, REML = FALSE)

summary(econ_multi)

econ_multi2 <- lmer(econ_right ~ white_british +
                      religion + male + higher_ed +
                      home_owner + social_housing +
                      tenure_other + Age + (1|la_code),
                    data = df_econ, REML = FALSE)

summary(econ_multi2)

# including level 2 predictor for affordability ------------------------------

econ_con <- lmer(econ_right ~ white_british +
                   religion + male + higher_ed +
                   own_outright + social_housing + private_renting +
                   tenure_other +
                   Age + affordability + (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_con)

anova(econ_multi, econ_con)

# cross level interaction ------------------------------------------------------

econ_own <- lmer(econ_right ~ (own_outright * affordability) + 
                   white_british +
                   religion + male + higher_ed +
                   social_housing + private_renting + tenure_other +
                   Age + (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_own)

anova(econ_con, econ_own)

econ_soc <- lmer(econ_right ~ (social_housing * affordability) + 
                   white_british +
                   religion + male + higher_ed +
                   own_outright + private_renting + tenure_other +
                   Age + (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_soc)

anova(econ_con, econ_soc)

econ_pri <- lmer(econ_right ~ (private_renting * affordability) + 
                   white_british +
                   religion + male + higher_ed +
                   own_outright + social_housing + tenure_other +
                   Age + (1|la_code),
                 data = df_econ, REML = FALSE)
summary(econ_pri)

anova(econ_con, econ_pri)
