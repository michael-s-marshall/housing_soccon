pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest)

rm(list = ls())

# loading dataset ---------------------------------------------

load("cross_sectional_df.RData")

# lmer_coefs function ------------------------------------------

source("001_lmer_coefs_function.R")

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# missing observations --------------------------------

df_immi <- df %>% 
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni_full, -prices, -prices_raw)

df_immi %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(la_code) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$la_code
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(df_immi, affordability)

# missing degree pct is Scotland
missing_las(df_immi, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(df_immi, foreign_per_1000)

# removing NAs
df_immi <- df_immi %>% na.omit()

# modelling ---------------------------------------------

# ols null model
immi_fit <- lm(immigSelf ~ 1, data = df_immi)

# lmer null model
immi_lmer <- lmer(immigSelf ~ (1|la_code), data = df_immi, REML = F)

logLik(immi_fit)
logLik(immi_lmer)
2 * (logLik(immi_lmer) - logLik(immi_fit))

# multivariate ------------------------------------------------

immi_multi <- lmer(immigSelf ~ white_british + 
                     no_religion + uni +
                     social_housing + private_renting + 
                     homeowner + age + 
                     c1_c2 + d_e + non_uk_born +
                     (1|la_code),
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
                   degree_pct + manuf_pct +
                   (1|la_code),
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
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|la_code),
                 data = df_immi, REML = FALSE)
summary(immi_int)

anova(immi_con, immi_int)

# plotting coefficients ------------------------------------------------

plot_names <- tibble(
  term = names(fixef(immi_int))[-1],
  var_name = c("Social housing", "Affordability", "Homeowner",
               "White British", "No religion", "University graduate",
               "Private renter", "Age", "Social class: C1-C2",
               "Social class: D-E", "Non-UK born", "GDP per capita",
               "Population density", "Non-UK born population", "Over 65 %", "Under 15 %",
               "Graduate %", "Manufacturing %", "Affordability:Social housing",
               "Affordability:Homeowner"),
  grouping = c("Housing", "Housing", "Housing",
               "Individual","Individual", "Individual",
               "Housing", "Individual", "Individual", 
               "Individual", "Individual", "Local", 
               "Local", "Local", "Local", "Local",
               "Local", "Local","Housing", "Housing")
) %>% 
  mutate(grouping = fct_relevel(as.factor(grouping), 
                                c("Housing", "Individual",
                                  "Local")))

lmer_coefs(immi_int, "boot", plot_names)

# robustness check - with uni predictions ------------------------------

df_immi_preds <- df %>% 
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni, -prices, -prices_raw)

df_immi_preds %>% map_int(~sum(is.na(.)))

# removing NAs
df_immi_preds <- df_immi_preds %>% na.omit()

immi_int_preds <- lmer(immigSelf ~ (social_housing * affordability) +
                         (homeowner * affordability) +
                         white_british +
                         no_religion + uni_full +
                         private_renting + age +
                         c1_c2 + d_e + non_uk_born +
                         gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                         over_65_pct + under_15_pct +
                         degree_pct + manuf_pct +
                         (1|la_code),
                       data = df_immi_preds, REML = FALSE)
summary(immi_int_preds)

# robustness check - log scale -------------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log) +
                   (homeowner * affordability_log) +
                   white_british + 
                   no_religion + uni +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|la_code),
                 data = df_immi, REML = FALSE)
summary(immi_log)

AIC(immi_int, immi_log)

# robustness check - with prices -------------------------------------

df_immi_price <- df %>% 
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni_full, -affordability, -affordability_raw,
         -affordability_log, -affordability_log_raw)

df_immi_price %>% map_int(~sum(is.na(.)))

# removing NAs
df_immi_price <- df_immi_price %>% na.omit()

immi_int_price <- lmer(immigSelf ~ (social_housing * prices) +
                         (homeowner * prices) +
                         white_british +
                         no_religion + uni +
                         private_renting + age +
                         c1_c2 + d_e + non_uk_born +
                         gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                         over_65_pct + under_15_pct +
                         degree_pct + manuf_pct +
                         (1|la_code),
                       data = df_immi_price, REML = FALSE)
summary(immi_int_price)

# visualising interaction term ------------------------------

# making interaction terms with raw values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

immi_viz <- lmer(immigSelf ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability_raw +
                   white_british + 
                   no_religion + uni +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct + (1|la_code),
                 data = df_immi, REML = FALSE)

# anti immigration among social housing tenants 
x_scale <- seq(min(df_immi$affordability_raw), 
               max(df_immi$affordability_raw), 
               (max(df_immi$affordability_raw) - min(df_immi$affordability_raw))/5)

immi_dummy <- expand.grid(
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  uni = c(mean(df_immi$uni)),
  homeowner = c(0,1),
  private_renting = c(mean(df_immi$private_renting)),
  age = mean(df_immi$age),
  c1_c2 = c(mean(df_immi$c1_c2)),
  d_e = c(mean(df_immi$d_e)),
  non_uk_born = c(mean(df_immi$non_uk_born)),
  gdp_capita = c(mean(df_immi$gdp_capita)),
  pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
  foreign_per_1000 = c(mean(df_immi$foreign_per_1000)),
  over_65_pct = c(mean(df_immi$over_65_pct)),
  under_15_pct = c(mean(df_immi$under_15_pct)),
  degree_pct = c(mean(df_immi$degree_pct)),
  manuf_pct = c(mean(df_immi$manuf_pct)),
  social_housing = c(0,1),
  affordability_raw = x_scale
) %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

acov <- vcov(immi_viz)
fixed <- summary(immi_viz)$coefficients[,"Estimate"]
vars_order <- names(fixed)[-1]
xmat <- immi_dummy %>%
  mutate(int = 1, .before = 1) %>%
  select(int, all_of(vars_order)) %>%
  as.matrix()

immi_dummy$fit <- xmat %*% fixed
immi_dummy$SE <- xmat %*% acov %*% t(xmat) %>%
  diag() %>%
  sqrt()

immi_dummy <- immi_dummy %>%
  mutate(LL = fit - qnorm(0.975)*SE,
         UL = fit + qnorm(0.975)*SE)

pacman::p_load(patchwork)

p1 <- immi_dummy %>%
  mutate(
    tenure = ifelse(social_housing == 1 & homeowner == 0, "Social housing",
                    ifelse(homeowner == 1 & social_housing == 0, "Homeowner",
                           ifelse(social_housing == 0 & homeowner == 0, "Control",
                                  "remove")))
  ) %>% 
  filter(tenure != "remove") %>% 
  mutate(tenure = fct_drop(tenure)) %>% 
  ggplot(aes(x = affordability_raw, y = fit,
             colour = tenure)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = tenure, fill = tenure,
                  colour = NULL),
              alpha = 0.2) +
  geom_line(linewidth = 2.5) +
  theme_bw() +
  drop_gridlines() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Affordability ratio",
       y = "Anti-immigration (predicted values)",
       colour = "Tenure",
       fill = "Tenure") +
  coord_cartesian(ylim = c(5.5,8.5)) +
  theme(legend.position = "top")

p2 <- df_immi %>% 
  ggplot(aes(x = affordability_raw)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, 
                 colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

p1 / p2
