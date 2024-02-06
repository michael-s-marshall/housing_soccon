pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest, ggstance)

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
         -uni_full, -prices, -prices_raw, -uni) %>% 
  rename(LAD = la_code)

df_immi %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(LAD) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$LAD
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(df_immi, affordability)

# missing degree pct is Scotland
missing_las(df_immi, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(df_immi, foreign_per_1000)

# missing housing costs measures is Scotland
missing_las(df_immi, cost_ratio)
missing_las(df_immi, price_ratio)
missing_las(df_immi, hcli)

# removing NAs
df_eng_wales <- df_immi %>% drop_na(c(affordability, degree_pct, foreign_per_1000))

df_eng_wales %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% na.omit() 

1 - (nrow(df_immi) / nrow(df_eng_wales))

rm(df_eng_wales)

# modelling ---------------------------------------------

# ols null model
immi_fit <- lm(immigSelf ~ 1, data = df_immi)

# lmer null model
immi_lmer <- lmer(immigSelf ~ (1|LAD), data = df_immi, REML = F)

logLik(immi_fit)
logLik(immi_lmer)
2 * (logLik(immi_lmer) - logLik(immi_fit))

# multivariate ------------------------------------------------

immi_multi <- lmer(immigSelf ~ male + white_british + 
                     no_religion + edu_20plus +
                     social_housing + private_renting + 
                     homeowner + age + 
                     c1_c2 + d_e + non_uk_born +
                     (1|LAD),
                   data = df_immi, REML = FALSE)

summary(immi_multi)

# including level 2 predictors  ------------------------------

immi_con <- lmer(immigSelf ~ male + white_british + 
                   no_religion + edu_20plus +
                   social_housing + private_renting + 
                   homeowner + age + 
                   c1_c2 + d_e + non_uk_born + 
                   affordability + gdp_capita +
                   pop_sqm_2021 + foreign_per_1000  + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_con)

anova(immi_multi, immi_con)

# cross level interaction ------------------------------------------------------

immi_int <- lmer(immigSelf ~ (social_housing * affordability) +
                   (homeowner * affordability) +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_int)

anova(immi_con, immi_int)

saveRDS(immi_int, file = "working/markdown_data/immi_int.RDS")

# plotting coefficients ------------------------------------------------

plot_names <- tibble(
  term = names(fixef(immi_int))[-1],
  var_name = c("Social renter", "Affordability", "Homeowner", "Male",
               "White British", "No religion", "Education age 20+",
               "Private renter", "Age", "Social class: C1-C2",
               "Social class: D-E", "Non-UK born", "GDP per capita",
               "Population density", "Non-UK born population", "Over 65 %", "Under 15 %",
               "Graduate %", "Manufacturing %", "Affordability:Social renter",
               "Affordability:Homeowner"),
  grouping = c("Housing", "Housing", "Housing", "Individual",
               "Individual","Individual", "Individual",
               "Housing", "Individual", "Individual", 
               "Individual", "Individual", "Local", 
               "Local", "Local", "Local", "Local",
               "Local", "Local","Housing", "Housing")
) %>% 
  mutate(grouping = fct_relevel(as.factor(grouping), 
                                c("Housing", "Individual",
                                  "Local")))

immi_coefs <- lmer_coefs(immi_int, "profile", plot_names)

immi_coefs

saveRDS(immi_coefs, file = "working/markdown_viz/immi_coefs.RDS")

# robustness check - with uni predictions ------------------------------

df_immi_preds <- df %>% 
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni, -prices, -prices_raw)

df_immi_preds %>% map_int(~sum(is.na(.)))

# removing NAs
df_immi_preds <- df_immi_preds %>% na.omit() %>% 
  rename(LAD = la_code)

immi_int_preds <- lmer(immigSelf ~ (social_housing * affordability) +
                         (homeowner * affordability) +
                         male + white_british +
                         no_religion + uni_full +
                         private_renting + age +
                         c1_c2 + d_e + non_uk_born +
                         gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                         over_65_pct + under_15_pct +
                         degree_pct + manuf_pct +
                         (1|LAD),
                       data = df_immi_preds, REML = FALSE)
summary(immi_int_preds)

saveRDS(immi_int_preds, file = "working/markdown_data/immi_int_preds.RDS")

# robustness check - log scale -------------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log) +
                   (homeowner * affordability_log) +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_log)

AIC(immi_int, immi_log)

saveRDS(immi_log, file = "working/markdown_data/immi_log.RDS")

# robustness check - with prices -------------------------------------

df_immi_price <- df %>% 
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni_full, -affordability, -affordability_raw,
         -affordability_log, -affordability_log_raw, -uni) %>% 
  rename(LAD = la_code)

df_immi_price %>% map_int(~sum(is.na(.)))

# removing NAs
df_immi_price <- df_immi_price %>% na.omit()

immi_int_price <- lmer(immigSelf ~ (social_housing * prices) +
                         (homeowner * prices) +
                         male + white_british +
                         no_religion + edu_20plus +
                         private_renting + age +
                         c1_c2 + d_e + non_uk_born +
                         gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                         over_65_pct + under_15_pct +
                         degree_pct + manuf_pct +
                         (1|LAD),
                       data = df_immi_price, REML = FALSE)
summary(immi_int_price)

saveRDS(immi_int_price, file = "working/markdown_data/immi_int_price.RDS")

# robustness check - dummy for region ----------------------------------

immi_reg <- lmer(immigSelf ~ (social_housing * affordability) +
                   (homeowner * affordability) +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct + region_code +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_reg)

anova(immi_int, immi_reg) 

# incl. region makes very little difference to estimates
tibble(
  var = names(fixef(immi_int)),
  no_region = fixef(immi_int),
  incl_region = fixef(immi_reg)[!str_detect(names(fixef(immi_reg)),"region")]
) %>% 
  pivot_longer(cols = no_region:incl_region,
               names_to = "model",
               values_to = "estimate") %>% 
  filter(var != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = var, colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 3, position = position_dodgev(height = 0.5)) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  drop_y_gridlines()

# robustness check - education age < uni ---------------------------

df_uni <- df %>%
  select(-tory_2019, -uni_propensity, -uni_pred,
         -uni_full, -prices, -prices_raw) %>% 
  rename(LAD = la_code)

df_uni %>% select(uni, edu_20plus) %>% map_int(~sum(is.na(.)))

df_uni <- df_uni %>% select(-edu_20plus) %>% na.omit()

immi_uni <- lmer(immigSelf ~ (social_housing * affordability) +
                   (homeowner * affordability) +
                   male + white_british + 
                   no_religion + uni +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   (1|LAD),
                 data = df_uni, REML = FALSE)

summary(immi_uni)

# using uni makes very little difference to estimates
tibble(
  edu_model = fixef(immi_int),
  uni_model = fixef(immi_uni),
  var = names(fixef(immi_int))
) %>% 
  pivot_longer(cols = uni_model:edu_model,
               names_to = "model",
               values_to = "estimate") %>%
  filter(var != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = var, colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 3, position = position_dodgev(height = 0.5)) +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  drop_y_gridlines()

# visualising interaction term ------------------------------

# making interaction terms with raw values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

immi_viz <- lmer(immigSelf ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability_raw +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct + (1|LAD),
                 data = df_immi, REML = FALSE)

# anti immigration among social housing tenants 
x_scale <- seq(min(df_immi$affordability_raw), 
               max(df_immi$affordability_raw), 
               (max(df_immi$affordability_raw) - min(df_immi$affordability_raw))/5)

immi_dummy <- expand.grid(
  male = c(mean(df_immi$male)),
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  edu_20plus = c(mean(df_immi$edu_20plus)),
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
                           ifelse(social_housing == 0 & homeowner == 0, "Other",
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
  labs(x = NULL,
       y = "Anti-immigration",
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

int_plot <- p1 / p2

int_plot

saveRDS(int_plot, file = "working/markdown_viz/int_plot.RDS")

# model for table --------------------------------------------------------------

df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_tab <- lmer(immigSelf ~ social_housing + homeowner + private_renting +  
                   affordability +
                   male + white_british + 
                   no_religion + edu_20plus +
                   age + 
                   c1_c2 + d_e + non_uk_born + 
                   gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                   over_65_pct + under_15_pct + 
                   degree_pct + manuf_pct +
                   social_housing.affordability + 
                   homeowner.affordability +
                   (1|LAD),
                 data = df_immi, REML = FALSE)
summary(immi_tab)
summary(immi_int)

saveRDS(immi_tab, file = "working/markdown_data/immi_tab.RDS")

# models using housing cost measures ------------------------------------------

# adding regional degree pct
degrees <- read_csv("regional_degree_pct.csv")

degrees <- degrees %>% 
  rename(region_code = `Area Codes`,
         degree_pct_region = `Level 4+ (%)`) %>%
  select(region_code, degree_pct_region)

df_immi <- df_immi %>% 
  left_join(degrees, by = "region_code")

# sh * costs, owners * affordability
immi_costs <- lmer(immigSelf ~ (social_housing * cost_ratio) +
                     degree_pct_region +
                     male + white_british +
                     no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     (1|region_code) + (1|region_code:LAD),
                   data = df_immi, REML = FALSE)

summary(immi_costs)

immi_hcli <- lmer(immigSelf ~ (social_housing * hcli) +
                    degree_pct_region +
                    male + white_british + 
                    no_religion + edu_20plus +
                    private_renting + age + 
                    c1_c2 + d_e + non_uk_born + 
                    (1|region_code) + (1|region_code:LAD),
                  data = df_immi, REML = FALSE)

summary(immi_hcli)

immi_price <- lmer(immigSelf ~ (social_housing * price_ratio) +
                     degree_pct_region +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     (1|region_code) + (1|region_code:LAD),
                   data = df_immi, REML = FALSE)

summary(immi_price)

AIC(immi_csts, immi_hcli, immi_price)

tibble(
  `Cost ratio` = fixef(immi_costs)[c("social_housing","cost_ratio","social_housing:cost_ratio")],
  `30:40` = fixef(immi_hcli)[c("social_housing","hcli","social_housing:hcli")],
  `Price earnings ratio` = fixef(immi_price)[c("social_housing","price_ratio","social_housing:price_ratio")],
  variable = c("Social housing", "Affordability", "Interaction term")
) %>% 
  pivot_longer(
    cols = `Cost ratio`:`Price earnings ratio`,
    names_to = "Affordability measure",
    values_to = "Estimate"
  ) %>% 
  ggplot(aes(x = Estimate, y = variable, fill = `Affordability measure`)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_viridis_d() +
  theme_bw() +
  drop_y_gridlines() +
  labs(y = NULL) +
  theme(legend.position = "top")

