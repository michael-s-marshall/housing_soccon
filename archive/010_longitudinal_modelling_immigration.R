pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, lfe)

rm(list = ls())

########################################################################
# immigration ----------------------------------------------------------
########################################################################

# loading data
load("longitudinal_df.RData")

# saving a df for later application of filters prior to robustness checks
df <- immig_df

# missing observations -------------------------------

immig_df %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(oslaua_code) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$oslaua_code
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(immig_df, affordability)

# missing degree pct is Scotland
missing_las(immig_df, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(immig_df, foreign_per_1000)

# dataset for main model
immig_df <- immig_df %>% 
  select(-degree_pct_change, -prices, -prices_mean) %>% 
  na.omit()

nrow(df) - nrow(immig_df)

# dataset for robustness with prices
immig_df_price <- df %>% 
  select(-degree_pct_change, -affordability, 
         -affordability_log, -affordability_log_mean,
         -affordability_mean) %>% 
  na.omit()

# dataset for robustness with degree change
immig_df_change <- df %>% 
  select(-prices, -prices_mean) %>% 
  na.omit()

## fixed effects ------------------------------------------------------------------

immi_fe <- felm(immigSelf ~ affordability +
                  pop_density + foreign_per_1000 +
                  over_65_pct + under_15_pct +
                  gdp_capita + 
                  manuf_pct + year_c | 
                  id + oslaua_code,
                data = immig_df)
summary(immi_fe)

saveRDS(immi_fe, file = "working/markdown_data/immi_fe.RDS")

immi_fe2 <- lmer(immigSelf ~ affordability +
                   pop_density + foreign_per_1000 +
                   over_65_pct + under_15_pct +
                   gdp_capita + 
                   manuf_pct +
                   year_c +
                   (1|oslaua_code) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_fe2)

immi_fe3 <- lmer(immigSelf ~ affordability + affordability_mean +
                   pop_density + pop_density_mean + 
                   foreign_per_1000 + foreign_per_1000_mean +
                   over_65_pct + over_65_pct_mean +
                   under_15_pct + under_15_pct_mean +
                   gdp_capita + gdp_capita_mean +
                   manuf_pct + manuf_pct_mean +
                   year_c + (1|oslaua_code) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_fe3)

# comparison of coefficients
# barplot of coefficients per model
tibble(
  felm = coef(immi_fe),
  lmer = fixef(immi_fe2)[-1],
  lmer_c = fixef(immi_fe3)[!str_detect(names(fixef(immi_fe3)),"Intercept|mean")],
  variable = names(coef(immi_fe))
) %>% 
  pivot_longer(
    felm:lmer_c,
    names_to = "model",
    values_to = "estimate"
  ) %>% 
  ggplot(aes(x = estimate, y = variable, fill = model)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "top") +
  drop_y_gridlines()

# interaction effects ------------------------------------------------

# level 1 no interactions
immi_lv1 <- lmer(immigSelf ~ affordability_mean + affordability +
                   pop_density + pop_density_mean +
                   foreign_per_1000 + foreign_per_1000_mean +
                   over_65_pct + over_65_pct_mean +
                   under_15_pct + under_15_pct_mean +
                   gdp_capita + gdp_capita_mean +
                   manuf_pct + manuf_pct_mean +
                   uni + white + no_religion + c1_c2 + 
                   d_e + non_uk_born + private_renting +
                   homeowner + social_housing +
                   year_c + degree_pct +
                   (1|oslaua_code) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_lv1)

immig_df <- immig_df %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_int <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability +
                   pop_density + pop_density_mean +
                   foreign_per_1000 + foreign_per_1000_mean +
                   over_65_pct + over_65_pct_mean +
                   under_15_pct + under_15_pct_mean +
                   gdp_capita + gdp_capita_mean +
                   manuf_pct + manuf_pct_mean +
                   uni + white + no_religion + c1_c2 + 
                   d_e + non_uk_born + 
                   #homeowner + social_housing +
                   year_c + degree_pct +
                   social_housing.affordability +
                   homeowner.affordability +
                   affordability_mean + 
                   (1|oslaua_code) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_int)

saveRDS(immi_int, file = "working/markdown_data/immi_int_long.RDS")

anova(immi_fe3, immi_lv1)
anova(immi_lv1, immi_int)

# robustness check - log affordability ---------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log) +
                   (homeowner * affordability_log) +
                   affordability_log_mean + # affordability +
                   pop_density + pop_density_mean +
                   foreign_per_1000 + foreign_per_1000_mean +
                   over_65_pct + over_65_pct_mean +
                   under_15_pct + under_15_pct_mean +
                   gdp_capita + gdp_capita_mean +
                   manuf_pct + manuf_pct_mean +
                   uni + white + no_religion + c1_c2 + 
                   d_e + non_uk_born + private_renting +
                   #homeowner + social_housing +
                   year_c + degree_pct +
                   (1|oslaua_code) + (1|id),
                 data = immig_df, REML = FALSE)
summary(immi_log)

saveRDS(immi_log, file = "working/markdown_data/immi_log_long.RDS")

# robustness check - prices -------------------------------------------

immi_price <- lmer(immigSelf ~ (social_housing * prices) +
                     (homeowner * prices) +
                     prices_mean + 
                     pop_density + pop_density_mean +
                     foreign_per_1000 + foreign_per_1000_mean +
                     over_65_pct + over_65_pct_mean +
                     under_15_pct + under_15_pct_mean +
                     gdp_capita + gdp_capita_mean +
                     manuf_pct + manuf_pct_mean +
                     uni + white + no_religion + c1_c2 +
                     d_e + non_uk_born + private_renting +
                     year_c + degree_pct +
                     (1|oslaua_code) + (1|id),
                   data = immig_df_price, REML = FALSE)
summary(immi_price)

saveRDS(immi_price, file = "working/markdown_data/immi_price_long.RDS")

# robustness check - degree percent change ---------------------------

immi_int2 <- lmer(immigSelf ~ (social_housing * affordability) +
                    (homeowner * affordability) +
                    affordability_mean + 
                    pop_density + pop_density_mean +
                    foreign_per_1000 + foreign_per_1000_mean +
                    over_65_pct + over_65_pct_mean +
                    under_15_pct + under_15_pct_mean +
                    gdp_capita + gdp_capita_mean +
                    manuf_pct + manuf_pct_mean +
                    uni + white + no_religion + c1_c2 + 
                    d_e + non_uk_born + private_renting +
                    year_c + degree_pct + degree_pct_change +
                    (1|oslaua_code) + (1|id),
                  data = immig_df_change, REML = FALSE)
summary(immi_int2)

saveRDS(immi_int2, file = "working/markdown_data/immi_int2_long.RDS")
