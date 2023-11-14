# model with interaction ----------------------------------------------------

# making interaction terms
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
                   gdp_capita +
                   pop_sqm_2021 + white_perc + 
                   over_65_pct + under_15_pct + 
                   degree_pct + 
                   manuf_pct +
                   (1|region_code) + (1|region_code:la_code),
                 data = df_immi, REML = FALSE)

## anti immigration among social hoousing tenants ----------------------------

x_scale <- seq(min(df_immi$affordability_raw), 
               max(df_immi$affordability_raw), 
               (max(df_immi$affordability_raw) - min(df_immi$affordability_raw))/5)

immi_dummy <- expand.grid(
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  uni = c(mean(df_immi$uni)),
  homeowner = c(mean(df_immi$homeowner)),
  private_renting = c(mean(df_immi$private_renting)),
  age = mean(df_immi$age),
  c1_c2 = c(mean(df_immi$c1_c2)),
  d_e = c(mean(df_immi$d_e)),
  non_uk_born = c(mean(df_immi$non_uk_born)),
  gdp_capita = c(mean(df_immi$gdp_capita)),
  pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
  white_perc = c(mean(df_immi$white_perc)),
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
  ggplot(aes(x = affordability_raw, y = fit,
             colour = as.factor(social_housing))) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = as.factor(social_housing)),
              colour = "lightgrey", alpha = 0.25) +
  geom_line(linewidth = 2.5) +
  theme_bw() +
  drop_gridlines() +
  scale_colour_viridis_d(option = "D") +
  labs(x = "Affordability ratio",
       y = "Anti-immigration (predicted values)",
       colour = "Social housing") +
  coord_cartesian(ylim = c(5.5,8.5))

p2 <- df_immi %>% 
  ggplot(aes(x = affordability_raw)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, 
                 colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

p1 / p2
