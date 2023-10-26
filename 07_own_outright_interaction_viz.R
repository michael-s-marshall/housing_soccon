pacman::p_load(patchwork)

## anti immigration among outright owners ----------------------------

immi_dummy <- expand.grid(
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  uni = c(mean(df_immi$uni)),
  social_housing = c(mean(df_immi$social_housing)),
  private_renting = c(mean(df_immi$private_renting)),
  own_mortgage = c(mean(df_immi$own_mortgage)),
  age = mean(df_immi$age),
  c1_c2 = c(mean(df_immi$c1_c2)),
  d_e = c(mean(df_immi$d_e)),
  non_uk_born = c(mean(df_immi$non_uk_born)),
  gdp_growth_pct_5 = c(mean(df_immi$gdp_growth_pct_5)),
  pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
  pop_growth = c(mean(df_immi$pop_growth)),
  white_perc = c(mean(df_immi$white_perc)),
  own_outright = c(0,1),
  affordability = seq(0, 1, 0.2)
) %>% 
  mutate(own_outright.affordability = own_outright * affordability)

acov <- vcov(immi_out)

fixed <- summary(immi_out)$coefficients[,"Estimate"]

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

p1 <- immi_dummy %>% 
  ggplot(aes(x = affordability, y = fit,
             colour = as.factor(own_outright))) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = as.factor(own_outright)),
              colour = "lightgrey", alpha = 0.25) +
  geom_line(linewidth = 2.5) +
  theme_bw() +
  drop_gridlines() +
  scale_colour_viridis_d(option = "D") +
  labs(x = "Affordability ratio",
       y = "Anti-immigration (predicted values)",
       colour = "Own outright")

p2 <- df_immi %>% 
  ggplot(aes(x = affordability)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

p1 / p2  
