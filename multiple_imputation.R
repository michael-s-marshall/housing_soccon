pacman::p_load(jomo, mitools, mitml)

df_immi <- df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion, c1_c2, d_e, 
         social_housing, private_renting, age, non_uk_born, homeowner)

df_immi <- df_immi %>% 
  drop_na(affordability)

df_binaries <- df_immi %>% 
  select(uni:private_renting, non_uk_born, homeowner) %>% 
  names()

df_immi[df_binaries] <- df_immi[df_binaries] %>% 
  map_df(as.factor)

df_immi <- df_immi %>% 
  mutate(immigSelf = 10 - immigSelf)

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df_immi %>% select(affordability:manuf_pct) %>% names()

# scaling
df_immi[level_twos] <- df_immi[level_twos] %>%
  map_df(scale_this)

# dropping NAs ---------------------------------------------------------

df_immi %>% map_chr(class)

df_immi %>% select_if(is.double) %>% map_dbl(mean, na.rm = T)

to_drop <- c("no_religion","c1_c2","d_e","homeowner","non_uk_born")
df_immi_fil <- df_immi %>% 
  drop_na(all_of(to_drop))

df_immi %>% map_int(~sum(is.na(.)))
df_immi_fil %>% map_int(~sum(is.na(.)))

# jomo ------------------------------------

# cluster var
clus <- df_immi_fil$la_code
# intercept var
df_immi_fil$cons <- 1
# Y
Y <- data.frame(df_immi_fil[,c("immigSelf","uni")])
# X
X_vars <- names(df_immi_fil)[!names(df_immi_fil) %in% c("la_code", names(Y))]
X <- data.frame(df_immi_fil[,X_vars])
# imp
set.seed(123)
imp <- jomo(Y = Y, X = X, clus = clus, nburn = 2000, nbetween = 1000, nimp = 5)

# fitting substantive model
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imp.list2 <- jomo2mitml.list(imp)

lmer_imp <- with(
  data = imp.list,
  lmer(immigSelf ~ (social_housing * affordability) +
         (homeowner * affordability) + white_british + no_religion + uni +
         private_renting + age + c1_c2 + d_e + non_uk_born + 
         gdp_capita + pop_sqm_2021 + white_perc + over_65_pct + under_15_pct + 
         degree_pct + manuf_pct + (1|clus))
)

# Extract coefficients and variances
coefs <- MIextract(lmer_imp, fun = fixef)
vars <- MIextract(lmer_imp, fun = function(x) diag(vcov(x)))
# Pool results with Rubin's rules
results <- MIcombine(coefs, vars)
summary(results)
tibble(
  est = coefs[[1]],
  var_name = names(coefs[[1]]),
  se = sqrt(vars[[1]]),
  upper_ci = est + se * qnorm(0.975),
  lower_ci = est - se * qnorm(0.975)
) %>% 
  mutate(significant = lower_ci > 0 | upper_ci < 0) %>% 
  filter(var_name != "(Intercept)") %>% 
  ggplot(aes(x = est, y = var_name, colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_linerangeh(aes(xmin = lower_ci, xmax = upper_ci)) +
  geom_point() +
  labs(y = NULL) +
  theme_minimal() +
  scale_colour_brewer(palette="Dark2")

lmer_imp2 <- with(
  data = imp.list2,
  lmer(immigSelf ~ (social_housing * affordability) +
         (homeowner * affordability) + white_british + no_religion + uni +
         private_renting + age + c1_c2 + d_e + non_uk_born + 
         gdp_capita + pop_sqm_2021 + white_perc + over_65_pct + under_15_pct + 
         degree_pct + manuf_pct + (1|clus))
)

pooled_est <- testEstimates(lmer_imp2, extra.pars = TRUE)

pooled_est$estimates[,1:7] %>% 
  as_tibble() %>% 
  bind_cols(tibble(var_name = row.names(pooled_est$estimates))) %>% 
  mutate(
    upper_ci = Estimate + Std.Error * qnorm(0.975),
    lower_ci = Estimate - Std.Error * qnorm(0.975)
  ) %>% 
  mutate(significant = lower_ci > 0 | upper_ci < 0) %>% 
  filter(var_name != "(Intercept)") %>% 
  ggplot(aes(x = Estimate, y = var_name, colour = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_linerangeh(aes(xmin = lower_ci, xmax = upper_ci)) +
  geom_point() +
  labs(y = NULL) +
  theme_minimal() +
  scale_colour_brewer(palette = "Dark2")


