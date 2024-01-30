pacman::p_load(haven, tidyverse, lavaan, jtools, lme4, lmerTest, margins)

rm(list = ls())

# loading data ----------------------------------------------------------------

df <- read_dta("panel_data/BES2019_W20_v24.0.dta")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

df %>% 
  select(starts_with("p_")) %>% 
  names() %>% 
  map(~count(df %>% select(starts_with("p_")), .data[[.x]]))

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

df$male <- ifelse(df$gender == 1, 1, 0)
df$soc_class[df$soc_class == "Other"] <- NA
df$c1_c2 <- ifelse(df$soc_class == "C1-C2", 1, 0)
df$d_e <- ifelse(df$soc_class == "D-E", 1, 0)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$homeowner <- ifelse(df$own_outright == 1 | df$own_mortgage == 1, 1, 0)
df$age_raw <- df$age
df$age <- scale_this(df$age)
df$edu_20plus <- ifelse(df$p_education_age == 5, 1, 0)
df$edu_20plus[is.na(df$p_education_age)] <- NA

df %>% count(male, gender)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(edu_20plus, p_education_age)

# making binary DV
df$immig_burden <- ifelse(df$immigrantsWelfareState == 4|df$immigrantsWelfareState == 5, 1, 0)
df$immig_burden[df$immigrantsWelfareState == 9999] <- NA
df %>% 
  count(immig_burden, immigrantsWelfareState) %>%
  arrange(desc(immig_burden)) %>% 
  mutate(perc = n / sum(n),
         cumulative_perc = cumsum(perc))

# affordability data ---------------------------------------------------------

# affordability ratio
afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2020`) %>% 
  select(la_code, affordability) %>% 
  mutate(affordability_log = log(affordability))

# prices
prices <- read_csv("median_house_prices.csv")

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = log(`2020`)) %>% 
  select(la_code, prices)

# merging
df <- df %>% 
  left_join(afford, by = "la_code") %>% 
  left_join(prices, by = "la_code")

df %>% 
  filter(is.na(affordability)) %>% 
  select(la_code) %>% 
  unique() %>% 
  as_vector() # missing = Scotland

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2020`) %>% 
  select(la_code, gdp_capita)

df <- df %>% 
  left_join(gdp_capita, by = "la_code")

# population data -----------------------------------------------------------

pop <- read_csv("population_change.csv")

names(pop) <- c("la_code", "name", "geography", "area_sqm",
                "pop_2021", "pop_sqm_2021", "pop_2011", "pop_sqm_2011",
                "pop_2001", "pop_sqm_2001")

pop <- pop %>% 
  select(la_code, pop_sqm_2021)

df <- df %>% 
  left_join(pop, by = "la_code")

# birth country ---------------------------------------------------

load("bc.RData")

bc <- bc %>%
  filter(year == 2020) %>%
  select(oslaua_code, foreign_per_1000)

df <- df %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))

## age of LAs --------------------------------------------------------

load("las_by_age.RData")

las_by_age <- las_by_age %>% 
  filter(year == 2020) %>% 
  select(-year)

df <- df %>% 
  left_join(las_by_age, by = c("la_code" = "oslaua_code"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19)

# education ---------------------------------------

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(la_code, degree_pct)

df <- df %>% 
  left_join(edu, by = "la_code")

# manufacturing percentage ------------------------------------------------

manuf <- read_csv("2020_industry_employment.csv")

indus_clean <- function(df){
  out <- df %>% 
    filter(str_detect(Area,"ladu")) %>% 
    rename(oslaua_code = mnemonic,
           manufacturing = 5) %>% 
    select(oslaua_code:23) %>% 
    pivot_longer(
      cols = 2:22,
      names_to = "industry",
      values_to = "employment"
    ) %>% 
    group_by(oslaua_code) %>% 
    mutate(total_employment = sum(employment),
           manuf_pct = employment / total_employment) %>% 
    ungroup() %>% 
    filter(industry == "manufacturing")
  return(out)
}

manuf <- indus_clean(manuf) %>% 
  rename(la_code = oslaua_code) %>% 
  select(la_code, manuf_pct)

df <- df %>% 
  left_join(manuf, by = "la_code")

# region --------------------------------------------------------------------

region <- read_csv("lasregionew2021lookup.csv")

region <- region %>% 
  rename(la_code = `LA code`,
         region_code = `Region code`) %>% 
  select(la_code, region_code)

df <- df %>% 
  left_join(region, by = "la_code")

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df %>% select(affordability:manuf_pct) %>% names()
rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}

df <- df %>% rename_raw(all_of(level_twos))

# scaling
df[level_twos] <- df[level_twos] %>%
  map_df(scale_this)

# missing values ---------------------------------------

df_immi <- df %>% 
  select(la_code, uni, male, white_british, no_religion, edu_20plus,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         immig_burden, all_of(level_twos), contains("raw"), 
         region_code) %>% 
  rename(LAD = la_code)

df_immi %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% select(-uni) %>% na.omit()

df_immi_uni <- df %>% 
  select(la_code, uni, male, white_british, no_religion, 
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         immig_burden, all_of(level_twos), contains("raw"),
         region_code) %>%
  rename(LAD = la_code) %>% 
  na.omit()

nrow(df) - nrow(df_immi)

(nrow(df) - nrow(df_immi)) / nrow(df)

###############################################################################
# modelling ------------------------------------------------------------------
###############################################################################

# multivariate ------------------------------------------------

immig_multi <- glmer(immig_burden ~ male + white_british +
                       no_religion + edu_20plus +
                       social_housing + private_renting +
                       homeowner + age +
                       c1_c2 + d_e + non_uk_born +
                       (1|LAD),
                     data = df_immi, family = binomial("logit"))

summary(immig_multi)

# including level 2 predictors  ------------------------------

immig_con <- glmer(immig_burden ~ male + white_british +
                     no_religion + edu_20plus +
                     social_housing + private_renting +
                     homeowner + age +
                     c1_c2 + d_e + non_uk_born +
                     affordability + gdp_capita +
                     pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct +
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))

summary(immig_con)

anova(immig_multi, immig_con)

# cross level interaction ------------------------------------------------------

df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_int <- glmer(immig_burden ~ social_housing + homeowner + private_renting +  
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
                   data = df_immi, family = binomial("logit"))
summary(immig_int)

anova(immig_con, immig_int)

saveRDS(immig_int, file = "working/markdown_data/immig_burden_int.RDS")

# marginal effects --------------------------------------------------------

marginals2 <- margins(immig_int, type = "response")

saveRDS(marginals2, file = "working/markdown_data/marginals2.RDS")

summary(marginals2)

plot_names <- tibble(
  term = marginals2 %>%
    summary %>% 
    as_tibble %>%
    select(factor) %>% 
    as_vector,
  var_name = c("Affordability", "Age", "Social class: C1-C2", "Social class: D-E",
               "Graduate %", "Education age 20+", "Non-UK born population",
               "GDP per capita", "Homeowner","Affordability:Homeowner", "Male", 
               "Manufacturing %", "No religion", "Non-UK born", "Over 65 %",
               "Population density", "Private renter", "Social renter",
               "Affordability:Social renter", "Under 15 %", "White British"),
  grouping = c("Housing", "Individual","Individual", "Individual",
               "Local", "Individual", "Local", "Local", 
               "Housing", "Housing", "Individual", "Local",
               "Individual", "Individual", "Local", "Local", 
               "Housing", "Housing", "Housing", 
               "Local", "Individual")
) %>% 
  mutate(grouping = fct_relevel(as.factor(grouping), 
                                c("Housing", "Individual",
                                  "Local")))

# plotting marginal effects
immi_burden_coefs <- marginals2 %>%
  summary %>% 
  as_tibble %>%
  left_join(plot_names, by = c("factor" = "term")) %>% 
  mutate(
    sig = lower > 0 | upper < 0,
    sig = ifelse(sig == T, "Significant","Non-significant"),
    sig = fct_rev(as.factor(sig))
  ) %>%
  ggplot(aes(x = AME, y = fct_rev(var_name), colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.5, alpha = 0.7) +
  ggstance::geom_linerangeh(aes(xmin = lower, xmax = upper),
                            size = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3.5) +
  theme_minimal() +
  drop_y_gridlines() +
  labs(x = "Estimate", y = NULL, colour = NULL) +
  facet_wrap(~grouping, ncol = 1, scales = "free_y") +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold")) +
  scale_colour_manual(values = c("black","grey65"))

immi_burden_coefs

saveRDS(immi_burden_coefs, file = "working/markdown_viz/immi_burden_coefs.RDS")

# with uni var ---------------------------------------------------

df_immi_uni <- df_immi_uni %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_uni <- glmer(immig_burden ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     male + white_british + no_religion + uni +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + (1|LAD),
                   data = df_immi_uni, family = binomial("logit"))
summary(immig_uni)

# log affordability ---------------------------------------

immig_log <- glmer(immig_burden ~ (social_housing * affordability_log) + 
                     (homeowner * affordability_log) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immig_log)

# prices --------------------------------------------------

immig_pri <- glmer(immig_burden ~ (social_housing * prices) + 
                     (homeowner * prices) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immig_pri)

# region dummies ------------------------------------------

immig_reg <- glmer(immig_burden ~ social_housing + homeowner + private_renting +  
                     affordability +
                     male + white_british + 
                     no_religion + edu_20plus +
                     age + 
                     c1_c2 + d_e + non_uk_born + 
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + manuf_pct +
                     social_housing.affordability + 
                     homeowner.affordability + region_code +
                     (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immig_reg)

# not incl. homeowner interaction ------------------------------------

immi_int3 <- glmer(immig_burden ~ social_housing.affordability +
                     # homeowner.affordability +
                     social_housing + homeowner + affordability +
                     male + white_british + no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + (1|LAD),
                   data = df_immi, family = binomial("logit"))
summary(immi_int3)

anova(immig_con, immi_int3)

marginals3 <- margins(immi_int3, type = "response")
summary(marginals3)
