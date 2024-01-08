pacman::p_load(tidyverse, lavaan, jtools, lme4, lmerTest, margins)

rm(list = ls())

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W23_v25.0.dta")

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

df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)

# making binary DV
df$equality_too_far <- ifelse(df$blackEquality == 4|df$blackEquality == 5, 1, 0)
df %>% 
  count(equality_too_far, blackEquality) %>%
  arrange(desc(equality_too_far)) %>% 
  mutate(perc = n / sum(n),
         cumulative_perc = cumsum(perc))

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2022`) %>% 
  select(la_code, affordability)

# merging
df <- df %>% 
  left_join(afford, by = "la_code")

df %>% 
  filter(is.na(affordability)) %>% 
  select(la_code) %>% 
  unique() %>% 
  as_vector() # missing = Scotland, City of London, Isles of Scilly

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
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
  filter(year == 2021) %>%
  select(oslaua_code, foreign_per_1000)

df <- df %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))

## age of LAs --------------------------------------------------------

load("las_by_age.RData")

las_by_age <- las_by_age %>% 
  filter(year == 2021) %>% 
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

manuf <- read_csv("2021_industry_employment.csv")

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

df_equal <- df %>% 
  select(la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         equality_too_far, all_of(level_twos), contains("raw"))

df_equal %>% map_int(~sum(is.na(.)))

df_equal <- df_equal %>% na.omit()

nrow(df) - nrow(df_equal)

(nrow(df) - nrow(df_equal)) / nrow(df)

###############################################################################
# modelling ------------------------------------------------------------------
###############################################################################

# multivariate ------------------------------------------------

equal_multi <- glmer(equality_too_far ~ white_british +
                       no_religion + uni +
                       social_housing + private_renting +
                       homeowner + age +
                       c1_c2 + d_e + non_uk_born +
                       (1|la_code),
                     data = df_equal, family = binomial("logit"))

summary(equal_multi)

# including level 2 predictors  ------------------------------

equal_con <- glmer(equality_too_far ~ white_british +
                     no_religion + uni +
                     social_housing + private_renting +
                     homeowner + age +
                     c1_c2 + d_e + non_uk_born +
                     affordability + gdp_capita +
                     pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct +
                     degree_pct + manuf_pct +
                     (1|la_code),
                   data = df_equal, family = binomial("logit"))

summary(equal_con)

anova(equal_multi, equal_con)

# cross level interaction ------------------------------------------------------

df_equal <- df_equal %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

equal_int <- glmer(equality_too_far ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     white_british + no_religion + uni +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + (1|la_code),
                   data = df_equal, family = binomial("logit"))
summary(equal_int)

anova(equal_con, equal_int)

# marginal effects --------------------------------------------------------

marginals <- margins(equal_int, type = "response")

saveRDS(marginals, file = "working/markdown_data/marginals.RDS")

plot_names <- tibble(
  term = marginals %>%
    summary %>% 
    as_tibble %>%
    select(factor) %>% 
    as_vector,
  var_name = c("Affordability", "Age", "Social class: C1-C2", "Social class: D-E",
               "Graduate %", "Non-UK born population","GDP per capita",
               "Homeowner","Affordability:Homeowner", "Manufacturing %",
               "No religion", "Non-UK born", "Over 65 %", "Population density",
               "Private renter", "Social renter", "Affordability:Social renter",
               "Under 15 %", "University graduate", "White British"),
  grouping = c("Housing", "Individual","Individual", "Individual",
               "Local", "Local", "Local", "Housing", "Housing", "Local",
               "Individual", "Individual", "Local", "Local", 
               "Housing", "Housing", "Housing", 
               "Local", "Individual", "Individual")
) %>% 
  mutate(grouping = fct_relevel(as.factor(grouping), 
                                c("Housing", "Individual",
                                  "Local")))

# plotting marginal effects
equality_coefs <- marginals %>%
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

equality_coefs

saveRDS(equality_coefs, file = "working/markdown_viz/equality_coefs.RDS")

# for table ---------------------------------------

equal_int2 <- glmer(equality_too_far ~ 
                     social_housing + homeowner +  private_renting + 
                     affordability +
                     white_british + no_religion + uni +
                     age +
                     c1_c2 + d_e + non_uk_born +
                     gdp_capita + pop_sqm_2021 + foreign_per_1000 +
                     over_65_pct + under_15_pct + degree_pct +
                     manuf_pct + 
                     social_housing.affordability +
                     homeowner.affordability + (1|la_code),
                   data = df_equal, family = binomial("logit"))
summary(equal_int2)

saveRDS(equal_int2, file = "working/markdown_data/equal_int.RDS")
