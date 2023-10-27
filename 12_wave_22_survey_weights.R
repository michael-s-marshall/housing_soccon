pacman::p_load(tidyverse, lavaan, psych, haven, jtools, lme4, lmerTest, survey,
               svylme)

rm(list = ls())

# lmer_coefs function ---------------------------------------------------------

source("05_lmer_coefs_function.R")

# summary and plotting coefs functions ---------------------------------------

summary.svy2lme <- function(svylme_obj){
  
  coefs<- cbind(beta=svylme_obj$beta,
                SE=sqrt(diag(svylme_obj$Vbeta)),
                t=svylme_obj$beta/sqrt(diag(svylme_obj$Vbeta)))
  coefs<-cbind(coefs,p=2*pnorm(-abs(coefs[,3])))
  colnames(coefs)<-c("beta","SE","t","p")
  coefs %>% 
    as_tibble() %>% 
    mutate(term = row.names(svylme_obj$beta),
           .before = 1) %>% 
    mutate(sig = ifelse(p < 0.001, "***",
                        ifelse(p < 0.01, "**",
                               ifelse(p < 0.05, "*", ""))),
           sig_sugg = ifelse(p < 0.005, "Significant", 
                             ifelse(p < 0.05, "Suggestive", "Non-significant")))
  
}

pacman::p_load(ggstance)

svylme_coefs <- function(svylme_obj){
  my_summ <- summary(svylme_obj) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(sig_ul = beta + SE * qnorm(0.9975),
           sig_ll = beta - SE * qnorm(0.9975),
           sug_ul = beta + SE * qnorm(0.975),
           sug_ll = beta - SE * qnorm(0.975),
           sig_sugg = fct_relevel(
             as.factor(sig_sugg),
             c("Significant","Suggestive")
           ))
  
  my_summ %>% 
    ggplot(aes(x = beta, y = term, colour = sig_sugg)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
               linewidth = 1.5, alpha = 0.7) +
    geom_linerangeh(aes(xmin = sig_ll, xmax = sig_ul), size = 1) +
    geom_linerangeh(aes(xmin = sug_ll, xmax = sug_ul), size = 2) +
    geom_point(shape = 21, fill = "white", size = 3.5) +
    theme_bw() +
    drop_y_gridlines() +
    labs(x = "Estimate", y = NULL, colour = NULL) +
    theme(legend.position = "top") +
    scale_colour_viridis_d() 
}

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W22_v24.0.dta")

df %>% 
  select(lr_scale, al_scale) %>% 
  ggplot(aes(x = lr_scale, y = al_scale)) +
  geom_boxplot(mapping = aes(group = cut_width(lr_scale, 0.25))) +
  geom_smooth()

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
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
      "Other" = c("7","8")
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
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                    NA, df$p_gross_household)
df$income <- rescale01(df$income, na.rm = T)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$redistSelf[df$redistSelf == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age <- rescale01(df$age, na.rm = T)

df %>% 
  count(tory_2019, p_turnout_2019, p_past_vote_2019)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(redistSelf)
df %>% count(immigSelf)

# missing values -------------------------------

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, income, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf) %>% 
  map_int(~sum(is.na(.)))

df %>% 
  select(lr_scale, al_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, income, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, tory_2019, non_voter,
         redistSelf, immigSelf) %>% 
  na.omit() %>% 
  nrow()

# df_con
df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_con <- df %>% 
  select(al_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, wt) %>% 
  na.omit()

# df_econ
df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_econ <- df %>% 
  select(lr_scale, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, wt) %>% 
  na.omit()

# df_redist
df %>% 
  select(redistSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_redist <- df %>% 
  select(redistSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, wt) %>% 
  na.omit()

# df_immi
df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_immi <- df %>% 
  select(immigSelf, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, wt) %>% 
  na.omit()

# df_tory
df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born) %>% 
  na.omit() %>% 
  nrow()

df_tory <- df %>% 
  select(tory_2019, la_code, uni, white_british, no_religion,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, non_uk_born, wt) %>% 
  na.omit()

# affordability data ---------------------------------------------------------

afford <- read_csv("affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2021`) %>% 
  select(la_code, affordability)

# merging
df_con <- df_con %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_econ <- df_econ %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_redist <- df_redist %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_immi <- df_immi %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_tory <- df_tory %>% 
  left_join(afford, by = "la_code") %>% 
  mutate(affordability = rescale01(affordability, na.rm = T))

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

df_redist %>% 
  map_int(~sum(is.na(.)))

df_immi %>% 
  map_int(~sum(is.na(.)))

# removing scotland
df_con <- df_con %>% na.omit()

df_econ <- df_econ %>% na.omit()

df_redist <- df_redist %>% na.omit()

df_immi <- df_immi %>% na.omit()

df_tory <- df_tory %>% na.omit()

# gdp data --------------------------------------------------------------------

gdp <- read_csv("gdp_per_capita.csv")

gdp_capita <- gdp %>% 
  rename(la_code = `LA code`,
         gdp_capita = `2021`) %>% 
  select(la_code, gdp_capita)

gdp_growth <- gdp %>% 
  rename(la_code = `LA code`) %>% 
  select(la_code, `2010`:`2021`) %>% 
  pivot_longer(cols = `2010`:`2021`,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.integer(year)) %>% 
  arrange(la_code, year) %>% 
  group_by(la_code) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = gdp - lag(gdp),
         gdp_growth_pct = ((diff_growth / diff_year)/gdp)*100) %>% 
  summarise(gdp_growth_pct = mean(gdp_growth_pct, na.rm = T),
            .groups = "drop")

gdp_growth_5 <- gdp %>% 
  rename(la_code = `LA code`) %>% 
  select(la_code, `2016`:`2021`) %>% 
  pivot_longer(cols = `2016`:`2021`,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.integer(year)) %>% 
  arrange(la_code, year) %>% 
  group_by(la_code) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = gdp - lag(gdp),
         gdp_growth_pct = ((diff_growth / diff_year)/gdp)*100) %>% 
  summarise(gdp_growth_pct_5 = mean(gdp_growth_pct, na.rm = T),
            .groups = "drop")

df_con <- df_con %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>%
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_econ <- df_econ %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_redist <- df_redist %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_immi <- df_immi %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_tory <- df_tory %>% 
  left_join(gdp_capita, by = "la_code") %>% 
  left_join(gdp_growth, by = "la_code") %>% 
  left_join(gdp_growth_5, by = "la_code") %>% 
  mutate(gdp_capita = rescale01(gdp_capita, na.rm = T),
         gdp_growth_pct = rescale01(gdp_growth_pct, na.rm = T),
         gdp_growth_pct_5 = rescale01(gdp_growth_pct_5, na.rm = T))

df_con %>% 
  map_int(~sum(is.na(.)))

df_econ %>% 
  map_int(~sum(is.na(.)))

df_redist %>% 
  map_int(~sum(is.na(.)))

df_immi %>% 
  map_int(~sum(is.na(.)))

df_tory %>% 
  map_int(~sum(is.na(.)))

# population data -----------------------------------------------------------

pop <- read_csv("population_change.csv")

names(pop) <- c("la_code", "name", "geography", "area_sqm",
                "pop_2021", "pop_sqm_2021", "pop_2011", "pop_sqm_2011",
                "pop_2001", "pop_sqm_2001")

pop <- pop %>% 
  mutate(pop_growth = ((pop_2021 - pop_2011) / pop_2011) * 100,
         pop_2021 = rescale01(pop_2021, na.rm = T),
         pop_sqm_2021 = rescale01(pop_sqm_2021, na.rm = T)) %>% 
  select(la_code, pop_2021, pop_growth, pop_sqm_2021)

pop$pop_growth <- rescale01(pop$pop_growth, na.rm = T)

df_con <- df_con %>% 
  left_join(pop, by = "la_code")
df_econ <- df_econ %>% 
  left_join(pop, by = "la_code")
df_redist <- df_redist %>% 
  left_join(pop, by = "la_code")
df_immi <- df_immi %>% 
  left_join(pop, by = "la_code")
df_tory <- df_tory %>% 
  left_join(pop, by = "la_code")

df_con %>% map_int(~sum(is.na(.)))
df_econ %>% map_int(~sum(is.na(.)))
df_redist %>% map_int(~sum(is.na(.)))
df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

# ethnic diversity ----------------------------------------------------------

ethnic <- read_csv("population-by-ethnicity-and-local-authority-2021.csv")

ethnic <- ethnic %>% 
  filter(Ethnicity == "White") %>%
  rename(
    la_code = Geography_code,
    white_perc = Value1
  ) %>% 
  mutate(white_perc = white_perc / 100) %>% 
  select(la_code, white_perc)

df_con <- df_con %>% 
  left_join(ethnic, by = "la_code")
df_econ <- df_econ %>% 
  left_join(ethnic, by = "la_code")
df_redist <- df_redist %>% 
  left_join(ethnic, by = "la_code")
df_immi <- df_immi %>% 
  left_join(ethnic, by = "la_code")
df_tory <- df_tory %>% 
  left_join(ethnic, by = "la_code")

df_con %>% map_int(~sum(is.na(.)))
df_econ %>% map_int(~sum(is.na(.)))
df_redist %>% map_int(~sum(is.na(.)))
df_immi %>% map_int(~sum(is.na(.)))
df_tory %>% map_int(~sum(is.na(.)))

## setting up interactions ------------------------------------------------

df_con <- df_con %>% 
  mutate(own_outright.affordability = own_outright * affordability,
         own_mortgage.affordability = own_mortgage * affordability,
         social_housing.affordability = social_housing * affordability,
         private_renting.affordability = private_renting * affordability)

df_econ <- df_econ %>% 
  mutate(own_outright.affordability = own_outright * affordability,
         own_mortgage.affordability = own_mortgage * affordability,
         social_housing.affordability = social_housing * affordability,
         private_renting.affordability = private_renting * affordability)

df_redist <- df_redist %>% 
  mutate(own_outright.affordability = own_outright * affordability,
         own_mortgage.affordability = own_mortgage * affordability,
         social_housing.affordability = social_housing * affordability,
         private_renting.affordability = private_renting * affordability)

df_immi <- df_immi %>% 
  mutate(own_outright.affordability = own_outright * affordability,
         own_mortgage.affordability = own_mortgage * affordability,
         social_housing.affordability = social_housing * affordability,
         private_renting.affordability = private_renting * affordability)

df_tory <- df_tory %>% 
  mutate(own_outright.affordability = own_outright * affordability,
         own_mortgage.affordability = own_mortgage * affordability,
         social_housing.affordability = social_housing * affordability,
         private_renting.affordability = private_renting * affordability)

###############################################################################
# authoritarianism ------------------------------------------------------------
##############################################################################

svy_con <- svydesign(ids=~1, 
                    data = df_con,
                    weights = df_con$wt)

con_int <- svy2lme(al_scale ~ white_british +
                       social_housing.affordability +
                       own_outright.affordability + 
                       no_religion + uni +
                       own_outright + own_mortgage + social_housing +
                       private_renting + age + 
                       c1_c2 + d_e + non_uk_born +
                       affordability + gdp_capita +
                       pop_sqm_2021 + pop_growth + white_perc +
                       (1|la_code),
                     design = svy_con)

summary(con_int)

svylme_coefs(con_int)

##############################################################################
# immigself ------------------------------------------------------------------
##############################################################################

# reordering immigSelf
df_immi <- df_immi %>% 
  rename(immigSelf_pro = immigSelf) %>% 
  mutate(immigSelf = 10 - immigSelf_pro)

svy_immi <- svydesign(ids=~1,
                      data = df_immi,
                      weights = df_immi$wt)

immi_int <- svy2lme(immigSelf ~ white_british +
                      social_housing.affordability +
                      own_outright.affordability + 
                      no_religion + uni +
                      own_outright + own_mortgage + social_housing +
                      private_renting + age + 
                      c1_c2 + d_e + non_uk_born +
                      affordability + gdp_capita +
                      pop_sqm_2021 + pop_growth + white_perc +
                      (1|la_code),
                    design = svy_immi)

summary(immi_int)

svylme_coefs(immi_int)

###############################################################################
# econ_right dimension --------------------------------------------------------
###############################################################################

svy_econ <- svydesign(ids=~1, 
                     data = df_econ,
                     weights = df_econ$wt)

econ_int <- svy2lme(lr_scale ~ white_british +
                     social_housing.affordability +
                     own_outright.affordability + 
                     no_religion + uni +
                     own_outright + own_mortgage + social_housing +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born +
                     affordability + gdp_capita +
                     pop_sqm_2021 + pop_growth + white_perc +
                     (1|la_code),
                   design = svy_econ)

summary(econ_int)

svylme_coefs(econ_int)

###############################################################################
# redistself ------------------------------------------------------------------
###############################################################################

svy_redist <- svydesign(ids=~1, 
                      data = df_redist,
                      weights = df_redist$wt)

redist_int <- svy2lme(redistSelf ~ white_british +
                      social_housing.affordability +
                      own_outright.affordability + 
                      no_religion + uni +
                      own_outright + own_mortgage + social_housing +
                      private_renting + age + 
                      c1_c2 + d_e + non_uk_born +
                      affordability + gdp_capita +
                      pop_sqm_2021 + pop_growth + white_perc +
                      (1|la_code),
                    design = svy_redist)

summary(redist_int)

svylme_coefs(redist_int)
