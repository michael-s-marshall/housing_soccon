pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, gghighlight, survey,
               srvyr)

rm(list = ls())

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

df <- read_dta("U:/housing_soccon/bes_panel_stata/stata/stata13_se/bes2019_w20_ukdspanel_v0-2.dta")

df %>%
  select(contains("immigSelf")) %>% 
  names() %>% 
  map(~count(df %>%
               select(contains("immigSelf")),
             .data[[.x]])) %>% 
  map(na.omit) %>% 
  map(mutate, cumsum(n))

immig_df <- df %>%
  select(id, contains("immigSelf")) %>% 
  select(-immigSelfW12,-immigSelfW13) %>% 
  pivot_longer(
    cols = immigSelfW7:immigSelfW20,
    names_to = c("var","wave"),
    values_to = "immigSelf",
    names_sep = "W"
  ) %>% 
  select(-var) %>% 
  filter(!wave %in% c("7","8","9"))

msoa_vars <- df %>%
  select(contains("msoa")) %>% 
  names()

msoas <- df %>%
  select(id, all_of(msoa_vars)) %>% 
  pivot_longer(
    cols = all_of(msoa_vars),
    names_to = c("var","wave"),
    values_to = "msoa",
    names_sep = "W"
  ) %>% 
  select(-var)

immig_df <- immig_df %>% 
  left_join(msoas, by = c("id","wave"))

la_vars <- df %>%
  select(contains("oslaua")) %>% 
  names()

las <- df %>%
  select(id, all_of(la_vars)) %>% 
  pivot_longer(
    cols = all_of(la_vars),
    names_to = c("var","wave"),
    values_to = "oslaua",
    names_sep = "W"
  ) %>% 
  select(-var)

immig_df <- immig_df %>% 
  left_join(las, by = c("id","wave"))

# removing duplicates
duplicate_ids <- immig_df %>% 
  count(id) %>% 
  filter(n == 56) %>% 
  select(id) %>% 
  as_vector() %>% 
  str_c()

immig_df$immigSelf[immig_df$immigSelf == 9999] <- NA
immig_df %>% count(immigSelf)

immig_df <- immig_df %>% 
  filter(!id %in% duplicate_ids) %>% 
  na.omit()

rm(duplicate_ids, msoa_vars, msoas, la_vars, las)

# modelling ----------------------------------------------------------

immig_df$msoa <- as.factor(immig_df$msoa)
immig_df$oslaua <- as.factor(immig_df$oslaua)

immig_lme <- lmer(immigSelf ~ (1|id) + (1|oslaua) + (1|oslaua:msoa),
                  data = immig_df, REML = FALSE)

summ(immig_lme, r.squared = FALSE)
summary(immig_lme)

# modelling only wave 20 ----------------------------------------------

immig_20 <- immig_df %>% 
  filter(wave == "20")

immig_w20_lme <- lmer(immigSelf ~ (1|oslaua) + (1|oslaua:msoa),
                      data = immig_20, REML = FALSE)

summ(immig_w20_lme, r.squared = FALSE)
summary(immig_w20_lme)
