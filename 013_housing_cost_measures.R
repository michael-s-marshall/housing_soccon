pacman::p_load(tidyverse, haven, survey, srvyr, jtools)

rm(list = ls())

data_path <- "U:/UKHLS_stata/UKDA-6614-stata/stata/stata13_se/ukhls/"
df <- read_dta(file=paste0(data_path, "k_hhresp.dta"))

missval <- c(-9, -8, -7, -2, -1)
for (i in 1:5) {
  df <- df %>%
    mutate_all(., list(~na_if(., missval[i])))
}

h_df <- df %>% 
  select(k_gor_dv, k_rent_dv, k_rentgrs_dv, k_fihhmngrs1_dv, 
         k_houscost1_dv, k_ieqmoecd_dv, k_psu,
         k_strata, k_hhdenui_xw) %>% 
  mutate(renter = ifelse(is.na(k_rent_dv), FALSE, TRUE),
         hb = k_rentgrs_dv - k_rent_dv, # HB amount
         hb = replace_na(hb, 0), # making non-renters HB = £0
         hous_costs_no_hb = k_houscost1_dv - hb, # housing costs net of HB
         eq_inc_bhc = (k_fihhmngrs1_dv - hb)/k_ieqmoecd_dv, # equivalised gross income net of HB
         cost_ratio = hous_costs_no_hb / eq_inc_bhc) # ratio of costs to income net of HB

h_df %>% map_int(~sum(is.na(.)))

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

h_svy <- h_df %>%
  as_survey_design(id = k_psu,
                   strata = k_strata,
                   weights = k_hhdenui_xw)

q40 <- h_svy %>% 
  summarise(threshold = survey_quantile(eq_inc_bhc, 0.4, na.rm = T)) %>% 
  select(threshold_q40) %>% 
  as_vector()

h_svy <- h_svy %>% 
  mutate(
    house_stress = ifelse(cost_ratio > 0.3, 1, 0),
    below_q40 = ifelse(eq_inc_bhc < q40, 1, 0),
    hcli = ifelse(below_q40 == 1 & house_stress == 1, 1, 0)
  )

hcli_proportions <- h_svy %>% 
  group_by(k_gor_dv) %>% 
  summarise(
    house_stress_proportion = survey_mean(house_stress, na.rm = T),
    q40_total = survey_total(below_q40, na.rm = T),
    hcli_total = survey_total(hcli, na.rm = T)
  ) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(region_name = as_factor(k_gor_dv),
         hcli = hcli_total / q40_total,
         .before = 2)

hcli_proportions

saveRDS(hcli_proportions, file = "hcli_proportions.RDS")

# cost ratios -----------------------------------------------------------------

rm(h_df, h_svy, hcli_proportions)

h_df <- df %>% 
  select(k_gor_dv, k_rent_dv, k_rentgrs_dv, k_fihhmngrs1_dv, 
         k_houscost1_dv, k_ieqmoecd_dv, k_psu,
         k_strata, k_hhdenui_xw) %>% 
  mutate(eq_inc_bhc = k_fihhmngrs1_dv/k_ieqmoecd_dv) # equivalised gross income

h_df %>% map_int(~sum(is.na(.)))

h_svy <- h_df %>%
  as_survey_design(id = k_psu,
                   strata = k_strata,
                   weights = k_hhdenui_xw)

cost_ratios <- h_svy %>% 
  group_by(k_gor_dv) %>% 
  summarise(
    grs_income = survey_mean(eq_inc_bhc, na.rm = T),
    house_costs = survey_mean(k_houscost1_dv, na.rm = T)
  ) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(region_name = as_factor(k_gor_dv),
         cost_ratio = house_costs / grs_income,
         .before = 2)

cost_ratios

saveRDS(cost_ratios, file = "cost_ratios.RDS")

# comparison of different measures -------------------------------------------

rm(list = ls())

hcli <- readRDS("hcli_proportions.RDS")
hcli <- hcli %>% 
  mutate(region_name = str_squish(str_to_lower(str_remove(region_name, "of England"))))
cr <- readRDS("cost_ratios.RDS")
cr <- cr %>% 
  mutate(region_name = str_squish(str_to_lower(str_remove(region_name, "of England"))))
ar <- read_csv("regional_affordability.csv")
ar <- ar %>% 
  select(Name, `2021`) %>% 
  rename(region_name = Name,
         price_ratio = `2021`) %>% 
  mutate(region_name = str_squish(str_to_lower(region_name)))

merged <- hcli %>% 
  select(region_name, hcli) %>% 
  left_join(cr %>% select(region_name, cost_ratio),
            by = "region_name") %>% 
  left_join(ar, by = "region_name") %>% 
  na.omit()

merged %>% 
  pivot_longer(cols = hcli:price_ratio,
               names_to = "measure",
               values_to = "values") %>% 
  ggplot(aes(x = values, y = fct_reorder(region_name, values),
             colour = measure)) +
  geom_point(size = 2, show.legend = FALSE) +
  facet_wrap(~measure, ncol = 3, scales = "free_x") +
  theme_bw() +
  drop_y_gridlines() +
  labs(y = NULL) +
  scale_colour_viridis_d()
