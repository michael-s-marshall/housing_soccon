edu_msoa <- read_csv("TS067-2021-1-filtered-2023-11-07T15_01_19Z.csv")

edu_msoa <- edu_msoa %>%
  rename(msoa_code = 1,
         msoa_name = 2,
         highest_qual_code = 3,
         highest_qual = 4,
         n_obs = 5) %>%
  filter(highest_qual_code != -8) %>% 
  group_by(msoa_code, msoa_name) %>% 
  mutate(total_obs = sum(n_obs)) %>% 
  ungroup() %>% 
  mutate(la_name = str_trim(str_remove_all(msoa_name, "[:digit:]")),
         .before = 3)

degree_msoa <- edu_msoa %>% 
  filter(highest_qual_code == 5) %>% 
  mutate(degree_pct = n_obs / total_obs)

ls_msoa_degree <- degree_msoa %>% 
  group_by(la_name) %>% 
  summarise(n_obs = sum(n_obs, na.rm = T),
            total_obs = sum(total_obs, na.rm = T),
            .groups = "drop") %>% 
  mutate(degree_pct = n_obs / total_obs) %>% 
  select(la_name, degree_pct)

ls_msoa_degree$la_name[ls_msoa_degree$la_name == "Bristol"] <- "Bristol, City of"
ls_msoa_degree$la_name[ls_msoa_degree$la_name == "Kingston upon Hull"] <- "Kingston upon Hull, City of"
ls_msoa_degree$la_name[ls_msoa_degree$la_name == "Herefordshire"] <- "Herefordshire, County of"

##############################################################################
# needs to be run after edu object created -----------------------------------
##############################################################################

test <- edu %>% 
  mutate(degree_pct = degree_pct / 100) %>% 
  left_join(afford %>% 
              select(`Local authority code`, `Local authority name`) %>% 
              rename(oslaua_code = 1, la_name = 2),
            by = "oslaua_code") %>% 
  left_join(ls_msoa_degree, by = "la_name")
