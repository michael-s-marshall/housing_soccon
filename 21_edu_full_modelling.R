pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, gghighlight, survey,
               srvyr, randomForest)

rm(list = ls())

edu <- read_csv("census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(oslaua_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(oslaua_code, degree_pct) %>% 
  filter(oslaua_code != "K04000001")

# education 2011--------------------------------------------------------------

edu_2011 <- read_csv("predicting_edu_data/education_census_2011.csv")

edu_2011 <- edu_2011 %>% 
  rename(oslaua_code = 1,
         degree_pct = 8) %>% 
  select(oslaua_code, degree_pct)

edu <- edu %>% 
  left_join(
    edu_2011, by = "oslaua_code", suffix = c("2021","2011")
  )

edu %>% 
  ggplot(aes(x = log(degree_pct2011), y = log(degree_pct2021))) +
  geom_point(alpha = 1/2) +
  geom_smooth()

(summary(lm(log(degree_pct2021) ~ log(degree_pct2011), data = edu)))

# earnings --------------------------------------------------------------------

ashe <- read_csv("predicting_edu_data/ashe_2021.csv",
                      na = c("x","NA"))

ashe_2021 <- ashe %>%
  rename(oslaua_code = Code,
         median_earnings = Median) %>% 
  select(oslaua_code, median_earnings)

edu <- edu %>%
  left_join(
    ashe_2021, by = "oslaua_code"
    ) 

edu %>% 
  ggplot(aes(x = median_earnings, y = degree_pct2021)) +
  geom_point(alpha = 1/2) +
  geom_smooth()

(summary(lm(degree_pct2021 ~ poly(median_earnings, 2), data = na.omit(edu))))

# tenure -------------------------------------------------------------------

load("wales_tenure.RData")
load("eng_tenure.RData")

eng_tenure <- eng_tenure %>% 
  filter(year == 2021) %>% 
  select(-year)

ashe <- ashe %>% 
  mutate(name_clean = str_squish(str_split_i(Description, "/", 1)))

wales_tenure <- wales_tenure %>% 
  filter(year == 2021) %>% 
  left_join(ashe %>% select(Code, name_clean),
            by = c("la_name" = "name_clean")) %>% 
  rename(oslaua_code = Code) %>% 
  select(oslaua_code, prs_perc, soc_hous_perc, own_occ_perc)

tenure <- bind_rows(eng_tenure, wales_tenure)

edu <- edu %>% 
  left_join(
    tenure, by = "oslaua_code"
  )

edu %>% 
  ggplot(aes(x = prs_perc, y = degree_pct2021)) +
  geom_point(alpha = 1/2) +
  geom_smooth() +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 3),
              colour = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              colour = "green")

edu %>% 
  ggplot(aes(x = prs_perc, y = log(degree_pct2021))) +
  geom_point(alpha = 1/2) +
  geom_smooth()

((summary(lm(degree_pct2021 ~ own_occ_perc + prs_perc, data = edu))))
((summary(lm(degree_pct2021 ~ soc_hous_perc + prs_perc, data = edu))))
((summary(lm(log(degree_pct2021) ~ splines::ns(prs_perc,df=3), data = na.omit(edu)))))

# population density ----------------------------------------------------------

load("pop.RData")

pop <- pop %>% 
  filter(year == 2021) %>% 
  select(oslaua_code, pop_density)

edu <- edu %>% 
  left_join(pop, by = "oslaua_code")

edu %>% 
  ggplot(aes(x = log(pop_density), y = degree_pct2011)) +
  geom_point(alpha = 1/2) +
  geom_smooth(method = "lm", formula = y ~ splines::ns(x,df = 4)) +
  geom_smooth(colour = "red")

((summary(lm(degree_pct2021 ~ splines::ns(log(pop_density),df=4), data = na.omit(edu)))))

# splitting sample ----------------------------------------------------------

edu_mod <- edu %>% na.omit()
set.seed(123)
train_edu <- sample_frac(edu_mod, 0.8)
test_edu <- edu_mod %>% 
  filter(!oslaua_code %in% train_edu$oslaua_code)

train_lm <- lm(degree_pct2021 ~
                 degree_pct2011 +
                 poly(median_earnings, 2) +
                 poly(prs_perc, 2) +
                 splines::ns(log(pop_density), df=4),
               data = na.omit(train_edu))
summary(train_lm)

train_lm2 <- lm(degree_pct2021 ~
                 degree_pct2011 +
                 #median_earnings +
                 splines::ns(prs_perc,df=3) +
                 splines::ns(log(pop_density), df = 4),
               data = na.omit(train_edu))
summary(train_lm2)

preds_test_lm <- predict(train_lm, newdata = test_edu)
preds_test_lm2 <- predict(train_lm2, newdata = test_edu)

mean((test_edu$degree_pct2021 - preds_test_lm)^2)
mean((test_edu$degree_pct2021 - preds_test_lm2)^2)

# randomforest ------------------------------------------------------

train_edu$pop_density_log <- log(train_edu$pop_density)
test_edu$pop_density_log <- log(test_edu$pop_density)

train_rf <- randomForest(degree_pct2021 ~
                           degree_pct2011 +
                           median_earnings +
                           prs_perc +
                           pop_density_log,
                         importance = T,
                         mtry = 2,
                         data = train_edu)

varImpPlot(train_rf)

preds_test_rf <- predict(train_rf, newdata = test_edu)
mean((test_edu$degree_pct2021 - preds_test_rf)^2)

# saving model -----------------------------------------------------

saveRDS(train_lm2, file = "edu_predictor.RDS")

# 2016-2020 data -----------------------------------------------------

load("wales_tenure.RData")
load("eng_tenure.RData")

eng_tenure_2020 <- eng_tenure %>% 
  filter(year != 2021)

wales_tenure_2020 <- wales_tenure %>%
  filter(year != 2021) %>% 
  left_join(ashe %>% select(Code, name_clean),
            by = c("la_name" = "name_clean")) %>% 
  rename(oslaua_code = Code) %>% 
  select(oslaua_code, year, prs_perc, soc_hous_perc, own_occ_perc)

tenure_2020 <- bind_rows(eng_tenure_2020, wales_tenure_2020)

load("pop.RData")

pop_2020 <- pop %>%
  filter(year %in% seq(2016, 2020, 1))

edu_2011 <- read_csv("predicting_edu_data/education_census_2011.csv")

edu_2011 <- edu_2011 %>% 
  rename(oslaua_code = 1,
         degree_pct = 8) %>% 
  select(oslaua_code, degree_pct)

prev_years <- expand.grid(
  oslaua_code = edu$oslaua_code,
  year = seq(2016, 2020, 1)
) %>% 
  as_tibble() %>% 
  arrange(oslaua_code, year) %>% 
  left_join(tenure_2020, by = c("oslaua_code","year")) %>% 
  left_join(pop_2020, by = c("oslaua_code","year")) %>% 
  left_join(edu_2011, by = "oslaua_code") %>% 
  rename(degree_pct2011 = degree_pct) %>% 
  na.omit()

prev_years$degree_pct <- predict(train_lm2, newdata = prev_years)

edu_full <- bind_rows(
  edu %>% 
    select(oslaua_code, degree_pct2021) %>% 
    rename(degree_pct = degree_pct2021) %>% 
    mutate(year = 2021),
  prev_years %>% select(oslaua_code, year, degree_pct)
) %>% 
  arrange(oslaua_code, year)

save(edu_full, file = "edu_full.RData")

