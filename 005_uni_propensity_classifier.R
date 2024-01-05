pacman::p_load(tidyverse, haven, jtools, lme4, lmerTest)

rm(list = ls())

# loading data ----------------------------------------------------------------

df <- read_dta("BES2019_W22_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

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
      parse_double()
  )

df$soc_class[df$soc_class == "Other"] <- NA
df$c1_c2 <- ifelse(df$soc_class == "C1-C2", 1, 0)
df$d_e <- ifelse(df$soc_class == "D-E", 1, 0)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$age_raw <- df$age
df$age <- scale_this(df$age)

df$broadsheet <- ifelse(df$p_paper_read %in% c(6,7,8,9,10), 1, 0)
df$broadsheet[is.na(df$p_paper_read)] <- NA
df$tabloid <- ifelse(df$p_paper_read %in% c(1,2,3,4,5), 1, 0)
df$tabloid[is.na(df$p_paper_read)] <- NA
df %>% count(broadsheet, tabloid, p_paper_read)
df$full_time <- ifelse(df$p_work_stat == 1, 1, 0)
df$edu_20plus <- ifelse(df$p_education_age == 5, 1, 0)
df$edu_20plus[is.na(df$p_education_age)] <- NA
df %>% count(edu_20plus, p_education_age)

# data frame to predict university attendance -------------------------

uni_df <- df %>% 
  select(uni, white_british, c1_c2, d_e, own_mortgage,
         own_outright, private_renting, social_housing, non_uk_born,
         non_voter, broadsheet, tabloid, full_time,
         edu_20plus, age_raw) %>%
  mutate(age_sq = age_raw ^ 2,
         age_cub = age_raw ^ 3) %>% 
  na.omit()

uni_df <- uni_df %>% 
  mutate(id = row_number())

# test and train datasets ------------------------------------

set.seed(123)
uni_df_train <- sample_frac(uni_df, size = 0.9)
uni_df_test <- uni_df %>% 
  filter(!id %in% uni_df_train$id)

uni_df_train <- uni_df_train %>% select(-id)
uni_df_test <- uni_df_test %>% select(-id)

# logit classifier ------------------------------------------------

uni_logit <- glm(uni ~ ., data = uni_df_train, family = "binomial")
summary(uni_logit)

# removing full-time employment as predictor 
uni_df_train <- uni_df_train %>% select(-full_time)

uni_logit <- glm(uni ~ ., data = uni_df_train, family = "binomial")
summary(uni_logit)

uni_probs <- predict(uni_logit, type = "response")
uni_preds <- ifelse(uni_probs > 0.5, 1, 0)
table(uni_df_train$uni, uni_preds)
mean(uni_df_train$uni == uni_preds)

test_probs <- predict(uni_logit, type = "response", newdata = uni_df_test)
test_preds <- ifelse(test_probs > 0.5, 1, 0)
table(uni_df_test$uni, test_preds)
mean(uni_df_test$uni == test_preds) # 85.2% correct predictions on test data

# saving model ------------------------------------------------

df <- df %>% 
  mutate(age_sq = age_raw ^ 2,
         age_cub = age_raw ^ 3)

df$uni_propensity <- predict(uni_logit, type = "response", newdata = df)
summary(df$uni_propensity)

df <- df %>% 
  select(id, uni, uni_propensity) %>% 
  mutate(uni_pred = ifelse(uni_propensity > 0.5, 1, 0),
         uni_full = ifelse(is.na(uni), uni_pred, uni))

df %>% count(uni, uni_full, uni_pred)

df <- df %>% 
  select(-uni)

df_uni <- df

rm(df)

save(df_uni, file = "df_uni.RData")
