# This file takes the processed CCAHS data and extracts empirical bayes
# estimates of neighborhood measures:
# Collective Efficacy (Cohesion and Trust + Control Expectations)
# In addition, this file limits to NCs with a specified number of respondents
# to assess sensitivity to low reliability.

library(tidyverse)
library(lme4)
source("./syntax/project_functions.R")
load("./data/derived/ccahs_individual_long.RData")
load("./data/derived/ccahs_individual_wide.RData")
library(insight)
# Note this is not the same function as PHDCN due to formula difference

lme_reliability_2lvl <- function(x, c = 1){
  x <- lmer_out_2lvl
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  n <- table(insight::get_random(x))
  J <- length(n) # number of neighbs
  return(sum(c*t00 / (c*t00 + s2 / n)) / J)
}

lme_reliability_3lvl_rb2002 <- function(x, c = 1){
  var_components <- insight::get_variance(x)
  tb <- var_components$var.intercept[[2]]
  tp <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  rel <- x@frame %>% 
    count(NC_ID, RESP_ID) %>%
    group_by(NC_ID) %>%
    summarize(reliability = tb / (tb + 1/(sum(1/(tp + s2/n))))) %>% 
    pull(reliability)
  return(mean(rel))
}

lme_reliability_3lvl_rrk1991 <- function(x, c = 1){
  var_components <- insight::get_variance(x)
  tb <- var_components$var.intercept[[2]]
  tp <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  rel <- x@frame %>% 
    count(NC_ID, RESP_ID) %>%
    group_by(NC_ID) %>%
    summarize(jk = n(),
              njk = sum(n)) %>%
    mutate(reliability = tb / (tb + tp/jk + s2/(njk))) %>% 
    pull(reliability)
  return(mean(rel))
}

lmer_out_3lvl <- lmer(value ~ FEMALE + FAM_married + FAM_sepdiv + 
                        FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                        years_in_home + SES + (1|NC_ID/RESP_ID),  
                      ## Standardize all (including binary) predictors
                      data = ccahs_individual_long %>% 
                        #mutate(RESP_ID = as.character(as.numeric(RESP_ID) + 4000)) %>%
                        #bind_rows(ccahs_individual_long) %>%
                        filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
                        select(-VICT_violent_ever) %>%
                        mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                      control = lmerControl(optimizer = "bobyqa"))

lme_reliability_3lvl_rb2002(lmer_out_3lvl)
lme_reliability_3lvl_rrk1991(lmer_out_3lvl)


lmer_out_2lvl <- lmer(value ~ FEMALE + FAM_married + FAM_sepdiv + 
       FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
       years_in_home + SES + (1|NC_ID),  
     ## Standardize all (including binary) predictors
     data = ccahs_individual_long %>% 
       mutate(RESP_ID = as.character(as.numeric(RESP_ID) + 4000)) %>%
       bind_rows(ccahs_individual_long) %>%
       filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
       select(-VICT_violent_ever) %>%
       mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))) %>%
       group_by(measure) %>%
       mutate(value = standardize(value)) %>%
       ungroup() %>%
       group_by(NC_ID, RESP_ID) %>%
       mutate(value = mean(value)) %>%
       select(-measure) %>%
       slice(1L) %>% ungroup(),
     control = lmerControl(optimizer = "bobyqa"))

message(paste0("CE HLM Reliability: ", round(lme_reliability_2lvl(lmer_out_2lvl),3))) 

eb_est_2lvl <- setNames(ranef(lmer_out_2lvl)$NC_ID, paste0("CE", "_hlm")) %>% 
  tibble::rownames_to_column("NC_ID") %>% 
  as_tibble() %>% 
  mutate(NC_ID = as.character(NC_ID))


## build model with the same formula, options etc., but new response var

lmer_out_2lvl_r75 <- lmer(value ~ FEMALE + FAM_married + FAM_sepdiv + 
                        FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                        years_in_home + SES + (1|NC_ID),  
                      ## Standardize all (including binary) predictors
                      data = ccahs_individual_long %>% 
                        filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
                        select(-VICT_violent_ever) %>%
                        mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))) %>%
                        group_by(measure) %>%
                        mutate(value = standardize(value)) %>%
                        ungroup() %>%
                        group_by(NC_ID, RESP_ID) %>%
                        mutate(value = mean(value)) %>%
                        select(-measure) %>%
                        slice(1L) %>% ungroup(),
                      control = lmerControl(optimizer = "bobyqa"))


lmer_out_3lvl_dbl <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sepdiv + 
                        FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                        years_in_home + SES + (1|NC_ID/RESP_ID),  
                      ## Standardize all (including binary) predictors
                      data = ccahs_individual_long %>% 
                        bind_rows(ccahs_individual_long %>% mutate(RESP_ID = as.character(as.numeric(RESP_ID) + 4000))) %>%
                        bind_rows(ccahs_individual_long %>% mutate(RESP_ID = as.character(as.numeric(RESP_ID) + 8000))) %>%
                        filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
                        select(-VICT_violent_ever) %>%
                        mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                      control = lmerControl(optimizer = "bobyqa"))

message(paste0("CE HLM Reliability: ", round(lme_reliability_3lvl(lmer_out_3lvl_dbl),3)))

eb_est_3lvl_dbl <- setNames(ranef(lmer_out_3lvl_dbl)$NC_ID, paste0("CE", "_hlm")) %>% 
  tibble::rownames_to_column("NC_ID") %>% 
  as_tibble() %>% 
  mutate(NC_ID = as.character(NC_ID))

save(eb_est_3lvl_dbl, file = "./data/derived/sensitivity/eb_est_3lvl_dbl.RData")

eb_est_2lvl %>% 
  rename(CE_hlm_2lvl = CE_hlm) %>%
  inner_join(eb_est_3lvl) %>%
  rename(CE_hlm_3lvl = CE_hlm) %>%
  select(-NC_ID) %>% cor()

# Staple them to every other estimate to compare

save(CE_eb_nc_fixed_reliability, file = "./data/derived/sensitivity/CE_eb_nc_fixed_reliability.RData")
