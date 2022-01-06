# This file takes the processed CCAHS data and extracts empirical bayes
# estimates of neighborhood measures:
# Collective Efficacy (Cohesion and Trust + Control Expectations)
# In addition, this file limits to NCs with a specified number of respondents
# to assess sensitivity to low reliability.

respondent_cutoff <- 8

library(tidyverse)
library(lme4)
library(lavaan)
source("./syntax/project_functions.R")
load("./data/derived/ccahs_individual_long.RData")
load("./data/derived/ccahs_individual_wide.RData")

# Median respondent count is 8; modal is 7
ccahs_individual_wide %>% count(NC_ID) %>% count(n) %>% mutate(cumsum(nn)/sum(nn))



ccahs_individual_wide %>% 
  group_by(NC_ID) %>% 
  filter(n() >= respondent_cutoff) %>% 
  distinct(NC_ID)

# This cuts to 267 NCs

ccahs_individual_long_subset <- ccahs_individual_wide %>% 
  group_by(NC_ID) %>% 
  filter(n() >= respondent_cutoff) %>% 
  ungroup() %>%
  mutate(across(matches("^(COHTR|INF)"), ~ as.numeric(.))) %>%
  pivot_longer(matches("^(COHTR|INF)"), names_to = "measure", values_to = "value")
  

# Note this is not the same function as PHDCN due to formula difference

eb_estimate_hlm <- function(x, df = ccahs_individual_long_subset){
  filter_x <-    ifelse(x == "CE", "^(INF_|COHTR_)", paste0("^", x, "_"))
  lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sepdiv + 
                     FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                     years_in_home + SES + (1|NC_ID/RESP_ID),  
                   ## Standardize all (including binary) predictors
                   data = df %>% 
                     filter(str_detect(measure, filter_x)) %>%
                     select(-VICT_violent_ever) %>%
                     mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                   control = lmerControl(optimizer = "bobyqa"))
  message(paste0(x, " HLM Reliability: ", round(lme_reliability_3lvl(lmer_out),3))) 
  if(x == "CE"){ # Crude way to only get NC_ID once
    eb_est <- setNames(ranef(lmer_out)$NC_ID, paste0(x, "_hlm")) %>% 
      tibble::rownames_to_column("NC_ID") %>% 
      as_tibble() %>% 
      mutate(NC_ID = as.character(NC_ID))
  } else {
    eb_est <- setNames(ranef(lmer_out)$NC_ID, paste0(x, "_hlm"))
  }
  return(eb_est)
}

eb_nc_hlm_sub <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_hlm(.x, ccahs_individual_long_subset))

# Staple them to every other estimate to compare

load("./data/derived/ccahs_nc.RData")

ccahs_nc %>%
  select(NC_ID, CE_2001) %>%
  inner_join(eb_nc_hlm_sub) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  # mutate(CE_1995_est  =  CE_2001  - CE_1995_2001, # Cal CE using change score in CCAHS
  #        CE_1995_calc = (INF_1995 + COHTR_1995)/2) %>% # Calc as average of scales
  cor()

# Everything pretty highly correlated

CE_eb_nc_subset_2001 <- eb_nc_hlm_sub %>%
  rename_with(~paste0(., "_2001"), .cols = -NC_ID) %>%
  rename(ccahs_nc = NC_ID) %>%
  mutate(across(-ccahs_nc, ~ standardize(.)))
save(CE_eb_nc_subset_2001, file = "./data/derived/sensitivity/CE_eb_nc_subset_2001.RData")

#
load("./data/derived/crosswalks/complete_crosswalk.RData")
lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sepdiv + 
                   FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                   years_in_home + SES + (1|NC_ID/RESP_ID),  
                 ## Standardize all (including binary) predictors
                 data = ccahs_individual_long %>% 
                   filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
                   select(-VICT_violent_ever) %>%
                   mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                 control = lmerControl(optimizer = "bobyqa"))

CE_hlm_block_2001 <- ranef(lmer_out)$`RESP_ID:NC_ID` %>%
  tibble::rownames_to_column("RESP_ID") %>% 
  as_tibble() %>%
  mutate(RESP_ID = str_remove(RESP_ID, ":.*")) %>%
  rename(CE_hlm_indiv_2001 = `(Intercept)`) %>%
  mutate(RESP_ID = as.character(RESP_ID)) %>%
  inner_join(ccahs_individual_long %>%
               select(RESP_ID, BLOCK_ID, NC_ID)) %>%
  rename(census_block = BLOCK_ID, ccahs_nc = NC_ID) %>%
  group_by(ccahs_nc, census_block) %>%
  summarize(CE_hlm_indiv_2001 = mean(CE_hlm_indiv_2001), .groups = "drop") %>%
  right_join(complete_crosswalk %>% filter(!is.na(ccahs_sso_block)) %>% distinct(census_block, census_tract_6 , ccahs_nc)) %>% 
  mutate(CE_hlm_indiv_2001 = ifelse(is.na(CE_hlm_indiv_2001), 0, CE_hlm_indiv_2001))

save(CE_hlm_block_2001, file = "./data/derived/sensitivity/CE_hlm_block_2001.RData")
