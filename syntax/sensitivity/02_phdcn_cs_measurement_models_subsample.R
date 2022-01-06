# This file takes the processed PHDCN-CS data and extracts empirical bayes
# estimates of neighborhood measures:
# Collective Efficacy (Cohesion and Trust + Control Expectations)
# Police Efficacy
# Legal Cynicism
# Perceived Violence
# Violent Victimization

library(tidyverse)
library(lme4)
library(lavaan)
source("./syntax/project_functions.R")
load("./data/derived/sensitivity/CE_eb_nc_subset_2001.RData")
load("./data/derived/crosswalks/nc_crosswalk.RData")
load("./data/derived/phdcn_cs_individual_long.RData")
load("./data/derived/phdcn_cs_individual_wide.RData")

phdcn_cs_individual_long_subset <- phdcn_cs_individual_long %>%
  left_join(nc_crosswalk, by = c("NC_ID"="phdcn_nc")) %>%
  filter(ccahs_nc %in% CE_eb_nc_subset_2001$ccahs_nc)

phdcn_cs_individual_long_subset_trunc <- phdcn_cs_individual_long_subset %>% 
  filter(str_detect(measure, "^(INF_|COHTR_)" )) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  group_by(NC_ID) %>% 
  slice_sample(n = 10) %>%
  ungroup() %>%
  pivot_longer(matches("^(INF_|COHTR_)"), names_to = "measure", values_to = "value")



# 3 Levels: In-person, between-person, between-neighborhood

eb_estimate_hlm <- function(x, df = phdcn_cs_individual_long_subset){
  filter_x <- ifelse(x == "CE", "^(INF_|COHTR_)", paste0("^", x, "_"))
  lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sep_div + 
                     FAM_single + HOMEOWN + RACE_latino + RACE_black + 
                     MOBILITY + AGE + YRS_IN_NEIGHB + SES + (1|NC_ID/RESP_ID),  
                           ## Standardize all (including binary) predictors
                           data = df %>% 
                             filter(str_detect(measure, filter_x)) %>%
                             mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, - ccahs_nc), ~standardize(.))),
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
eb_nc_hlm_subset <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_hlm(.x, phdcn_cs_individual_long_subset))


# Staple them to every other estimate to compare

load("./data/derived/phdcn_nc.RData")

phdcn_nc %>% 
  select(NC_ID, COHTR_1995, INF_1995) %>%
  inner_join(eb_nc_hlm_subset) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  mutate(CE_1995_calc = (INF_1995 + COHTR_1995)/2) %>% # Calc as average of scales
  cor()
# Everything pretty highly correlated
# R = 0.915 between HLM and SEM

CE_eb_nc_subset_1995 <- eb_nc_hlm_subset %>% 
  rename_with(~paste0(., "_1995"), .cols = -NC_ID) %>%
  rename(phdcn_nc = NC_ID) %>%
  mutate(across(-phdcn_nc, ~ standardize(.)))
save(CE_eb_nc_subset_1995, file = "./data/derived/sensitivity/CE_eb_nc_subset_1995.RData")
