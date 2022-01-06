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
load("./data/derived/crosswalks/nc_crosswalk.RData")
load("./data/derived/phdcn_cs_individual_long.RData")
load("./data/derived/phdcn_cs_individual_wide.RData")

phdcn_cs_individual_longtrunc <- phdcn_cs_individual_long %>% 
  filter(str_detect(measure, "^(INF_|COHTR_)" )) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  group_by(NC_ID) %>% 
  slice_sample(prop = 0.4) %>%
  ungroup() %>%
  pivot_longer(matches("^(INF_|COHTR_)"), names_to = "measure", values_to = "value")

eb_estimate_hlm <- function(x, df = phdcn_cs_individual_long_subset){
  filter_x <- ifelse(x == "CE", "^(INF_|COHTR_)", paste0("^", x, "_"))
  lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sep_div + 
                     FAM_single + HOMEOWN + RACE_latino + RACE_black + 
                     MOBILITY + AGE + YRS_IN_NEIGHB + SES + (1|NC_ID/RESP_ID),  
                   ## Standardize all (including binary) predictors
                   data = df %>% 
                     filter(str_detect(measure, filter_x)) %>%
                     mutate(across(c(-value, -measure, -NC_ID, -RESP_ID), ~standardize(.))),
                   control = lmerControl(optimizer = "bobyqa"))
  message(paste0(x, " HLM Reliability: ", round(lme_reliability_3lvl_rb2002(lmer_out),3))) 
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

eb_nc_hlm <- map_dfc(c("CE"), ~ eb_estimate_hlm(.x, phdcn_cs_individual_long))
eb_nc_hlm_trunc <- map_dfc(c("CE"), ~ eb_estimate_hlm(.x, phdcn_cs_individual_longtrunc)) %>%
  rename_with(~paste0(., "_trunc"), .cols = -NC_ID)



eb_nc_hlm_trunc %>%
  inner_join(eb_nc_hlm) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  cor()
# Everything pretty highly correlated
# R = 0.915 between HLM and SEM
-2.06 / -2.91
-5.43 / -7.75

CE_eb_nc_trunc_1995 <- eb_nc_hlm_trunc %>% 
  rename_with(~paste0(., "_1995"), .cols = -NC_ID) %>%
  rename(phdcn_nc = NC_ID) %>%
  mutate(across(-phdcn_nc, ~ standardize(.)))
save(CE_eb_nc_trunc_1995, file = "./data/derived/sensitivity/CE_eb_nc_trunc_1995.RData")


load("./data/derived/ltdb_factors_wide.RData")
load("./data/derived/phdcn_nc.RData")

phdcn_nc %>%
  inner_join(CE_eb_nc_trunc_1995, by = c("NC_ID"="phdcn_nc")) %>%
  select(CE_hlm_trunc_1995, CNT_MURDER_1995, VIOLENT_CRIME_1995) %>%
  cor()

load("./data/derived/CE_eb_nc_1995.RData")

phdcn_nc_trunc_1995 <- phdcn_nc %>%
  inner_join(CE_eb_nc_trunc_1995, by = c("NC_ID"="phdcn_nc")) %>%
  inner_join(ltdb_factors_wide, by = c("NC_ID"="phdcn_nc")) %>%
  inner_join(CE_eb_nc_1995, by = c("NC_ID"="phdcn_nc") )

MASS::glm.nb(CNT_MURDER_1995 ~ CE_hlm_trunc_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + density_ltdb_nc_1990, data = phdcn_nc_trunc_1995) %>% summary()
MASS::glm.nb(CNT_MURDER_1995 ~ CE_hlm_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + density_ltdb_nc_1990, data = phdcn_nc_trunc_1995) %>% summary()

lm(VIOLENT_CRIME_1995 ~ CE_hlm_trunc_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + density_ltdb_nc_1990, data = phdcn_nc_trunc_1995) %>% summary()
lm(VIOLENT_CRIME_1995 ~ CE_hlm_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + density_ltdb_nc_1990, data = phdcn_nc_trunc_1995) %>% summary()
