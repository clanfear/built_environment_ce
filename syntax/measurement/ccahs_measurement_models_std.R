# This file takes the processed CCAHS data and extracts empirical bayes
# estimates of neighborhood measures, but standardizes DVs first.
# This results in identical estimates, so the other measurement model output
# is used (only because it was done first).


library(tidyverse)
library(lme4)
library(lavaan)
source("./syntax/project_functions.R")
load("./data/chicago/derived/ccahs_individual_long.RData")
load("./data/chicago/derived/ccahs_individual_wide.RData")

# Note this is not the same function as PHDCN due to formula difference

ccahs_individual_long_std <- ccahs_individual_long %>%
  group_by(measure) %>%
  mutate(value = standardize(value)) %>%
  ungroup()

eb_estimate_hlm <- function(x, df = ccahs_individual_long_std){
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

eb_nc_hlm <- map_dfc(c("CE", "PE", "LC", "INF", "COHTR", "PV", "TE", "KT"), ~ eb_estimate_hlm(.x, ccahs_individual_long_std))
cor(eb_nc_hlm[,-1])

vict_model <- glmer(VICT_violent_ever ~ FEMALE + FAM_married + FAM_sepdiv + 
                      FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                      years_in_home + SES + (1|NC_ID),  
                    ## Standardize all (including binary) predictors
                    data = ccahs_individual_long_std %>% 
                      group_by(RESP_ID) %>%
                      slice(1L) %>% ungroup() %>%
                      mutate(across(c(-value, -measure, -VICT_violent_ever, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                    control = glmerControl(optimizer = "bobyqa"), family = binomial)
eb_nc_vict <- setNames(ranef(vict_model)$NC_ID, "VICT_hlm") %>% 
  tibble::rownames_to_column("NC_ID") %>% 
  as_tibble() %>% 
  mutate(NC_ID = as.character(NC_ID))
message(paste0("VICT Reliability: ", round(lme_reliability(vict_model),3)))


# Staple them to every other estimate to compare

load("./data/chicago/derived/ccahs_nc.RData")

ccahs_nc %>%
  select(NC_ID, CE_2001) %>%
  inner_join(eb_nc_hlm) %>%
  inner_join(eb_nc_vict) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  # mutate(CE_1995_est  =  CE_2001  - CE_1995_2001, # Cal CE using change score in CCAHS
  #        CE_1995_calc = (INF_1995 + COHTR_1995)/2) %>% # Calc as average of scales
  cor()

# Everything pretty highly correlated

CE_PE_LC_PV_eb_nc_2001_std <- eb_nc_hlm %>%
  inner_join(eb_nc_vict) %>%
  rename_with(~paste0(., "_2001_std"), .cols = -NC_ID) %>%
  rename(ccahs_nc = NC_ID) %>%
  mutate(across(-ccahs_nc, ~ standardize(.)))
save(CE_PE_LC_PV_eb_nc_2001_std, file = "./data/chicago/derived/CE_PE_LC_PV_eb_nc_2001_std.RData")
