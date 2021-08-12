# This file takes the processed CCAHS data and extracts empirical bayes
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
load("./data/derived/ccahs_individual_long.RData")
load("./data/derived/ccahs_individual_wide.RData")

# Note this is not the same function as PHDCN due to formula difference

eb_estimate_hlm <- function(x, df = ccahs_individual_long){
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

eb_nc_hlm <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_hlm(.x, ccahs_individual_long))

############
# SEM models
############

# Crude single factor model; with only two factors can't get much leverage on
# second-order CE factor

measure_formula_1fac <- "
         CE =~ COHTR_closeknit + COHTR_help + COHTR_getalong + COHTR_sharevalues + COHTR_trust  +    
               INF_skip + INF_graffiti + INF_disrespect + INF_fight + INF_firestation"
measure_formula_2fac <- "
         COHTR =~ COHTR_closeknit + COHTR_help + COHTR_getalong + COHTR_sharevalues + COHTR_trust  
         INF =~   INF_skip + INF_graffiti + INF_disrespect + INF_fight + INF_firestation
         COHTR ~~ INF"


# Pairwise maximum likelihood for missing ordinal data
# Not sure of equivalence between this method and the leave-out HLM approach

ccahs_1fac_sem <- sem(measure_formula_1fac, data = ccahs_individual_wide , missing = "available.cases", estimator = "PML", std.lv = TRUE)
ccahs_2fac_sem <- sem(measure_formula_2fac, data = ccahs_individual_wide , missing = "available.cases", estimator = "PML", std.lv = TRUE)
anova(ccahs_1fac_sem, ccahs_2fac_sem) # 4 factor definitely preferred


scores_1fac <- lavPredict(ccahs_1fac_sem,  method = "ML", type = "lv")
scores_2fac <- lavPredict(ccahs_2fac_sem,  method = "ML", type = "lv")
scores_cohtr_inf <- as.data.frame(scores_2fac) %>%
  select(COHTR, INF)


# Pull maximum likelihood factor scores and merge to original data
ccahs_eb_indiv <- cbind(scores_1fac, 
                        scores_cohtr_inf,
                        ccahs_individual_wide)

# Center all predictors, run two-level HLM: within/between-neighborhood

eb_estimate_sem <- function(x, df = ccahs_eb_indiv){
  lmer_out <- lmer(formula(paste0(x, "~ FEMALE + FAM_married + FAM_sepdiv + FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + years_in_home + SES + (1|NC_ID)")), 
                   data = df %>% mutate(across(c(-NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))))
  message(paste0(x, " SEM Reliability: ", round(lme_reliability(lmer_out),3))) # Lower reliability--not sure if this is accurate
  if(x == "CE"){ # Crude way to only get NC_ID once
    eb_est <- setNames(ranef(lmer_out)$NC_ID, paste0(x, "_sem")) %>% 
      tibble::rownames_to_column("NC_ID") %>% 
      as_tibble() %>% 
      mutate(NC_ID = as.character(NC_ID))
  } else {
    eb_est <- setNames(ranef(lmer_out)$NC_ID, paste0(x, "_sem"))
  }
  return(eb_est)
}

eb_nc_sem <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_sem(.x, ccahs_eb_indiv))

# Staple them to every other estimate to compare

load("./data/derived/ccahs_nc.RData")

ccahs_nc %>%
  select(NC_ID, CE_2001) %>%
  inner_join(eb_nc_sem) %>%
  inner_join(eb_nc_hlm) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  # mutate(CE_1995_est  =  CE_2001  - CE_1995_2001, # Cal CE using change score in CCAHS
  #        CE_1995_calc = (INF_1995 + COHTR_1995)/2) %>% # Calc as average of scales
  cor()

# Everything pretty highly correlated

CE_eb_nc_2001 <- eb_nc_hlm %>%
  inner_join(eb_nc_sem) %>%
  rename_with(~paste0(., "_2001"), .cols = -NC_ID) %>%
  rename(ccahs_nc = NC_ID) %>%
  mutate(across(-ccahs_nc, ~ standardize(.)))
save(CE_eb_nc_2001, file = "./data/derived/CE_eb_nc_2001.RData")
