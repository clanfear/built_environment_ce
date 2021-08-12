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
load("./data/derived/phdcn_cs_individual_long.RData")
load("./data/derived/phdcn_cs_individual_wide.RData")

# 3 Levels: In-person, between-person, between-neighborhood

eb_estimate_hlm <- function(x, df = phdcn_cs_individual_long){
  filter_x <- ifelse(x == "CE", "^(INF_|COHTR_)", paste0("^", x, "_"))
  lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sep_div + 
                     FAM_single + HOMEOWN + RACE_latino + RACE_black + 
                     MOBILITY + AGE + YRS_IN_NEIGHB + SES + (1|NC_ID/RESP_ID),  
                           ## Standardize all (including binary) predictors
                           data = df %>% 
                             filter(str_detect(measure, filter_x)) %>%
                             mutate(across(c(-value, -measure, -NC_ID, -RESP_ID), ~standardize(.))),
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
eb_nc_hlm <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_hlm(.x, phdcn_cs_individual_long))

############
# SEM models
############

# Crude single factor CE model; with only two factors can't get much leverage on
# second-order CE factor. Two factor only useful if going to try interaction later... and I'm not.

# Not calculating factors here for the two indicator constructs.

measure_formula_2fac <- "
         COHTR =~ COHTR_closeknit + COHTR_help + COHTR_getalong + COHTR_sharevalues + COHTR_trust  
         INF  =~     INF_skip + INF_graffiti + INF_disrespect + INF_fight + INF_firestation
         COHTR ~~ INF
"

measure_formula_1fac <- "
         CE =~ COHTR_closeknit + COHTR_help + COHTR_getalong + COHTR_sharevalues + COHTR_trust + 
                 INF_skip + INF_graffiti + INF_disrespect + INF_fight + INF_firestation"

# Pairwise maximum likelihood for missing ordinal data
# Not sure of equivalence between this method and the leave-out HLM approach

phdcn_cs_2fac_sem <- sem(measure_formula_2fac, data = phdcn_cs_individual_wide , 
                         missing = "available.cases", estimator = "PML", 
                         std.lv = TRUE)
phdcn_cs_1fac_sem <- sem(measure_formula_1fac, data = phdcn_cs_individual_wide , missing = "available.cases", estimator = "PML", std.lv = TRUE)
anova(phdcn_cs_2fac_sem, phdcn_cs_1fac_sem) # 4 factor definitely preferred

scores_1fac <- lavPredict(phdcn_cs_1fac_sem,  method = "ML", type = "lv")
scores_2fac <- lavPredict(phdcn_cs_2fac_sem,  method = "ML", type = "lv")
scores_cohtr_inf <- as.data.frame(scores_2fac) %>%
  select(COHTR, INF)

# Pull maximum likelihood factor scores and merge to original data
phdcn_cs_eb_indiv <- cbind(scores_1fac, 
                           scores_cohtr_inf,
                           phdcn_cs_individual_wide)

# Center all predictors, run two-level HLM: within/between-neighborhood

eb_estimate_sem <- function(x, df = phdcn_cs_eb_indiv){
  lmer_out <- lmer(formula(paste0(x, "~ FEMALE + FAM_married + FAM_sep_div + FAM_single + HOMEOWN + RACE_latino + RACE_black + MOBILITY + AGE + YRS_IN_NEIGHB + SES + (1|NC_ID)")), 
                                  data = df %>% mutate(across(c(-NC_ID, -RESP_ID), ~standardize(.))))
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

eb_nc_sem <- map_dfc(c("CE", "INF", "COHTR"), ~ eb_estimate_sem(.x, phdcn_cs_eb_indiv))

# Staple them to every other estimate to compare

load("./data/derived/phdcn_nc.RData")

phdcn_nc %>% 
  select(NC_ID, COHTR_1995, INF_1995) %>%
  inner_join(eb_nc_hlm) %>%
  inner_join(eb_nc_sem) %>%
  select(-NC_ID) %>%
  mutate(across(everything(), ~ standardize(.))) %>%
  mutate(CE_1995_calc = (INF_1995 + COHTR_1995)/2) %>% # Calc as average of scales
  cor()
# Everything pretty highly correlated
# R = 0.915 between HLM and SEM

CE_eb_nc_1995 <- eb_nc_hlm %>% 
  inner_join(eb_nc_sem) %>%
  rename_with(~paste0(., "_1995"), .cols = -NC_ID) %>%
  rename(phdcn_nc = NC_ID) %>%
  mutate(across(-phdcn_nc, ~ standardize(.)))
save(CE_eb_nc_1995, file = "./data/derived/CE_eb_nc_1995.RData")
