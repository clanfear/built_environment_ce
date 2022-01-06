# This file takes the processed CCAHS data and extracts empirical bayes
# estimates of neighborhood measures:
# Collective Efficacy (Cohesion and Trust + Control Expectations)
# In addition, this file limits to NCs with a specified number of respondents
# to assess sensitivity to low reliability.


library(tidyverse)
library(lme4)
source("./syntax/project_functions.R")
load("./data/derived/ccahs_individual_wide.RData")
load("./data/derived/phdcn_cs_individual_wide.RData")
load("./data/derived/crosswalks/nc_crosswalk.RData")

pooled_long <- ccahs_individual_wide %>% 
  select(matches("^(COHTR|INF)"), FEMALE,  FAM_married,  FAM_sep_div = FAM_sepdiv ,  
         FAM_single,  HOMEOWN,  RACE_latino,  RACE_black,  AGE,  SES, NC_ID) %>%
  mutate(RESP_ID = row_number(), YEAR = 2001) %>%
  mutate(across(matches("^(COHTR|INF)"), ~as.numeric(.))) %>%
  left_join(nc_crosswalk, by = c("NC_ID"="ccahs_nc")) %>%
  select(-NC_ID) %>%
  rename(NC_ID = phdcn_nc) %>%
  bind_rows(phdcn_cs_individual_wide %>% 
              select(matches("^(COHTR|INF)"), FEMALE,  FAM_married,  FAM_sep_div,  
                     FAM_single,  HOMEOWN,  RACE_latino,  RACE_black,  AGE,  SES, NC_ID) %>%
              mutate(across(matches("^(COHTR|INF)"), 
                            ~ case_when(
                              is.na(.) ~ NA_real_,
                              . == "1" ~ 1,
                              . == "2" ~ 2,
                              . == "3" ~ NA_real_,
                              . == "4" ~ 3,
                              . == "5" ~ 4
                            ))) %>%
              mutate(RESP_ID = row_number() + 5000, YEAR = 1995, SES = standardize(SES))) %>%
  pivot_longer(matches("^(COHTR|INF)"), names_to = "measure", values_to = "value") %>%
  mutate(NC_YEAR = paste0(NC_ID, "_", YEAR))

lme_reliability_pooled_3lvl <- function(x){
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[["NC_ID:YEAR"]] #t00 is between group variance
  s2  <- var_components$var.intercept[["RESP_ID:(NC_ID:YEAR)"]] # s2 is within-group variance
  unique_ids <- insight::get_random(x) %>% distinct(RESP_ID, NC_ID, YEAR)
  n_2001 <- table(unique_ids %>% filter(YEAR == 2001) %>% pull(NC_ID)) # n is neighb sample size
  n_1995 <- table(unique_ids %>% filter(YEAR == 1995) %>% pull(NC_ID)) # n is neighb sample size
  J <- length(n_1995) # number of neighbs
  rel_2001 <- round(sum(t00 / (t00 + s2 / n_2001)) / J, 3)
  rel_1995 <- round(sum(t00 / (t00 + s2 / n_1995)) / J, 3)
  message("HLM Reliability: ", rel_1995, " (1995); ", rel_2001, " (2001)")
}

lmer_out <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sep_div + FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + SES + (1 | YEAR/NC_ID/RESP_ID),  
                   data = pooled_long %>% 
                     filter(str_detect(measure, "^(INF_|COHTR_)")) %>%
                     mutate(across(c(-value, -measure, -YEAR, -NC_ID, -RESP_ID, -NC_YEAR), ~standardize(.))),
                   control = lmerControl(optimizer = "bobyqa"))
lme_reliability_pooled_3lvl(lmer_out)
re_estimates <- ranef(lmer_out)
eb_est_pooled <- setNames(re_estimates$`NC_ID:YEAR`, "CE_hlm") %>% 
      tibble::rownames_to_column("NC_YEAR") %>% 
      as_tibble() %>% 
      separate(NC_YEAR, into = c("NC_ID", "YEAR")) %>%
      mutate(CE_hlm = CE_hlm + re_estimates$YEAR[YEAR,"(Intercept)"]) %>%
  mutate(YEAR = paste0("CE_hlm_pooled_", YEAR)) %>%
  pivot_wider(names_from = YEAR, values_from = CE_hlm) %>%
  rename(phdcn_nc = NC_ID) %>%
  left_join(nc_crosswalk)

# HLM Reliability: 0.739 (1995); 0.552 (2001)

# Staple them to every other estimate to compare

load("./data/derived/CE_eb_nc_1995.RData")
load("./data/derived/CE_eb_nc_2001.RData")

eb_est_all <- eb_est_pooled %>%
  inner_join(CE_eb_nc_1995) %>%
  inner_join(CE_eb_nc_2001) 

eb_est_all %>%
  select(where(is.numeric)) %>%
  cor()
# Doesn't look like it made a difference

save(eb_est_all, file = "./data/derived/sensitivity/eb_est_all.RData")
