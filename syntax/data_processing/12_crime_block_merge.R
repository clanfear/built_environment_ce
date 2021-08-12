library(tidyverse)
source("./syntax/project_functions.R")

load("./data/derived/cpd_crimes_block.RData")
load("./data/derived/crosswalks/complete_crosswalk.RData")
load("./data/derived/ccahs_sso_block.RData")
load("./data/derived/block_pop_estimates_2000.RData")


ccahs_block_sso_crime <- complete_crosswalk %>%
  select(ccahs_nc, census_tract_6, census_block, ccahs_tract, ccahs_sso_block) %>%
  inner_join(ccahs_sso_block, by = c("ccahs_nc" = "NC_ID" , "ccahs_tract" = "TRACT_ID" , "ccahs_sso_block" = "BLOCK_ID")) %>%
  group_by(ccahs_nc, census_tract_6, census_block, ccahs_tract) %>%
  summarize(across(-ccahs_sso_block, ~mean(.)), n_sso_blocks = n_distinct(ccahs_sso_block)) %>%
  ungroup() %>%
  left_join(cpd_crimes_block %>% 
              filter(year == 2004) %>% 
              select(-year) %>% 
              rename_with(~ paste0(., "_2004"), -c(census_tract_6, census_block))) %>%
  left_join(cpd_crimes_block %>% 
              filter(year %in% 2004:2006) %>% 
              group_by(census_tract_6, census_block) %>% 
              summarize(across(matches("CRIME"), ~sum(.))) %>%
              rename_with(~ paste0(., "_2004_2006"), -c(census_tract_6, census_block))) %>%
  rename_with(~ paste0(., "_2001"), -c(matches("[0-9]$"), ccahs_nc, census_tract_6, census_block, ccahs_tract, n_sso_blocks)) %>%
  inner_join(block_pop_estimates_2000 %>% # Some of the blocks here are combined in the SSO data
               group_by(phdcn_nc, ccahs_nc, census_tract_6, census_block, ccahs_tract) %>%
               summarize(area_block = sum(area_block), 
                         population_block = sum(population_block),
                         density_block = population_block / area_block))

ccahs_block_sso_crime %>% list_missing()

ccahs_block_sso_crime %>% select(where(is.numeric)) %>% cor()

save(ccahs_block_sso_crime, file = "./data/derived/ccahs_block_sso_crime.RData")


