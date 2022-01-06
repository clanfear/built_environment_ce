library(tidyverse)
source("./syntax/project_functions.R")
load("./data/derived/sensitivity/street_block.RData")
load("./data/derived/ccahs_block_sso_crime.RData")
load("./data/derived/ltdb_factors_wide.RData")
load("./data/derived/CE_eb_nc_2001.RData")
load("./data/derived/CE_eb_nc_1995.RData")
load("./data/derived/sensitivity/CE_hlm_block_2001.RData")
load("./data/derived/sensitivity/ltdb_factors_tract_wide.RData")
load("./data/derived/sensitivity/CE_eb_nc_fixed_reliability.RData")
load("./data/derived/sensitivity/eb_est_3lvl_dbl.RData")
# Reduce data to higher relia bility subset

load("./data/derived/sensitivity/CE_eb_nc_subset_1995.RData")
load("./data/derived/sensitivity/CE_eb_nc_subset_2001.RData")
load("./data/derived/phdcn_nc.RData")



ccahs_block_analytical_subset <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  right_join(CE_eb_nc_subset_2001, by = "ccahs_nc") %>%
  left_join(CE_eb_nc_subset_1995, by = "phdcn_nc") %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  select(CRIME_homicide_assault_battery_gun_2004_2006,
         CRIME_robbery_2004_2006,
         CRIME_violent_2004_2006,
         CRIME_property_2004_2006,
         CE_hlm_1995,
         CE_hlm_2001,
         FAC_disadv_2000,
         FAC_stability_2000, 
         FAC_hispimm_2000, 
         density_ltdb_nc_2000,
         BE_pr_bar_onstreet_block_2001,
         BE_pr_liquor_onstreet_block_2001,
         BE_pr_vacant_onstreet_block_2001,
         BE_pr_abandoned_bld_onstreet_block_2001,
         BE_pr_commer_dest_onstreet_block_2001,
         BE_pr_recreation_block_2001,
         BE_pr_parking_block_2001,
         MIXED_LAND_USE_2001,
         CE_hlm_2001,
         orig_density_block = density_block,
         ccahs_nc) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2],
         ccahs_nc = as.factor(as.numeric(as.factor(ccahs_nc)))) %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical_subset, file = "./data/derived/sensitivity/ccahs_block_analytical_subset.RData")

# Pooled measurement model

load("./data/derived/sensitivity/eb_est_all.RData")

ccahs_block_analytical_pooled <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  inner_join(eb_est_all, by = c("phdcn_nc", "ccahs_nc")) %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  select(CRIME_homicide_assault_battery_gun_2004_2006,
         CRIME_robbery_2004_2006,
         CRIME_violent_2004_2006,
         CRIME_property_2004_2006,
         CE_hlm_1995 = CE_hlm_pooled_1995,
         CE_hlm_2001 = CE_hlm_pooled_2001,
         FAC_disadv_2000,
         FAC_stability_2000, 
         FAC_hispimm_2000, 
         density_ltdb_nc_2000,
         BE_pr_bar_onstreet_block_2001,
         BE_pr_liquor_onstreet_block_2001,
         BE_pr_vacant_onstreet_block_2001,
         BE_pr_abandoned_bld_onstreet_block_2001,
         BE_pr_commer_dest_onstreet_block_2001,
         BE_pr_recreation_block_2001,
         BE_pr_parking_block_2001,
         MIXED_LAND_USE_2001,
         orig_density_block = density_block,
         ccahs_nc) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2],
         ccahs_nc = as.factor(as.numeric(as.factor(ccahs_nc)))) %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical_pooled, file = "./data/derived/sensitivity/ccahs_block_analytical_pooled.RData")

# Lower reliability PHDCN

load("./data/derived/sensitivity/CE_eb_nc_trunc_1995.RData")


ccahs_block_analytical_trunc <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  inner_join(CE_eb_nc_trunc_1995, by = c("phdcn_nc")) %>%
  inner_join(CE_eb_nc_1995, by = c("phdcn_nc")) %>%
  inner_join(CE_eb_nc_2001, by = c("ccahs_nc")) %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  select(
         CRIME_homicide_assault_battery_gun_2004_2006,
         CRIME_robbery_2004_2006,
         CRIME_violent_2004_2006,
         CRIME_property_2004_2006,
         CE_hlm_1995 = CE_hlm_trunc_1995,
         CE_hlm_2001,
         FAC_disadv_2000,
         FAC_stability_2000, 
         FAC_hispimm_2000, 
         density_ltdb_nc_2000,
         BE_pr_bar_onstreet_block_2001,
         BE_pr_liquor_onstreet_block_2001,
         BE_pr_vacant_onstreet_block_2001,
         BE_pr_abandoned_bld_onstreet_block_2001,
         BE_pr_commer_dest_onstreet_block_2001,
         BE_pr_recreation_block_2001,
         BE_pr_parking_block_2001,
         MIXED_LAND_USE_2001,
         orig_density_block = density_block,
         ccahs_nc) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2],
         ccahs_nc = as.factor(as.numeric(as.factor(ccahs_nc)))) %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical_trunc, file = "./data/sensitivity/derived/ccahs_block_analytical_trunc.RData")

# ----
# Streets


ccahs_block_analytical_streets <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  left_join(ltdb_factors_tract_wide) %>%
  inner_join(CE_eb_nc_1995, by = c("phdcn_nc")) %>%
  inner_join(CE_eb_nc_2001, by = c("ccahs_nc")) %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  left_join(phdcn_nc %>%
    select(NC_ID,HOM_RATE_1990, HOM_RATE_1995, CNT_MURDER_1995, VIOLENT_CRIME_1995), by = c("phdcn_nc" = "NC_ID")) %>%
  left_join(CE_hlm_block_2001) %>%
  rename(    orig_density_block = density_block) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2],
         ccahs_nc = as.factor(as.numeric(as.factor(ccahs_nc)))) %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract|street_class_int|street_class_near"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical_streets, file = "./data/derived/sensitivity/ccahs_block_analytical_streets.RData")

# ----
# Varying reliability


ccahs_block_analytical_reliability <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  left_join(ltdb_factors_tract_wide) %>%
  inner_join(CE_eb_nc_1995, by = c("phdcn_nc")) %>%
  inner_join(CE_eb_nc_2001, by = c("ccahs_nc")) %>%
  inner_join(eb_est_3lvl_dbl %>% rename(ccahs_nc = NC_ID)) %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  left_join(CE_hlm_block_2001) %>%
  rename(orig_density_block = density_block) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2],
         ccahs_nc = as.factor(as.numeric(as.factor(ccahs_nc)))) %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract|street_class_int|street_class_near"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical_reliability, file = "./data/derived/sensitivity/ccahs_block_analytical_reliability.RData")
