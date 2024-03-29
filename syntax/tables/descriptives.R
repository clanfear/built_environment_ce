library(tidyverse)
library(flextable)
source("./syntax/project_functions.R")
load("./data/analytical_data/ccahs_block_analytical_unstd.RData")

# Quick check to see quartiles of CE and abandoned; didn't rename from tertiles because lazy
ccahs_block_analytical_unstd %>%
  mutate(abandoned_tertile = ntile(BE_pr_abandoned_bld_onstreet_block_2001, 4),
         CE_tertile = ntile(CE_hlm_2001, 4)) %>%
  count(CE_tertile, abandoned_tertile) %>%
  pivot_wider(values_from = n, names_from = abandoned_tertile, names_prefix = "Abandoned ")

# Descriptives table with density plots in-line. Needs postprocessing for document.
be_descriptives_dat <- ccahs_block_analytical_unstd %>%
  select(-density_block_2, -FAC_disadv_2000_2, -density_block, -CRIME_assault_battery_gun_2004_2006, -CRIME_homicide_2004_2006, -BE_pr_commercial_block_2001) %>%
  mutate(across(c(orig_density_block, density_ltdb_nc_2000), ~ . / 1000)) %>%
  pivot_longer(-ccahs_nc) %>%
  mutate(level = factor(ifelse(str_detect(name, "^(BE|CRIME|MIX|orig|street_class)"), "Block (N=1,641)", "Neighborhood (N=343)"), levels = c("Neighborhood (N=343)", "Block (N=1,641)"))) %>%
  group_by(ccahs_nc, level, name) %>% 
  filter(level == "Block (N=1,641)" | (level == "Neighborhood (N=343)" & row_number()==1)) %>%
  ungroup() %>%
  group_by(level, name) %>%
  mutate(value = ifelse(str_detect(name, "^FAC"), standardize(value, scale =1), value)) %>%
  summarize(across(value, list(Mean = ~mean(.),
                               SD   = ~sd(.),
                               Min  = ~min(.),
                               Max  = ~max(.)), .names = "{.fn}"),
            df = list(value)) %>%
  ungroup() %>%
  mutate(across(-c(name, df, level), ~round(., 2))) %>%
  mutate(name           =
           fct_relevel(
             fct_recode(name,
                        "Homicide / Gun Assault"                = "CRIME_homicide_assault_battery_gun_2004_2006",
                        "Property Crime"                   = "CRIME_property_2004_2006",
                        "Robbery"                    = "CRIME_robbery_2004_2006",
                        "Violent Crime"                    = "CRIME_violent_2004_2006",
                        "Collective Efficacy (2001)" = "CE_hlm_2001",
                        "Collective Efficacy (1995)" = "CE_hlm_1995",
                        "Disadvantage"               = "FAC_disadv_2000",
                        "Stability"                  = "FAC_stability_2000",
                        "Hispanic/Immigrant"         = "FAC_hispimm_2000",
                        "Density (Neighborhood)"     = "density_ltdb_nc_2000",
                        "Abandoned"                  = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                       = "BE_pr_bar_onstreet_block_2001",
                        "Commercial Dest."           = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor"                     = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Land Use"                  = "MIXED_LAND_USE_2001",
                        "Parking"                    = "BE_pr_parking_block_2001",
                        "Recreation"                 = "BE_pr_recreation_block_2001",
                        "Vacant"                     = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"            = "orig_density_block",
                        "Street Class"               = "street_class_near"),
             "Homicide / Gun Assault",
             "Robbery"    ,
             "Violent Crime"    ,
             "Property Crime"   ,
             "Collective Efficacy (2001)"     ,
             "Collective Efficacy (1995)",
             "Disadvantage"              ,
             "Stability"            ,
             "Hispanic/Immigrant",
             "Density (Neighborhood)"    ,
             "Abandoned"            ,
             "Bars"                 ,
             "Commercial Dest."     ,
             "Liquor"               ,
             "Mixed Land Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant"               ,
             "Street Class"         ,
             "Density (Block)"      )) %>%
  arrange(level, name) %>%
  rename(Measure = name) %>%
  relocate(Min, df, Max, .after = last_col())

save(be_descriptives_dat, file = "./output/be_descriptives_dat.RData")
