library(tidyverse)
library(lme4)
library(piecewiseSEM)
source("./syntax/project_functions.R")
load("./data/derived/sensitivity/ccahs_block_analytical_subset.RData")
load("./data/derived/sensitivity/ccahs_block_analytical_pooled.RData")
load("./data/derived/sensitivity/ccahs_block_analytical_trunc.RData")
load("./data/derived/sensitivity/ccahs_block_analytical_streets.RData")
load("./data/derived/sensitivity/ccahs_block_analytical_reliability.RData")

dvs <- c("CRIME_homicide_assault_battery_gun_2004_2006",
         "CRIME_robbery_2004_2006",
         "CRIME_violent_2004_2006",
         "CRIME_property_2004_2006")
ce_1995 <- "CE_hlm_1995"
ce_2001 <- "CE_hlm_2001"
ivs_nc <- c("FAC_disadv_2000",
            "FAC_stability_2000", 
            "FAC_hispimm_2000", 
            "FAC_disadv_tract_2000",
            "FAC_stability_tract_2000",
            "FAC_hispimm_tract_2000", 
            "density_ltdb_nc_2000",
            "street_class_near")
be_block <- c("BE_pr_bar_onstreet_block_2001",
              "BE_pr_liquor_onstreet_block_2001",
              "BE_pr_vacant_onstreet_block_2001",
              "BE_pr_abandoned_bld_onstreet_block_2001",
              "BE_pr_recreation_block_2001",
              "BE_pr_commer_dest_onstreet_block_2001",
              "BE_pr_parking_block_2001",
              "MIXED_LAND_USE_2001")
be_block_int <- paste0(be_block, "*", "CE_hlm_2001")
be_block_int_disadv <- paste0(be_block, "*", "FAC_disadv_2000")
density_block <- c("density_block",
                   "density_block_2")
res <- "(1|ccahs_nc)"

main_hlm_formulas       <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block, density_block, "HOM_RATE_1995", res), collapse = " + "))
main_hlm_formulas_nore  <- str_remove(main_hlm_formulas, " \\+ \\(1\\|ccahs_nc\\)")
ce_2000_formula         <- paste0(ce_2001, " ~ ", paste(c(ce_1995, ivs_nc, be_block, "HOM_RATE_1990", density_block), collapse = " + "))
be_formulas             <- paste0(be_block, " ~ ", paste(c(ce_1995, ivs_nc, density_block, "HOM_RATE_1990", res), collapse = " + "))

#-------------------
# Swap between data sets here, rerun models
# psem doesn't play well inside functions due to scoping, I think
ccahs_block_analytical <- ccahs_block_analytical_streets %>%
  select(-matches("_tract_(1990|2000)")) %>%
  filter(!is.na(HOM_RATE_1995) & !is.na(HOM_RATE_1990))

psem_hlm_sensitivity_list <- psem(
  glmer.nb(formula(main_hlm_formulas[1]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  glmer.nb(formula(main_hlm_formulas[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  glmer.nb(formula(main_hlm_formulas[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  lm(formula(ce_2000_formula),  data = ccahs_block_analytical),
  lmer(formula(be_formulas[1]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[2]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[3]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[4]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[5]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[6]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[7]), data = ccahs_block_analytical),
  lmer(formula(be_formulas[8]), data = ccahs_block_analytical),
  # Has got to be a clean way to programmatically generate these as permutations of the above
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_bar_onstreet_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_vacant_onstreet_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_abandoned_bld_onstreet_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_commer_dest_onstreet_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_recreation_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% BE_pr_parking_block_2001,
  BE_pr_liquor_onstreet_block_2001          %~~% MIXED_LAND_USE_2001,
  BE_pr_bar_onstreet_block_2001             %~~% BE_pr_vacant_onstreet_block_2001,
  BE_pr_bar_onstreet_block_2001             %~~% BE_pr_abandoned_bld_onstreet_block_2001,
  BE_pr_bar_onstreet_block_2001             %~~% BE_pr_commer_dest_onstreet_block_2001,
  BE_pr_bar_onstreet_block_2001             %~~% BE_pr_recreation_block_2001,
  BE_pr_bar_onstreet_block_2001             %~~% BE_pr_parking_block_2001,
  BE_pr_bar_onstreet_block_2001             %~~% MIXED_LAND_USE_2001,
  BE_pr_vacant_onstreet_block_2001          %~~% BE_pr_abandoned_bld_onstreet_block_2001,
  BE_pr_vacant_onstreet_block_2001          %~~% BE_pr_commer_dest_onstreet_block_2001,
  BE_pr_vacant_onstreet_block_2001          %~~% BE_pr_recreation_block_2001,
  BE_pr_vacant_onstreet_block_2001          %~~% BE_pr_parking_block_2001,
  BE_pr_vacant_onstreet_block_2001          %~~% MIXED_LAND_USE_2001,
  BE_pr_abandoned_bld_onstreet_block_2001   %~~% BE_pr_commer_dest_onstreet_block_2001,
  BE_pr_abandoned_bld_onstreet_block_2001   %~~% BE_pr_recreation_block_2001,
  BE_pr_abandoned_bld_onstreet_block_2001   %~~% BE_pr_parking_block_2001,
  BE_pr_abandoned_bld_onstreet_block_2001   %~~% MIXED_LAND_USE_2001,
  BE_pr_commer_dest_onstreet_block_2001     %~~% BE_pr_recreation_block_2001,
  BE_pr_commer_dest_onstreet_block_2001     %~~% BE_pr_parking_block_2001,
  BE_pr_commer_dest_onstreet_block_2001     %~~% MIXED_LAND_USE_2001,
  BE_pr_recreation_block_2001               %~~% BE_pr_parking_block_2001,
  BE_pr_recreation_block_2001               %~~% MIXED_LAND_USE_2001,
  BE_pr_parking_block_2001                  %~~% MIXED_LAND_USE_2001,
  CRIME_robbery_2004_2006                   %~~% CRIME_homicide_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_homicide_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_homicide_assault_battery_gun_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_violent_2004_2006
)
psem_hlm_sensitivity_listsummary <- summary(psem_hlm_sensitivity_list)
psem_hlm_sensitivity_listsummary

# Truncated 1995 CE replacing 2003 CE

data.frame(
  truncated = (psem_hlm_sensitivity_listsummary$coefficients %>% {.[,-9]} %>% filter(Predictor == "CE_hlm_1995") %>% pull(Estimate)),
  original = (psem_hlm_list_summary$coefficients %>% {.[,-9]} %>% filter(Predictor == "CE_hlm_1995") %>% pull(Estimate))) %>%
  mutate(param_change = truncated / original) %>%
  pull(param_change) %>% mean()

#

lmer(BE_pr_commer_dest_onstreet_block_2001 ~ CE_hlm_1995 + FAC_disadv_2000 + 
       FAC_stability_2000 + FAC_hispimm_2000 + density_ltdb_nc_2000 + 
       street_class_near + density_block + density_block_2 + (1|ccahs_nc), data = ccahs_block_analytical_streets) %>% BIC()

main_hlm_indiv_formulas <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block, density_block, "CE_hlm_indiv_2001", res), collapse = " + "))

glmer.nb(formula(main_hlm_formulas[1]), data = ccahs_block_analytical_streets, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))) %>% summary()
glmer.nb(formula(main_hlm_formulas[2]), data = ccahs_block_analytical_streets, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e7))) %>% summary()
glmer.nb(formula(main_hlm_formulas[3]), data = ccahs_block_analytical_streets, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e7))) %>% summary()
glmer.nb(formula(main_hlm_formulas[4]), data = ccahs_block_analytical_streets, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))) %>% summary()

ccahs_block_analytical_streets_nconly <- ccahs_block_analytical_streets %>%
  group_by(ccahs_nc) %>%
  summarize(across(c(matches("^BE|^FAC|street_class|density|MIXED|CE_hlm_2001|CE_hlm_1995")), ~mean(.)),
            across(matches("^CRIME"), ~ sum(.))) %>%
  mutate(density_block_2 = density_block^2)

psem_hlm_sensitivity_list_nconly <- psem(
MASS::glm.nb(formula(main_hlm_formulas_nore[1]), data = ccahs_block_analytical_streets_nconly),# , control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))) %>% summary()
MASS::glm.nb(formula(main_hlm_formulas_nore[2]), data = ccahs_block_analytical_streets_nconly),# , control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e7))) %>% summary()
MASS::glm.nb(formula(main_hlm_formulas_nore[3]), data = ccahs_block_analytical_streets_nconly),# , control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e7))) %>% summary()
MASS::glm.nb(formula(main_hlm_formulas_nore[4]), data = ccahs_block_analytical_streets_nconly),# , control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))) %>% summary()
lm(formula(ce_2000_formula),  data = ccahs_block_analytical)
)
summary(psem_hlm_sensitivity_list_nconly)