library(tidyverse)
library(lme4)
library(piecewiseSEM)
source("./syntax/project_functions.R")

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

main_hlm_formulas       <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block, density_block, res), collapse = " + "))
main_hlm_formulas_nore  <- str_remove(main_hlm_formulas, " \\+ \\(1\\|ccahs_nc\\)")
ce_2000_formula         <- paste0(ce_2001, " ~ ", paste(c(ce_1995, ivs_nc, be_block, density_block), collapse = " + "))
be_formulas             <- paste0(be_block, " ~ ", paste(c(ce_1995, ivs_nc, density_block, res), collapse = " + "))

#-------------------
# Swap between data sets here, rerun models
# psem doesn't play well inside functions due to scoping, I think
ccahs_block_analytical <- ccahs_block_analytical_reliability %>%
  mutate(CE_hlm_2001 = CE_hlm)

psem_hlm_sensitivity_list <- psem(
  MASS::glm.nb(formula(main_hlm_formulas_nore[1]), data = ccahs_block_analytical), #, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[2]), data = ccahs_block_analytical), #, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[3]), data = ccahs_block_analytical), #, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[4]), data = ccahs_block_analytical), #, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  lm(formula(ce_2000_formula),  data = ccahs_block_analytical)
)
psem_hlm_sensitivity_list_summary <- summary(psem_hlm_sensitivity_list)
psem_hlm_sensitivity_list_summary$coefficients %>% {.[,-9]} %>% filter(Predictor == "CE_hlm_2001" | Response == "CE_hlm_2001")
psem_hlm_sensitivity_list_summary$coefficients %>% {.[,-9]} %>% filter(str_detect(Response, "CRIME") & Predictor == "CE_hlm_2001")

ccahs_block_analytical <- ccahs_block_analytical_reliability 

psem_hlm_sensitivity_list_orig <- psem(
  MASS::glm.nb(formula(main_hlm_formulas_nore[1]), data = ccahs_block_analytical), # control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[2]), data = ccahs_block_analytical), # control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[3]), data = ccahs_block_analytical), # control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  MASS::glm.nb(formula(main_hlm_formulas_nore[4]), data = ccahs_block_analytical), # control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  lm(formula(ce_2000_formula),  data = ccahs_block_analytical)
)
psem_hlm_sensitivity_list_summary_orig <- summary(psem_hlm_sensitivity_list_orig)
psem_hlm_sensitivity_list_summary_orig
psem_hlm_sensitivity_list_summary_orig$coefficients %>% {.[,-9]} %>% filter(Predictor == "CE_hlm_2001" | Response == "CE_hlm_2001")
psem_hlm_sensitivity_list_summary_orig$coefficients %>% {.[,-9]} %>% filter(str_detect(Response, "CRIME") & Predictor == "CE_hlm_2001")
