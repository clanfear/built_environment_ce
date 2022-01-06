library(tidyverse)
library(lme4)
library(piecewiseSEM)
source("./syntax/project_functions.R")
load("./data/analytical_data/ccahs_block_analytical.RData")

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
              "BE_pr_commer_dest_onstreet_block_2001",
              "BE_pr_recreation_block_2001",
              "BE_pr_parking_block_2001",
              "MIXED_LAND_USE_2001")
be_block_int <- paste0(be_block, "*", "CE_hlm_2001")
be_block_int_disadv <- paste0(be_block, "*", "FAC_disadv_2000")
density_block <- c("density_block",
                   "density_block_2")
res <- "(1|ccahs_nc)"

main_hlm_formulas            <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block, density_block, res), collapse = " + "))
main_hlm_formulas_nore       <- str_remove(main_hlm_formulas, " \\+ \\(1\\|ccahs_nc\\)")
ce_2000_formula         <- paste0(ce_2001, " ~ ", paste(c(ce_1995, ivs_nc, be_block, density_block), collapse = " + "))
be_formulas             <- paste0(be_block, " ~ ", paste(c(ce_1995, ivs_nc, density_block, res), collapse = " + "))

main_hlm_formulas_nobe            <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, density_block, res), collapse = " + "))
main_hlm_formulas_nobe_nore       <- str_remove(main_hlm_formulas_nobe, " \\+ \\(1\\|ccahs_nc\\)")

base_second_stage_nobe_list <- list(
  "Homicide/Gun Assault" = glmer.nb(formula(main_hlm_formulas_nobe[1]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Robbery"     = glmer.nb(formula(main_hlm_formulas_nobe[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  "Violent"     = glmer.nb(formula(main_hlm_formulas_nobe[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Property"    = glmer.nb(formula(main_hlm_formulas_nobe[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
)
save(base_second_stage_nobe_list, file = "./output/base_second_stage_nobe_list.RData")

main_hlm_formulas_polydis         <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, "FAC_disadv_2000_2", be_block, density_block,  res), collapse = " + "))
main_hlm_formulas_polydis_nore       <- str_remove(main_hlm_formulas_polydis, " \\+ \\(1\\|ccahs_nc\\)")

base_second_stage_polydis_list <- list(
  "Homicide/Gun Assault" = glmer.nb(formula(main_hlm_formulas_polydis[1]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Robbery"     = glmer.nb(formula(main_hlm_formulas_polydis[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7))),
  "Violent"     = glmer.nb(formula(main_hlm_formulas_polydis[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Property"    = glmer.nb(formula(main_hlm_formulas_polydis[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
)
save(base_second_stage_polydis_list, file = "./output/base_second_stage_polydis_list.RData")