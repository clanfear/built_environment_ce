psem_hlm_list_summary$dTable
psem_hlm_list_summary_2.1.0$dTable
psem_nlm_list_oldmodel_freshsummary <- summary(psem_hlm_list)
psem_nlm_list_oldmodel_freshsummary$dTable


psem_hlm_list_original <- psem_hlm_list

dSep(psem_hlm_list_original)

cor(residuals(psem_hlm_list_original))
cor(residuals(psem_hlm_list_new))

psem_hlm_list_original_summary <- summary(psem_hlm_list_original)
psem_hlm_list_new_summary <- summary(psem_hlm_list_new)

psem_hlm_list_original_summary$dTable
psem_hlm_list_new_summary$dTable

cor.test(residuals(psem_hlm_list_original)[, "CRIME_robbery_2004_2006_residuals"] ,residuals(psem_hlm_list_original)[, "CE_hlm_2001_residuals"])
cor.test(residuals(psem_hlm_list_new)[, "CRIME_robbery_2004_2006_residuals"] ,residuals(psem_hlm_list_new)[, "CE_hlm_2001_residuals"])

psem_hlm_list_new$data == psem_hlm_list_original$data

psem_hlm_list_new$data %>% list_missing()
psem_hlm_list_original$data %>% list_missing()

setNames(lapply(colnames(psem_hlm_list_original$data), function(x){
  all.equal(psem_hlm_list_original$data[,x], psem_hlm_list_new$data[,x])
}), colnames(psem_hlm_list_original$data))

setNames(lapply(str_subset(colnames(psem_hlm_list_new$data), "^(CRIME|FAC|BE|CE|density)"), function(x){
  cor(psem_hlm_list_new$data[,x], psem_hlm_list_original$data[,x])
}), str_subset(colnames(psem_hlm_list_new$data), "^(CRIME|FAC|BE|CE|density)"))


psem_hlm_list_original$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")] != psem_hlm_list_new$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")]

cbind(cbind(psem_hlm_list_original$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")],
      psem_hlm_list_new$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")])[,c(1,4,2,5,3,6)], 
      all.equal(psem_hlm_list_original$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")], psem_hlm_list_new$data[,c("FAC_disadv_2000", "FAC_stability_2000", "FAC_hispimm_2000")])
)

as.data.frame(psem_hlm_list_original$data) %>% 
  select(matches("^density")) %>% head()
as.data.frame(psem_hlm_list_new$data) %>% select(matches("^density")) %>% head()
ccahs_block_analytical %>% select(matches("^FAC")) %>% head()

ccahs_block_analytical <- psem_hlm_list_original$data %>% as.data.frame()
psem_original_data <- psem(
  MASS::glm.nb(formula(main_hlm_formulas_nore[1]), data = ccahs_block_analytical),
  glmer.nb(formula(main_hlm_formulas[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[5]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
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
  CRIME_assault_battery_gun_2004_2006       %~~% CRIME_homicide_2004_2006,
  CRIME_robbery_2004_2006                   %~~% CRIME_homicide_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_homicide_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_homicide_2004_2006,
  CRIME_robbery_2004_2006                   %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_violent_2004_2006
)
psem_original_data_summary <- summary(psem_original_data)
psem_original_data_summary

ccahs_block_analytical <- psem_hlm_list_new$data %>% as.data.frame() %>% select(-FAC_disadv_2000_2) %>%
  mutate(new_rob = CRIME_robbery_2004_2006)
psem_new_data <- psem(
  MASS::glm.nb(formula(main_hlm_formulas_nore[1]), data = ccahs_block_analytical),
  glmer.nb(formula(main_hlm_formulas[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  glmer.nb(formula(main_hlm_formulas[5]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
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
  CRIME_assault_battery_gun_2004_2006       %~~% CRIME_homicide_2004_2006,
  CRIME_robbery_2004_2006                   %~~% CRIME_homicide_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_homicide_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_homicide_2004_2006,
  CRIME_robbery_2004_2006                   %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_assault_battery_gun_2004_2006,
  CRIME_violent_2004_2006                   %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_robbery_2004_2006,
  CRIME_property_2004_2006                  %~~% CRIME_violent_2004_2006
)
psem_new_data_summary <- summary(psem_new_data)
psem_new_data_summary

psem_hlm_list_stupid_summary <- summary(psem_hlm_list_stupid)
summary(psem_hlm_list_original, direction = c("CRIME_robbery_2004_2006 <- CE_hlm_2001"))
summary(psem_hlm_list_new, direction = c("CRIME_robbery_2004_2006 <- CE_hlm_2001"))

psem_hlm_list_original$data %>% as_tibble() %>% summary()
psem_hlm_list_new$data %>% as_tibble() %>% summary()
