orig_base <- glmer.nb("CRIME_robbery_2004_2006 ~ CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 +      FAC_hispimm_2000 + density_ltdb_nc_2000 + BE_pr_bar_onstreet_block_2001 +  
    BE_pr_liquor_onstreet_block_2001 + BE_pr_vacant_onstreet_block_2001 +      BE_pr_abandoned_bld_onstreet_block_2001 + BE_pr_commer_dest_onstreet_block_2001 +  
    BE_pr_recreation_block_2001 + BE_pr_parking_block_2001 +      MIXED_LAND_USE_2001 + density_block + density_block_2 + (1 |      ccahs_nc)", data = psem_original_data$data, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))

new_base <- glmer.nb("CRIME_robbery_2004_2006 ~ CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 +      FAC_hispimm_2000 + density_ltdb_nc_2000 + BE_pr_bar_onstreet_block_2001 +  
    BE_pr_liquor_onstreet_block_2001 + BE_pr_vacant_onstreet_block_2001 +      BE_pr_abandoned_bld_onstreet_block_2001 + BE_pr_commer_dest_onstreet_block_2001 +  
    BE_pr_recreation_block_2001 + BE_pr_parking_block_2001 +      MIXED_LAND_USE_2001 + density_block + density_block_2 + (1 |      ccahs_nc)", data = psem_new_data$data, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))

orig_1995 <- glmer.nb("CRIME_robbery_2004_2006 ~ CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 +      FAC_hispimm_2000 + density_ltdb_nc_2000 + BE_pr_bar_onstreet_block_2001 +  
    BE_pr_liquor_onstreet_block_2001 + BE_pr_vacant_onstreet_block_2001 +      BE_pr_abandoned_bld_onstreet_block_2001 + BE_pr_commer_dest_onstreet_block_2001 +  
    BE_pr_recreation_block_2001 + BE_pr_parking_block_2001 +      MIXED_LAND_USE_2001 + density_block + density_block_2 + CE_hlm_1995 + (1 |      ccahs_nc)", data = psem_original_data$data, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))

new_1995 <- glmer.nb("CRIME_robbery_2004_2006 ~ CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 +      FAC_hispimm_2000 + density_ltdb_nc_2000 + BE_pr_bar_onstreet_block_2001 +  
    BE_pr_liquor_onstreet_block_2001 + BE_pr_vacant_onstreet_block_2001 +      BE_pr_abandoned_bld_onstreet_block_2001 + BE_pr_commer_dest_onstreet_block_2001 +  
    BE_pr_recreation_block_2001 + BE_pr_parking_block_2001 +      MIXED_LAND_USE_2001 + density_block + density_block_2 + CE_hlm_1995 + (1 |      ccahs_nc)", data = psem_new_data$data, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))


cbind(psem_new_data$data %>% as_tibble() %>% select(where(is.numeric)) %>% cor() %>% {.[,"CRIME_robbery_2004_2006"]},
psem_original_data$data %>% as_tibble() %>% select(where(is.numeric), -FAC_disadv_2000_2) %>% cor() %>% {.[,"CRIME_robbery_2004_2006"]})

"CRIME_robbery_2004_2006 ~ CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 +      FAC_hispimm_2000 + density_ltdb_nc_2000 + BE_pr_bar_onstreet_block_2001 +  
    BE_pr_liquor_onstreet_block_2001 + BE_pr_vacant_onstreet_block_2001 +      BE_pr_abandoned_bld_onstreet_block_2001 + BE_pr_commer_dest_onstreet_block_2001 +  
    BE_pr_recreation_block_2001 + BE_pr_parking_block_2001 +      MIXED_LAND_USE_2001 + density_block + density_block_2 + CE_hlm_1995 +      (1 | ccahs_nc)"

test2 <- glmer.nb("CRIME_robbery_2004_2006 ~ CE_hlm_2001 + MIXED_LAND_USE_2001 +      BE_pr_parking_block_2001 + BE_pr_recreation_block_2001 +  
            BE_pr_commer_dest_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001 +      BE_pr_vacant_onstreet_block_2001 + BE_pr_liquor_onstreet_block_2001 +  
            BE_pr_bar_onstreet_block_2001 + density_block_2 + density_block +      density_ltdb_nc_2000 + FAC_hispimm_2000 + FAC_stability_2000 +      FAC_disadv_2000 + CE_hlm_1995 + (1 | ccahs_nc)", data = psem_new_data$data, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))


test <- update(bMod, formula(paste(". ~ ", 
                           paste(rev(b[[i]][-2]), collapse = " + "), " + ", 
                           piecewiseSEM:::onlyBars(formula(bMod)))), data = data, control = glmerControl(optimizer="nlminbwrap", optCtrl=list(maxfun=2e7)))

function(df)
apply(df, )

function(x, y){
  all.equal(x, y)
}