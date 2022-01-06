library(tidyverse)
library(flextable)
source("./syntax/project_functions.R")
load("./output/psem_hlm_subset_listsummary.RData")
load("./output/psem_hlm_pooled_listsummary.RData")

current_listsummary <- psem_hlm_sensitivity_listsummary
# SECOND STAGE 
second_stage_data <- current_listsummary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block|min_class)"), "Block", "Neighborhood"),
  ) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff (2001)"      = "CE_hlm_2001",
                        "Disadvantage"               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commercial Dest."      = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor\nStores"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2",
                        "Street Type" = "min_class"),
             "Coll. Eff (2001)"     ,
             "Disadvantage"              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Abandoned"            ,
             "Bars"                 ,
             "Commercial Dest."     ,
             "Liquor\nStores"               ,
             "Mixed Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant"               ,
             "Density (Block)"      ,
             "Density (Block)^2"    ,
             "Street Type")) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

dsep <- c(" "="Past Coll. Eff.\nd-Sep. P-value", "", setNames(round(current_listsummary$dTable$P.Value,2), 
                                                          str_remove_all(current_listsummary$dTable$Independ.Claim, 
                                                                         "^CRIME_|_2004.*")))
r2vals_2s <- c(" "="R2", "", setNames(round(current_listsummary$R2$Marginal[1:5],2), 
                                  str_remove_all(current_listsummary$R2$Response[1:5], 
                                                 "^CRIME_|_2004.*")))

set_flextable_defaults(digits = 2)

s2_subset_table <- 
  second_stage_data %>% 
  as_grouped_data(groups = "Level") %>%
  flextable(col_keys = c("Level", "Predictor", "Homicide /\nGun Assault", "Robbery", "Violent", "Property")) %>%
  compose(j = "Homicide /\nGun Assault", value = as_paragraph(homicide_assault_battery_gun_Estimate, "\n", "(", homicide_assault_battery_gun_Std.Error,")")) %>%
  compose(j = "Robbery", value = as_paragraph(robbery_Estimate, "\n", "(", robbery_Std.Error,")" )) %>%
  compose(j = "Violent", value = as_paragraph(violent_Estimate, "\n", "(", violent_Std.Error,")")) %>%
  compose(j = "Property", value = as_paragraph(property_Estimate, "\n", "(", property_Std.Error,")")) %>%
  merge_h_range(i = c(1,6), j1 = 1, j2 = 6) %>%
  set_header_labels(Level = "", Predictor = "Predictor", `Homicide /\nGun Assault`="Homicide /\nGun Assault", Robbery = "Robbery", Violent = "Violent", Property = "Property") %>%
  bold(i = ~ homicide_assault_battery_gun_p_sig == TRUE, j = "Homicide /\nGun Assault") %>%
  bold(i = ~ robbery_p_sig == TRUE, j = "Robbery") %>%
  bold(i = ~ violent_p_sig == TRUE, j = "Violent") %>%
  bold(i = ~ property_p_sig == TRUE, j = "Property") %>% 
  compose(j = 2, i = 17, value = as_paragraph("Density (Block)", as_sup("2"))) %>%
  align(i = c(2:6, 8:17), j = 3:6, align = "center", part = "body") %>%
  align(j = 3:6, align = "center", part = "header") %>%
  italic(i = c(1,6)) %>%
  #add_footer_row(values = r2vals_2s, colwidths = rep(1,6)) %>%
  add_footer_row(values = dsep, colwidths = rep(1,6)) %>%
  align(j = 3:6, align = "center", part = "footer") %>%
  valign(part = "all") %>%
  merge_h_range(j1=1, j2=2, part = "footer") %>%
  # font(fontname = "Latin Modern Roman", part = "all") %>%
  set_table_properties(layout = "fixed") %>%
  border_remove() %>%
  width(j = 1, width = 0.2) %>%
  width(j = 2, width = 1) %>%
  add_footer_row(top=FALSE, values = "Standard errors in parentheses; Bolded estimates significant at 95% level", colwidths = 6) %>%
  fontsize(size = 10, part = "all") %>%
  hline_bottom(border = officer::fp_border(width = 1), part = "body") %>%
  # hline_bottom(border = officer::fp_border(width = 0.5), part = "body") %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))
s2_subset_table
