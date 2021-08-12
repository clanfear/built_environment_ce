library(tidyverse)
library(flextable)
source("./syntax/project_functions.R")
load("./output/psem_hlm_int_list_summary.RData")

second_stage_data_int <- psem_hlm_int_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "(BE_|MIX|density_block)"), "Block", "Neighborhood"),
         Predictor = str_replace(Predictor, ":", " x ")
  ) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor = ifelse(str_detect(Predictor, " x "), str_replace(Predictor, "CE_hlm_2001", "CE"), Predictor),
         Predictor           =
           str_replace_all(Predictor,c(
             "CE_hlm_2001"                            = "Coll. Eff (2001)"      ,
             "FAC_disadv_2000"                        = "Disadv."               ,
             "FAC_stability_2000"                     = "Stability"             ,
             "FAC_hispimm_2000"                       = "Hispanic /\nImmigrant" ,
             "density_ltdb_nc_2000"                   = "Density (Neighb.)"     ,
             "BE_pr_abandoned_bld_onstreet_block_2001"= "Abandoned"             ,
             "BE_pr_bar_onstreet_block_2001"          = "Bars"                  ,
             "BE_pr_commer_dest_onstreet_block_2001"  = "Commercial Dest."      ,
             "BE_pr_liquor_onstreet_block_2001"       = "Liquor"                ,
             "MIXED_LAND_USE_2001"                    = "Mixed Use"             ,
             "BE_pr_parking_block_2001"               = "Parking"               ,
             "BE_pr_recreation_block_2001"            = "Recreation"            ,
             "BE_pr_vacant_onstreet_block_2001"       = "Vacant"                ,
             "density_block$"                         = "Density (Block)"       ,       
             "density_block_2"                        = "Density (Block)^2"      )),
  ) %>%
  mutate(Predictor =
           fct_relevel(Predictor,
                       "Coll. Eff (2001)"     ,
                       "Disadv."              ,
                       "Stability"            ,
                       "Hispanic /\nImmigrant",
                       "Density (Neighb.)"    ,
                       "Abandoned"            ,
                       "Bars"                 ,
                       "Commercial Dest."     ,
                       "Liquor"               ,
                       "Mixed Use"            ,
                       "Parking"              ,
                       "Recreation"           ,
                       "Vacant"               ,
                       "Density (Block)"      ,
                       "Density (Block)^2"    ,
                       "CE x Abandoned"            ,
                       "CE x Bars"                 ,
                       "CE x Commercial Dest."     ,
                       "CE x Liquor"               ,
                       "CE x Mixed Use"            ,
                       "CE x Parking"              ,
                       "CE x Recreation"           ,
                       "CE x Vacant"               )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

dsep <- c(" "="Past Coll. Eff.\nd-Sep. P-value", "", setNames(round(psem_hlm_int_list_summary$dTable$P.Value,2), 
                                                          str_remove_all(psem_hlm_int_list_summary$dTable$Independ.Claim, 
                                                                         "^CRIME_|_2004.*")))
r2vals_2s <- c(" "="R2", "", setNames(round(psem_hlm_int_list_summary$R2$Marginal[1:5],2), 
                                  str_remove_all(psem_hlm_int_list_summary$R2$Response[1:5], 
                                                 "^CRIME_|_2004.*")))
set_flextable_defaults(digits = 2)

s2_int_table <- 
  second_stage_data_int %>% 
  as_grouped_data(groups = "Level") %>%
  flextable(col_keys = c("Level", "Predictor", "Homicide", "Gun Assault", "Robbery", "Violent", "Property")) %>%
  compose(j = "Homicide", value = as_paragraph(homicide_Estimate, "\n", "(", homicide_Std.Error,")")) %>%
  compose(j = "Gun Assault", value = as_paragraph(assault_battery_gun_Estimate, "\n", "(", assault_battery_gun_Std.Error,")")) %>%
  compose(j = "Robbery", value = as_paragraph(robbery_Estimate, "\n", "(", robbery_Std.Error,")" )) %>%
  compose(j = "Violent", value = as_paragraph(violent_Estimate, "\n", "(", violent_Std.Error,")")) %>%
  compose(j = "Property", value = as_paragraph(property_Estimate, "\n", "(", property_Std.Error,")")) %>%
  set_header_labels(Level = "", Predictor = "Predictor", Homicide = "Homicide", `Gun Assault`="Gun Assault", Robbery = "Robbery", Violent = "Violent", Property = "Property") %>%
  merge_h_range(i = c(1,7), j1 = 1, j2 = 7) %>%
  bold(i = ~ homicide_p_sig == TRUE, j = "Homicide") %>%
  bold(i = ~ assault_battery_gun_p_sig == TRUE, j = "Gun Assault") %>%
  bold(i = ~ robbery_p_sig == TRUE, j = "Robbery") %>%
  bold(i = ~ violent_p_sig == TRUE, j = "Violent") %>%
  bold(i = ~ property_p_sig == TRUE, j = "Property") %>% 
  compose(j = 2, i = 17, value = as_paragraph("Density (Block)", as_sup("2"))) %>%
  align(i = c(2:6, 8:25), j = 3:7, align = "center", part = "body") %>%
  padding(i = c(2:6, 8:25), j = 1, padding.left = 20) %>%
  align(j = 3:7, align = "center", part = "header") %>%
  # add_footer_row(values = r2vals_2s, colwidths = rep(1,7)) %>%
  # add_footer_row(values = dsep, colwidths = rep(1,7)) %>%
  # align(j = 3:7, align = "center", part = "footer") %>%
  valign(part = "all") %>%
  #merge_h_range(j1=1, j2=2, part = "footer") %>%
  # font(fontname = "Latin Modern Roman", part = "all") %>%
  set_table_properties(layout = "fixed") %>%
  border_remove() %>%
  width(j = 1, width = 0.1) %>%
  width(j = 2, width = 0.8) %>%
  add_footer_row(top=FALSE, values = "N = 1641 for all models", colwidths = 7) %>%
  add_footer_row(top=FALSE, values = "Standard errors in parentheses; Bolded estimates significant at 95% level", colwidths = 7) %>%
  fontsize(size = 10, part = "all") %>%
  # hline_bottom(border = officer::fp_border(width = 1), part = "footer") %>%
  hline_bottom(border = officer::fp_border(width = 0.5), part = "body") %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))

save(s2_int_table, file = "./output/s2_int_table.RData")