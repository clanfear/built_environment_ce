library(tidyverse)
library(piecewiseSEM)
library(flextable)
library(ftExtra)

load("./data/chicago/derived/psem_hlm_list_summary.RData")
load("./data/chicago/derived/psem_hlm_int_list_summary.RData")

# SECOND STAGE 
second_stage_data <- psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff (2001)"      = "CE_hlm_2001",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commercial Dest."      = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
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
                    "Density (Block)^2"    )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

dsep <- c(" "="Past Coll. Eff.\nd-Sep. P-value", setNames(round(psem_hlm_list_summary$dTable$P.Value,2), 
                 str_remove_all(psem_hlm_list_summary$dTable$Independ.Claim, 
                                "^CRIME_|_2004.*")))
r2vals_2s <- c(" "="R2", setNames(round(psem_hlm_list_summary$R2$Marginal[1:5],2), 
                                          str_remove_all(psem_hlm_list_summary$R2$Response[1:5], 
                                                         "^CRIME_|_2004.*")))


set_flextable_defaults(digits = 2)

second_stage_data %>% 
  group_by(Level) %>%
  as_flextable(col_keys = c("Predictor", "Homicide", "Gun Assault", "Robbery", "Violent", "Property"), hide_grouplabel = TRUE) %>%
  compose(j = "Homicide", value = as_paragraph(homicide_Estimate, "\n", "(", homicide_Std.Error,")")) %>%
  compose(j = "Gun Assault", value = as_paragraph(assault_battery_gun_Estimate, "\n", "(", assault_battery_gun_Std.Error,")")) %>%
  compose(j = "Robbery", value = as_paragraph(robbery_Estimate, "\n", "(", robbery_Std.Error,")" )) %>%
  compose(j = "Violent", value = as_paragraph(violent_Estimate, "\n", "(", violent_Std.Error,")")) %>%
  compose(j = "Property", value = as_paragraph(property_Estimate, "\n", "(", property_Std.Error,")")) %>%
  set_header_labels(Predictor = "", Homicide = "Homicide", `Gun Assault`="Gun Assault", Robbery = "Robbery", Violent = "Violent", Property = "Property") %>%
  bold(i = ~ homicide_p_sig == TRUE, j = "Homicide") %>%
  bold(i = ~ assault_battery_gun_p_sig == TRUE, j = "Gun Assault") %>%
  bold(i = ~ robbery_p_sig == TRUE, j = "Robbery") %>%
  bold(i = ~ violent_p_sig == TRUE, j = "Violent") %>%
  bold(i = ~ property_p_sig == TRUE, j = "Property") %>% 
  align(i = c(2:6, 8:17), j = 2:6, align = "center", part = "body") %>%
  padding(i = c(2:6, 8:17), j = 1, padding.left = 20) %>%
  align(j = 2:6, align = "center", part = "header") %>%
  italic(i = c(1,7)) %>%
  add_footer_row(values = r2vals_2s, colwidths = rep(1,6)) %>%
  add_footer_row(values = dsep, colwidths = rep(1,6)) %>%
  align(j = 2:6, align = "center", part = "footer") %>%
  border_remove() %>%
  save_as_docx(path = "./output/chicago/tables/second_stage_table.docx")

# FIRST STAGE

first_stage_data <-  psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "^CRIME|^~~|^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Coll. Eff (1995)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Density (Block)"      ,
             "Density (Block)^2"    )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

ce_data <-  psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commercial Dest."      = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Coll. Eff (1995)"     ,
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
             "Density (Block)^2"    )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

# BOTH

r2vals_both <- c(" "="R2", setNames(round(psem_hlm_list_summary$R2$Marginal[c(6,10,7,11,8,14,13,12,9)], 2), 
                     str_remove_all(psem_hlm_list_summary$R2$Response[c(6,10,7,11,8,14,13,12,9)], 
                                    "^BE_pr_|_block|_2001.*")))
ce_data %>% 
  full_join(first_stage_data) %>%
  group_by(Level) %>%
  as_flextable(col_keys = c("Predictor", 
                            "Collective\nEfficacy", 
                            "Abandoned", 
                            "Bar", 
                            "Commercial\nDestination", 
                            "Liquor", 
                            "Mixed\nLand Use", 
                            "Parking", 
                            "Recreation", 
                            "Vacant"), 
               hide_grouplabel = TRUE) %>%
  compose(j = "Collective\nEfficacy",    value = as_paragraph(CE_hlm_2001_Estimate, "\n", "(", CE_hlm_2001_Std.Error, ")")) %>%
  compose(j = "Abandoned",               i = ~ !is.na(BE_pr_abandoned_bld_onstreet_block_2001_Estimate),              value = as_paragraph(BE_pr_abandoned_bld_onstreet_block_2001_Estimate, "\n", "(", BE_pr_abandoned_bld_onstreet_block_2001_Std.Error, ")")) %>%
  compose(j = "Bar",                     i = ~ !is.na(BE_pr_bar_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_bar_onstreet_block_2001_Estimate,           "\n", "(", BE_pr_bar_onstreet_block_2001_Std.Error,           ")")) %>%
  compose(j = "Commercial\nDestination", i = ~ !is.na(BE_pr_commer_dest_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_commer_dest_onstreet_block_2001_Estimate,   "\n", "(", BE_pr_commer_dest_onstreet_block_2001_Std.Error,   ")")) %>%
  compose(j = "Liquor",                  i = ~ !is.na(BE_pr_liquor_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_liquor_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_liquor_onstreet_block_2001_Std.Error,        ")")) %>%
  compose(j = "Mixed\nLand Use",         i = ~ !is.na(MIXED_LAND_USE_2001_Estimate), value = as_paragraph(MIXED_LAND_USE_2001_Estimate,                     "\n", "(", MIXED_LAND_USE_2001_Std.Error,                     ")")) %>%
  compose(j = "Parking",                 i = ~ !is.na(BE_pr_parking_block_2001_Estimate), value = as_paragraph(BE_pr_parking_block_2001_Estimate,                "\n", "(", BE_pr_parking_block_2001_Std.Error,                ")")) %>%
  compose(j = "Recreation",              i = ~ !is.na(BE_pr_recreation_block_2001_Estimate), value = as_paragraph(BE_pr_recreation_block_2001_Estimate,             "\n", "(", BE_pr_recreation_block_2001_Std.Error,             ")")) %>%
  compose(j = "Vacant",                  i = ~ !is.na(BE_pr_vacant_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_vacant_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_vacant_onstreet_block_2001_Std.Error,        ")")) %>%
  set_header_labels(Predictor                 = "", 
                    `Collective\nEfficacy`    = "Collective\nEfficacy",
                    Abandoned                 = "Abandoned", 
                    Bar                       = "Bar", 
                    `Commercial\nDestination` = "Commercial\nDestination", 
                    Liquor                    = "Liquor", 
                    `Mixed\nLand Use`         = "Mixed\nLand Use", 
                    Parking                   = "Parking", 
                    Recreation                = "Recreation", 
                    Vacant                    = "Vacant"  ) %>%
  bold(i = ~ CE_hlm_2001_p_sig == TRUE, j = "Collective\nEfficacy") %>%
  bold(i = ~ BE_pr_abandoned_bld_onstreet_block_2001_p_sig == TRUE, j = "Abandoned") %>%
  bold(i = ~ BE_pr_bar_onstreet_block_2001_p_sig == TRUE, j = "Bar") %>%
  bold(i = ~ BE_pr_commer_dest_onstreet_block_2001_p_sig == TRUE, j = "Commercial\nDestination") %>%
  bold(i = ~ BE_pr_liquor_onstreet_block_2001_p_sig == TRUE, j = "Liquor") %>%
  bold(i = ~ MIXED_LAND_USE_2001_p_sig == TRUE, j = "Mixed\nLand Use") %>%
  bold(i = ~ BE_pr_parking_block_2001_p_sig == TRUE, j = "Parking") %>%
  bold(i = ~ BE_pr_recreation_block_2001_p_sig == TRUE, j = "Recreation") %>%
  bold(i = ~ BE_pr_vacant_onstreet_block_2001_p_sig == TRUE, j = "Vacant") %>%
  align(i = c(2:6, 8:17), j = 2:7, align = "center", part = "body") %>%
  padding(i = c(2:6, 8:17), j = 1, padding.left = 20) %>%
  align(j = 2:7, align = "center", part = "header") %>%
  italic(i = c(1,7)) %>%
  add_footer_row(values = r2vals_both , colwidths = rep(1, length(r2vals_both))) %>%
  align(j = 2:7, align = "center", part = "footer") %>%
  border_remove() %>%
  save_as_docx(path = "./output/chicago/tables/first_stage_table.docx")

