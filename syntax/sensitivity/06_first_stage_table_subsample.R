library(tidyverse)
library(flextable)
source("./syntax/project_functions.R")

load("./output/sensitivity/psem_hlm_sensitivity_listsummary.RData")

current_listsummary <- psem_hlm_sensitivity_listsummary

first_stage_data <-  current_listsummary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "^CRIME|^~~|^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block|min_class)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff. (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2",
                         "Street Type" = "min_class"),
             "Coll. Eff. (1995)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Density (Block)"      ,
             "Density (Block)^2"    ,
             "Street Type")) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

ce_data <-  current_listsummary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block|min_class)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff. (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commer. Dest."         = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor\nStores"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2",
                        "Street Type" = "min_class"),
             "Coll. Eff. (1995)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Abandoned"            ,
             "Bars"                 ,
             "Commer. Dest."        ,
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

# BOTH

r2vals_both <- c(" "="R2", "", setNames(round(current_listsummary$R2$Marginal[c(6,10,7,11,8,14,13,12,9)], 2), 
                                    str_remove_all(current_listsummary$R2$Response[c(6,10,7,11,8,14,13,12,9)], 
                                                   "^BE_pr_|_block|_2001.*")))
s1_subset_table <- 
  ce_data %>% 
  full_join(first_stage_data) %>%
  as_grouped_data(groups = "Level") %>%
  flextable(col_keys    = c("Level",
                            "Predictor", 
                            "Collective\nEfficacy", 
                            "Abandoned", 
                            "Bars", 
                            "Commercial\nDestination", 
                            "Liquor\nStores", 
                            "Mixed\nLand\nUse", 
                            "Parking", 
                            "Recreation", 
                            "Vacant")) %>%
  compose(j = "Collective\nEfficacy",    value = as_paragraph(CE_hlm_2001_Estimate, "\n", "(", CE_hlm_2001_Std.Error, ")")) %>%
  compose(j = "Abandoned",               i = ~ !is.na(BE_pr_abandoned_bld_onstreet_block_2001_Estimate),              value = as_paragraph(BE_pr_abandoned_bld_onstreet_block_2001_Estimate, "\n", "(", BE_pr_abandoned_bld_onstreet_block_2001_Std.Error, ")")) %>%
  compose(j = "Bars",                    i = ~ !is.na(BE_pr_bar_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_bar_onstreet_block_2001_Estimate,           "\n", "(", BE_pr_bar_onstreet_block_2001_Std.Error,           ")")) %>%
  compose(j = "Commercial\nDestination", i = ~ !is.na(BE_pr_commer_dest_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_commer_dest_onstreet_block_2001_Estimate,   "\n", "(", BE_pr_commer_dest_onstreet_block_2001_Std.Error,   ")")) %>%
  compose(j = "Liquor\nStores",          i = ~ !is.na(BE_pr_liquor_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_liquor_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_liquor_onstreet_block_2001_Std.Error,        ")")) %>%
  compose(j = "Mixed\nLand\nUse",        i = ~ !is.na(MIXED_LAND_USE_2001_Estimate), value = as_paragraph(MIXED_LAND_USE_2001_Estimate,                     "\n", "(", MIXED_LAND_USE_2001_Std.Error,                     ")")) %>%
  compose(j = "Parking",                 i = ~ !is.na(BE_pr_parking_block_2001_Estimate), value = as_paragraph(BE_pr_parking_block_2001_Estimate,                "\n", "(", BE_pr_parking_block_2001_Std.Error,                ")")) %>%
  compose(j = "Recreation",              i = ~ !is.na(BE_pr_recreation_block_2001_Estimate), value = as_paragraph(BE_pr_recreation_block_2001_Estimate,             "\n", "(", BE_pr_recreation_block_2001_Std.Error,             ")")) %>%
  compose(j = "Vacant",                  i = ~ !is.na(BE_pr_vacant_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_vacant_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_vacant_onstreet_block_2001_Std.Error,        ")")) %>%
  compose(j = "Abandoned",               i = ~ is.na(BE_pr_abandoned_bld_onstreet_block_2001_Estimate), value = as_paragraph("---")) %>%
  compose(j = "Bars",                    i = ~ is.na(BE_pr_bar_onstreet_block_2001_Estimate),           value = as_paragraph("---")) %>%
  compose(j = "Commercial\nDestination", i = ~ is.na(BE_pr_commer_dest_onstreet_block_2001_Estimate),   value = as_paragraph("---")) %>%
  compose(j = "Liquor\nStores",          i = ~ is.na(BE_pr_liquor_onstreet_block_2001_Estimate),        value = as_paragraph("---")) %>%
  compose(j = "Mixed\nLand\nUse",        i = ~ is.na(MIXED_LAND_USE_2001_Estimate),                     value = as_paragraph("---")) %>%
  compose(j = "Parking",                 i = ~ is.na(BE_pr_parking_block_2001_Estimate),                value = as_paragraph("---")) %>%
  compose(j = "Recreation",              i = ~ is.na(BE_pr_recreation_block_2001_Estimate),             value = as_paragraph("---")) %>%
  compose(j = "Vacant",                  i = ~ is.na(BE_pr_vacant_onstreet_block_2001_Estimate),        value = as_paragraph("---")) %>%
  set_header_labels(Level                     = "",
                    Predictor                 = "Predictor", 
                    `Collective\nEfficacy`    = "Collec.\nEffic.",
                    Abandoned                 = "Aband-\noned", 
                    Bars                      = "Bars", 
                    `Commercial\nDestination` = "Commer.\nDest.", 
                    `Liquor\nStores`          = "Liquor\nStores", 
                    `Mixed\nLand\nUse`        = "Mixed\nLand\nUse", 
                    Parking                   = "Parking", 
                    Recreation                = "Recre-\nation", 
                    Vacant                    = "Vacant"  ) %>%
  merge_h_range(i = c(1,7), j1 = 1, j2 = 11) %>%
  bold(i = ~ CE_hlm_2001_p_sig == TRUE, j = "Collective\nEfficacy") %>%
  bold(i = ~ BE_pr_abandoned_bld_onstreet_block_2001_p_sig == TRUE, j = "Abandoned") %>%
  bold(i = ~ BE_pr_bar_onstreet_block_2001_p_sig == TRUE, j = "Bars") %>%
  bold(i = ~ BE_pr_commer_dest_onstreet_block_2001_p_sig == TRUE, j = "Commercial\nDestination") %>%
  bold(i = ~ BE_pr_liquor_onstreet_block_2001_p_sig == TRUE, j = "Liquor\nStores") %>%
  bold(i = ~ MIXED_LAND_USE_2001_p_sig == TRUE, j = "Mixed\nLand\nUse") %>%
  bold(i = ~ BE_pr_parking_block_2001_p_sig == TRUE, j = "Parking") %>%
  bold(i = ~ BE_pr_recreation_block_2001_p_sig == TRUE, j = "Recreation") %>%
  bold(i = ~ BE_pr_vacant_onstreet_block_2001_p_sig == TRUE, j = "Vacant") %>%
  compose(j = 2, i = 17, value = as_paragraph("Density (Block)", as_sup("2"))) %>%
  align(i = c(2:6, 8:17), j = 3:11, align = "center", part = "body") %>%
  align(j = 3:11, align = "center", part = "header") %>%
  italic(i = c(1,7)) %>%
  add_footer_row(values = r2vals_both , colwidths = rep(1, length(r2vals_both))) %>%
  align(j = 3:11, align = "center", part = "footer") %>%
  # font(fontname = "Latin Modern Roman", part = "all") %>%
  set_table_properties(layout = "fixed")  %>%
  border_remove() %>%
  width(j = 1, width = 0.2) %>%
  width(j = 2, width = 0.7) %>%
  width(j = 3:11, width = 0.55) %>%
  height(height = 0.35, part = "body") %>%
  merge_h_range(i=1, j1 = 1, j2 = 2, part = "footer") %>%
  # hline_bottom(border = officer::fp_border(width = 0.5)) %>%
  add_footer_row(top=FALSE, values = "Standard errors in parentheses; Bolded estimates significant at 95% level", colwidths = 11) %>%
  fontsize(size = 9, part = "all") %>%
  hline_bottom(border = officer::fp_border(width = 1), part = "body") %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))
s1_subset_table
