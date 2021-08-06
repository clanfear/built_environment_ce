library(tidyverse)
library(flextable)
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical_unstd.RData")

# Quick check to see quartiles of CE and abandoned; didn't rename from tertiles because lazy
ccahs_block_analytical_unstd %>%
  mutate(abandoned_tertile = ntile(BE_pr_abandoned_bld_onstreet_block_2001, 4),
         CE_tertile = ntile(CE_hlm_2001, 4)) %>%
  count(CE_tertile, abandoned_tertile) %>%
  pivot_wider(values_from = n, names_from = abandoned_tertile, names_prefix = "Abandoned ")

# Descriptives table with density plots in-line. Needs postprocessing for document.
be_descriptives_dat <- ccahs_block_analytical_unstd %>%
  select(-census_block, -ccahs_tract, -density_block_2, -FAC_disadv_2000_2, -density_block) %>%
  mutate(across(c(orig_density_block, density_ltdb_nc_2000), ~ . / 1000)) %>%
  pivot_longer(-ccahs_nc) %>%
  mutate(level = factor(ifelse(str_detect(name, "^(BE|CRIME|MIX|orig)"), "Block (N=1,641)", "Neighborhood (N=343)"), levels = c("Neighborhood (N=343)", "Block (N=1,641)"))) %>%
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
                        "Homicide"                   = "CRIME_homicide_2004_2006",
                        "Gun Assault"                = "CRIME_assault_battery_gun_2004_2006",
                        "Property"                   = "CRIME_property_2004_2006",
                        "Robbery"                    = "CRIME_robbery_2004_2006",
                        "Violent"                    = "CRIME_violent_2004_2006",
                        "Collective Efficacy (2001)" = "CE_hlm_2001",
                        "Collective Efficacy (1995)" = "CE_hlm_1995",
                        "Disadvantage"               = "FAC_disadv_2000",
                        "Stability"                  = "FAC_stability_2000",
                        "Hispanic/Immigrant"         = "FAC_hispimm_2000",
                        "Density (Neighborhood)"          = "density_ltdb_nc_2000",
                        "Abandoned"                  = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                       = "BE_pr_bar_onstreet_block_2001",
                        "Commercial Dest."           = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor"                     = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"                  = "MIXED_LAND_USE_2001",
                        "Parking"                    = "BE_pr_parking_block_2001",
                        "Recreation"                 = "BE_pr_recreation_block_2001",
                        "Vacant"                     = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"            = "orig_density_block"),
             "Homicide"   ,
             "Gun Assault",
             "Robbery"    ,
             "Violent"    ,
             "Property"   ,
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
             "Mixed Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant"               ,
             "Density (Block)"      )) %>%
  arrange(level, name) %>%
  rename(Measure = name) %>%
  relocate(Min, df, Max, .after = last_col())

save(be_descriptives_dat, file = "./output/chicago/tables/be_descriptives_dat.RData")

be_descriptives <- be_descriptives_dat %>%
  as_grouped_data(groups = "level") %>%
  flextable() %>%
  mk_par(j = "df", i = ~ !is.na(Max), value = as_paragraph(
    plot_chunk(value = df, type = "density", col = "black", 
               width = 0.8, height = .2, free_scale = TRUE)
  )) %>%
  set_header_labels(df = "Density", level = "") %>%
  merge_h_range(i = c(1,8), j1 = 1, j2 = 7) %>%
  font(fontname = "Latin Modern Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  set_table_properties(width = 0.45, layout = "autofit") %>%
  border_remove() %>%
  hline_bottom(border = officer::fp_border(width = 1)) %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))

save_as_image(be_descriptives, "./output/chicago/tables/descriptives.png")

be_descriptives %>%
  font(fontname = "Times New Roman", part = "all") %>%
  save_as_docx(path = "./output/chicago/tables/descriptives.docx")
  
