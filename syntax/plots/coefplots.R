library(tidyverse)
library(piecewiseSEM)

load("./output/psem_hlm_list_summary.RData")
load("./output/psem_hlm_int_list_summary.RData")

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
process_response_name <- function(x){
  . <- str_remove_all(x, "(CRIME_|_2004_2006)")
  . <- str_replace(., "homicide_assault_battery_gun", "Homicide /\nGun Assault")
  . <- str_to_title(.)
}

# SECOND STAGE 
second_stage_plot_data <- psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = fct_relevel(process_response_name(Response), 
                               "Homicide /\nGun Assault", "Robbery", "Violent", "Property"
                               ),
         Predictor           = 
           fct_rev(
           fct_relevel(
             fct_recode(Predictor,
                        "Collective\nEfficacy\n(2003)"      = "CE_hlm_2001",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned\nBuildings"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commercial\nDestinations"      = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor\nStores"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed\nLand Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant\nLots"                = "BE_pr_vacant_onstreet_block_2001",
                        "Street Class"          = "street_class_near",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Collective\nEfficacy\n(2003)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Abandoned\nBuildings"            ,
             "Bars"                 ,
             "Commercial\nDestinations"     ,
             "Liquor\nStores"               ,
             "Mixed\nLand Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant\nLots"               ,
             "Street Class",
             "Density (Block)"      ,
             "Density (Block)^2"    ))) %>%
  filter(!(Predictor %in% c("Disadv.", "Stability", "Hispanic /\nImmigrant", "Density (Neighb.)", "Street Class", "Density (Block)", "Density (Block)^2"))) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  mutate(lb = Estimate - (Std.Error*1.96),
         ub = Estimate + (Std.Error*1.96)) %>%
  mutate(across(c(Estimate, lb, ub), ~exp(.)))

# Plotting
be_coefplot <- function(x){
  ggplot(x, aes(x = Estimate, y = Predictor, group = Level, color = p_sig)) + 
    facet_wrap(~Response, ncol = 4) + 
    geom_vline(xintercept=1, linetype = "dashed", size = 0.25) + 
    geom_point(size = 0.75) + 
    geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.15, size = 0.3) + 
    theme_minimal() +
    xlab("Incidence Rate Ratio (95% CI)") + 
    ylab("") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey50"), guide = "none") +
    scale_x_continuous(breaks = seq(0.8, 1.2, by = 0.2),
                       labels = c(".8", "1.0", "1.2"),
                       limits = c(0.8, 1.4)) +
    theme(axis.text.y = element_text(hjust=0.5, color = "black"), 
          strip.text.x = element_text(hjust = 0),
          panel.spacing.x = unit(.4, "in"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = "serif", color = "black"))
}

ggsave("./docs/figure/coefplot_stage_2.png",  be_coefplot(second_stage_plot_data), width = 5, height = 5, units = "in", device = ragg_png, bg = "white")

