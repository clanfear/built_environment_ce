library(tidyverse)
library(piecewiseSEM)

load("./output/psem_hlm_list_summary.RData")
load("./output/psem_hlm_int_list_summary.RData")

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
process_response_name <- function(x){
  . <- str_remove_all(x, "(CRIME_|_2004_2006)")
  . <- str_replace(., "assault_battery_gun", "gun assault")
  . <- str_to_title(.)
  . <- str_replace(., " ", "\n")
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
                               "Homicide", "Gun\nAssault", "Robbery", "Violent", "Property"
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
                        "Commercial\nDestination"      = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor\nStores"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed\nLand Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant\nLots"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Collective\nEfficacy\n(2003)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Abandoned\nBuildings"            ,
             "Bars"                 ,
             "Commercial\nDestination"     ,
             "Liquor\nStores"               ,
             "Mixed\nLand Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant\nLots"               ,
             "Density (Block)"      ,
             "Density (Block)^2"    ))) %>%
  filter(!(Predictor %in% c("Disadv.", "Stability", "Hispanic /\nImmigrant", "Density (Neighb.)", "Density (Block)", "Density (Block)^2"))) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  mutate(lb = Estimate - (Std.Error*1.96),
         ub = Estimate + (Std.Error*1.96)) %>%
  mutate(across(c(Estimate, lb, ub), ~exp(.)))

# Plotting
be_coefplot <- function(x){
  ggplot(x, aes(x = Estimate, y = Predictor, group = Level, color = p_sig)) + 
  facet_wrap(~Response, ncol = 5) + 
  geom_vline(xintercept=1, linetype = "dashed") + 
  geom_point() + 
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.1, size = 0.3) + 
  theme_minimal() +
  xlab("Incidence Rate Ratio (95% CI)") + 
  ylab("") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey50"), guide = "none") +
  scale_x_continuous(breaks = seq(0.6, 1.4, by = 0.4)) +
  theme(axis.text.y = element_text(hjust=0.5, color = "black"), 
        panel.spacing.x = unit(.25, "in"),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Times New Roman", color = "black"))
}

ggsave("./docs/figure/coefplot_stage_2.png",  be_coefplot(second_stage_plot_data), width = 6, height = 6, units = "in", device = ragg_png)
