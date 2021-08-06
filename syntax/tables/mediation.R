library(tidyverse)
library(piecewiseSEM)

load("./data/chicago/derived/psem_hlm_list_summary.RData")

be_crime_effects <- psem_hlm_list_summary$coefficients[,-9] %>%
  filter(str_detect(Response, "CRIME") & str_detect(Predictor, "^BE_|^MIXED")) %>%
  select(Response, BE = Predictor, stage2estimate = Estimate)

ce_be_effects <- psem_hlm_list_summary$coefficients[,-9] %>%
  filter(str_detect(Response, "^BE_|^MIXED") & str_detect(Predictor, "^CE")) %>%
  select(BE = Response, stage1estimate = Estimate)

be_crime_effects %>% left_join(ce_be_effects) %>%
  mutate(mediated_effect = stage1estimate * stage2estimate) %>%
  group_by(Response) %>%
  summarize(mediated_effect = exp(sum(mediated_effect))-1)

be_crime_effects %>% left_join(ce_be_effects) %>%
  mutate(mediated_effect = stage1estimate * stage2estimate) %>%
  filter(BE == "BE_pr_abandoned_bld_onstreet_block_2001" | BE == "MIXED_LAND_USE_2001") %>%
  group_by(Response) %>%
  summarize(mediated_effect = exp(sum(mediated_effect))-1)

psem_hlm_list_summary$coefficient[psem_hlm_list_summary$coefficients$Response == "CRIME_property_2004_2006",]
  