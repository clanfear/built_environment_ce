# This file pulls the pre-calculated NC-level measures from the CCAHS. I 
# don't actually really use ANY of these. But I do check how they correlate with
# my own measures to be sure I'm not mucking anything up.

library(tidyverse)
source("./syntax/project_functions.R")
ccahs_dir    <- "F:/SecureData/CCAHS/"

# Note CCAHS has a .do file to change missings (e.g. -2, -5) to .
ccahs_individual_raw <- haven::read_dta(paste0(ccahs_dir, "DS0001/31142-0001-Data-REST.dta"))

# PULL NC-LEVEL PRECALCULATED MEASURES

ccahs_nc <- ccahs_individual_raw %>%
  select(NC_ID          = V5,
         PERC_DIS_2001  = V2826,
         PERC_VIOL_2001 = V2828,
         VICT_EVER_2001 = V2834,
         CE_2001        = V2836,
         DISADV_2000    = V3805,
         AFFLUENCE_2000 = V3806,
         HISP_FOR_2000  = V3807,
         DENSITY_2000   = V3812,
         POP_2000       = V3811) %>%
  mutate(NC_ID = as.character(NC_ID)) %>%
  group_by(NC_ID) %>%
  summarize_all(~mean(.))

save(ccahs_nc, file = "./data/chicago/derived/ccahs_nc.RData")