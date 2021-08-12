library(tidyverse)
source("./syntax/file_path_index.R")

# Note CCAHS has a .do file to change missings (e.g. -2, -5) to .
ccahs_sso_raw <- haven::read_dta(ccahs_sso_path)

ccahs_sso <- ccahs_sso_raw %>%
  select(CMBID,
         FACE_ID,
         STREET_ID                          = STRT_ID,
         BLOCK_ID,
         BLOCKGROUP_ID                      = GROUP_ID,
         TRACT_ID,
         NC_ID,
         LAND_USE_7_CAT                     = S4110,
         BE_pr_residential_block            = PFB1001, # pr face in block with residential use
         BE_pr_commercial_block             = PFB1002, # pr face in block with commercial/professional use
         BE_pr_industrial_block             = PFB1003, # pr face in block with industrial/warehouse use
         BE_pr_parking_block                = PFB1004, # pr face in block with parking lots
         BE_pr_vacant_lot_block             = PFB1005, # pr face in block with vacant lots or open space
         BE_pr_institution_block            = PFB1006, # pr face in block with institutional land use
         BE_pr_recreation_block             = PFB1007, # pr face in block with recreational land use
         BE_pr_abandoned_bld_onstreet_block = PSB0617, # pr of streets in block with abandoned/burned out/boarded up building
         BE_pr_vacant_onstreet_block        = PSB0618, # pr of streets in block with vacant but usable house/building
         BE_pr_commer_dest_onstreet_block   = PSB4031, # pr of streets in block with commercial destination
         BE_pr_liquor_onstreet_block        = PSB0615, # pr of streets in block with liquor store
         BE_pr_bar_onstreet_block           = PSB0614) # pr of streets in block with bar/cocktail lounge

ccahs_sso_block <- ccahs_sso %>%
  mutate(NC_ID = as.character(NC_ID),
         BLOCK_ID = as.character(BLOCK_ID),
         TRACT_ID = as.character(TRACT_ID)) %>%
  select(NC_ID, BLOCK_ID, TRACT_ID, matches("_block|_face"), LAND_USE_7_CAT) %>%
  mutate(MIXED_LAND_USE = ifelse(LAND_USE_7_CAT %in% c(2,4), 1, 0)) %>%
  select(-LAND_USE_7_CAT) %>%
  group_by(NC_ID, TRACT_ID, BLOCK_ID) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE))) %>%
  ungroup()

ccahs_sso_block %>% select(where(is.numeric)) %>% cor()

save(ccahs_sso_block, file = "./data/derived/ccahs_sso_block.RData")
