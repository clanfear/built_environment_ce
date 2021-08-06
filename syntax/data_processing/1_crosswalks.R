# This file uses the CCAHS and tract to NC crosswalk to generate a "global"
# crosswalk across the PHDCN, CCAHS, and US census based on year 2000 tracts.
# Due to the absence of identifiers, it cannot link PHDCN respondents or SSO data
# to tracts or blocks.

library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

phdcn_nc_to_ccahs_tract <- haven::read_sav("F:/SecureData/Matsueda-tract_linknc.sav") %>% 
  mutate(across(everything(), ~as.character(.))) %>%
  rename(phdcn_nc = link_nc, 
         ccahs_tract = tract)

ccahs_dir    <- "F:/SecureData/CCAHS/"
ccahs_nc_to_ccahs_tract <- haven::read_dta(paste0(ccahs_dir, "DS0001/31142-0001-Data-REST.dta")) %>%
  select(ccahs_nc          = V5,
         ccahs_tract       = V6) %>%
  mutate(ccahs_tract = round(ccahs_tract, 0)) %>% # Not sure where 5306.0600... came from
  mutate(across(everything(), ~as.character(.))) %>%
  distinct(ccahs_nc, ccahs_tract)
         

ccahs_tract_to_nc <- 
  full_join(phdcn_nc_to_ccahs_tract, 
            ccahs_nc_to_ccahs_tract) %>% 
  group_by(phdcn_nc) %>% 
  arrange(ccahs_nc) %>%
  fill(ccahs_nc) %>% 
  ungroup() %>%
  group_by(ccahs_nc) %>%
  fill(phdcn_nc) %>%
  ungroup()

nc_crosswalk <-  ccahs_tract_to_nc %>% distinct(ccahs_nc, phdcn_nc)
save(nc_crosswalk, file = "./data/chicago/derived/crosswalks/nc_crosswalk.RData")

#-----
# Match ccahs tracts to real tracts
il_tract_raw <- tigris::tracts("IL", cb=TRUE, year=1990)
il_tract     <- il_tract_raw %>%
  mutate(TRACT = paste0(TRACTBASE, TRACTSUF),
         FIPSSTCO = paste0(STATEFP, COUNTYFP)) %>%
  filter(FIPSSTCO == "17031") %>%
  select(TRACT, TRACTBASE, TRACTSUF, geometry)

save(il_tract, file = "./data/chicago/derived/il_tract.RData")

tract_to_nc <- ccahs_tract_to_nc %>% 
  mutate(census_tract_4 = str_pad(ccahs_tract, 4, "left", 0)) %>%
  left_join(il_tract, by = c("census_tract_4"="TRACTBASE")) %>%
  select(-geometry, -TRACTSUF) %>% 
  rename(census_tract_6 = TRACT)
  
save(tract_to_nc, file = "./data/chicago/derived/crosswalks/tract_to_nc.RData")

#----
# Add blocks

il_block_raw <- sf::read_sf("./data/chicago/raw/blocks_1990/IL_block_1990.shp") %>%
  st_transform(3435)

# Fixing 8104-813, which is one block in CCAHS but two separate blocks in census

il_block_810400813 <- il_block_raw %>% filter(TRACT == "810400" & BLOCK %in% c("813A", "813B")) %>%
  summarize(across(c(FIPSSTCO, TRACT, STFID, GISJOIN), ~ first(.)), BLOCK = "813", geometry = st_union(geometry))
il_block <- 
  il_block_raw %>% 
  filter(!(TRACT == "810400" & BLOCK %in% c("813A", "813B"))) %>% 
  rbind(il_block_810400813)

save(il_block, file = "./data/chicago/derived/il_block.RData")

# SSO IDs

ccahs_sso_ids <- haven::read_dta(paste0(ccahs_dir, "DS0002/31142-0002-Data-REST.dta")) %>% 
  select(NC_ID, TRACT_ID, BLOCKGROUP_ID = GROUP_ID, BLOCK_ID) %>%
  transmute(ccahs_nc = as.character(NC_ID),
            census_block = str_sub(BLOCK_ID, 1, 3),
            census_blockgroup = str_sub(BLOCKGROUP_ID, -1,-1),
            census_tract_4 = str_pad(TRACT_ID, side= "left", pad = "0", width = 4),
            ccahs_tract = as.character(TRACT_ID),
            ccahs_sso_block = as.character(BLOCK_ID)) %>% 
  distinct()

save(ccahs_sso_ids, file = "./data/chicago/derived/ccahs_sso_ids.RData")

# Merge everything

complete_crosswalk <- il_block %>%
  st_drop_geometry() %>% 
  filter(FIPSSTCO == "17031") %>% 
  select(census_tract_6 = TRACT, census_block = BLOCK) %>% 
  right_join(tract_to_nc) %>%
  filter(!(is.na(ccahs_nc) & is.na(phdcn_nc))) %>% 
  mutate(census_blockgroup = str_sub(census_block, 1, 1)) %>%
  mutate(census_block = ifelse(census_tract_6 == "810400" & census_block %in% c("813A","813B"), "813", census_block)) %>%
  left_join(ccahs_sso_ids) %>%
  distinct()

save(complete_crosswalk, file = "./data/chicago/derived/crosswalks/complete_crosswalk.RData")
 