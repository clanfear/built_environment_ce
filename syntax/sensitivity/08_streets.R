library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/ccahs_block_sso_crime.RData")
load("./data/derived/crosswalks/complete_crosswalk.RData")
load("./data/derived/blocks_1990_interpolated_sum.RData")

streets <- read_sf("./data/raw/street_center_lines/geo_export_41863fb2-af38-4010-8784-f90261f0dde0.shp")


street_block <- blocks_1990_interpolated_sum %>% 
  select(-population) %>%
  st_join(streets %>%
    st_transform(st_crs(blocks_1990_interpolated_sum)) %>%
    select(geometry, class)) %>%
  st_drop_geometry() %>%
  # Extremely small number of blocks by freeway
  filter(!is.na(class) & class != 1) %>% 
  mutate(
    num_class = as.numeric(case_when(
    class %in% as.character(c(1:4)) ~ class,
    TRUE ~  "5"))) %>% # Treating the small / unusual categories as same as the low flow collectors; makes no difference to shift around.
  group_by(TRACT, BLOCK) %>%
  summarize(min_class = min(num_class)) %>%
  ungroup() %>%
  mutate() %>%
  rename(census_tract_6 = TRACT, census_block = BLOCK)
  
save(street_block, file = "./data/derived/sensitivity/street_block.RData")
