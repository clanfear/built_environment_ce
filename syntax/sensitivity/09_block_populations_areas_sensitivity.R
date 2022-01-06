# This file calculates block level populations, areas, and densities. So the 
# CCAHS uses sampling from the PHDCN which is 1990 boundaries. But it happened
# after 2000. So I try to make sure things match up. Unlike tract boundaries,
# blocks change all the place across years so this is more important.

library(tidyverse)
library(sf)
library(tidycensus)
library(areal)
source("./syntax/project_functions.R")
load("./data/derived/il_block.RData")
load("./data/derived/crosswalks/complete_crosswalk.RData")
streets <- read_sf("./data/raw/street_center_lines/geo_export_41863fb2-af38-4010-8784-f90261f0dde0.shp")



block_shape_pop <- tigris::blocks("IL", "Cook", year = 2000, class = "sf", refresh=TRUE) %>% 
  mutate(tract_block = str_sub(BLKIDFP00, -10, -1)) %>% 
  inner_join(block_pops) %>% 
  st_transform(3435) %>%
  select(tract_block, population = P001001, geometry)

blocks_1990 <- il_block %>% 
  filter(FIPSSTCO == "17031") %>% 
  select(TRACT, BLOCK, geometry) %>%
  mutate(tract_block = paste0(TRACT, BLOCK)) %>%
  st_make_valid()

#  census_vars_sf1 <- tidycensus::load_variables(2000, "sf1")
#  census_vars_sf3 <- tidycensus::load_variables(2000, "sf3")

# perc_black        = P003004/P003001,
# perc_hisp         = P004002/P004001,
# perc_under18      = (P012003 + P012004 + P012005 + P012006 + P012027 + P012028 + P012029 + P012030)/P012001),
# perc_owned        = H004002/H004001,
# perc_fhh          = P034015/P034001,
# perc_foreign      = P021013/P021001,
# 
# perc_edhighschool = (P148A005 + P148A013)/P148A001,
# perc_edcollege    = (P148A008 + P148A016)/P148A001,
# perc_unemployed   = (P043007 + P043014)/P043001,
# perc_professional = (P050050 + P050003)/P050001,
# perc_poverty      = P087002/P087001,
# perc_moved        = (H038003 + H038004 + H038005 + H038010 + H038011 + H038012)/H038001
# 
# vars_sf1 <- c("P003004", "P003001",
#           "P004002", "P004001",
#           "P012003", "P012004", "P012005", "P012006", "P012027", "P012028", "P012029", "P012030", "P012001",
#           "H004002", "H004001",
#           "P034015", "P034001",
#           "P021013", "P021001")
# 
# vars_sf3 <- c(
#           "P021013", "P021001",
#           "P148A005", "P148A013", "P148A001",
#           "P148A008", "P148A016", "P148A001",
#           "P043007", "P043014", "P043001",
#           "P050050", "P050003", "P050001",
#           "P087002", "P087001",
#           "H038003", "H038004", "H038005", "H038010",  "H038011",  "H038012", "H038001")
# 
# block_shape_pop <- get_decennial("block", state = "IL", county = "Cook",
#               year = 2000,
#               variables = c(c("P001001"), vars_sf1),
#               output = "wide", geometry=TRUE) %>% 
#   mutate(tract_block = str_sub(GEOID, -10, -1)) %>%
#   rename(population = P001001) %>% 
#   st_transform(3435)
# 
# block_sf3 <- get_decennial("block", state = "IL", county = "Cook",
#                                  year = 2000,
#                                  variables = vars_sf3[c(1:2, 17:23)],
#                                  output = "wide", geometry=FALSE) %>% 
#   mutate(tract_block = str_sub(GEOID, -10, -1))

blocks_1990_interpolated_sum <- aw_interpolate(blocks_1990, 
                                               tid = tract_block, 
                                               source = block_shape_pop, 
                                               sid = tract_block, 
                                               extensive = "population", 
                                               weight = "sum", output = "sf") %>% 
  mutate(area    = as.numeric(units::set_units(st_area(geometry), "km^2")),
         density_block = as.numeric(population / area)) %>%
  select(census_tract_6   = TRACT, 
         census_block     = BLOCK, 
         area_block       = area,
         population_block = population) %>%
  inner_join(complete_crosswalk %>%
               filter(!is.na(ccahs_sso_block))) %>% 
  group_by(phdcn_nc, ccahs_nc, census_tract_6, census_block, ccahs_tract) %>%
  summarize(geometry         = st_union(st_combine(geometry)),
            area_block       = sum(area_block),
            population_block = sum(population_block)) %>%
  mutate(density_block = population_block / area_block)

save(blocks_1990_interpolated_sum, file = "./data/derived/blocks_1990_interpolated_sum.RData")
  
# block_intersect <- blocks_1990_interpolated_sum %>% 
#   st_join(
#     streets %>%
#       st_transform(st_crs(blocks_1990_interpolated_sum)) %>%
#       select(geometry, class) %>%
#       mutate(class = as.numeric(str_remove_all(class, "[^0-9]"))) %>%
#       # Treating the small / unusual categories as same as the low flow collectors; makes no difference to shift around.
#       mutate(num_class = 
#                case_when(
#                  is.na(class) ~ 5,
#                  class %in% 2:4 ~ class,
#                  TRUE ~  5)))

block_within <- blocks_1990_interpolated_sum %>% 
  st_join(
    streets %>%
      st_transform(st_crs(blocks_1990_interpolated_sum)) %>%
      select(geometry, class) %>%
      mutate(class = as.numeric(str_remove_all(class, "[^0-9]"))) %>%
      # Treating the small / unusual categories as same as the low flow collectors; makes no difference to shift around.
      mutate(num_class = 
               case_when(
                 is.na(class) ~ 5,
                 class %in% 2:4 ~ class,
                 TRUE ~  5)),
    join = st_is_within_distance,
    dist = units::set_units(30, "ft"))

block_pop_estimates_2000 <-  blocks_1990_interpolated_sum  %>% 
  st_drop_geometry() %>%
  inner_join(block_within %>%
                 st_drop_geometry() %>% 
                   group_by(phdcn_nc, ccahs_nc, census_tract_6, census_block, ccahs_tract) %>%
                   # Take the minimum (most busy) street type
                   summarize(street_class_near = min(num_class))%>%
                   ungroup()) %>%
  mutate(street_class_near = -ifelse(street_class_near == 5, 4, street_class_near)+5) %>%
  ungroup()

save(block_pop_estimates_2000, file = "./data/derived/block_pop_estimates_2000.RData")
