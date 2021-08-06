# Incomplete script for getting quick and dirty map of sampled blocks in CCAHS
library(tidyverse)
library(sf)
library(ragg)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")

load("./data/chicago/derived/nc_boundaries.RData")
load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")
load("./data/chicago/derived/blocks_1990_interpolated_sum.RData")

all_blocks <- complete_crosswalk %>% 
  inner_join(blocks_1990_interpolated_sum %>% 
               select(census_tract_6   = TRACT, 
                      census_block     = BLOCK, 
                      population_block = population, 
                      geometry)) %>% st_as_sf()



selected_blocks <- all_blocks %>%
  filter(!is.na(ccahs_sso_block)) %>%
  group_by(census_tract_6, census_block) %>%
  slice(1L) %>%
  ungroup() %>%
  st_as_sf()

all_blocks_union <- all_blocks %>% st_union()
st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}
cook_water <- tigris::area_water("IL", "Cook", class = "sf") %>% st_transform(st_crs(all_blocks))
all_blocks_nowater <- all_blocks_union %>% 
  st_erase(cook_water)

boundary_map <- ggplot() + 
  geom_sf(data = all_blocks_nowater) + 
  geom_sf(data = selected_blocks, color = NA, fill = "black") + 
  geom_sf(data = nc_boundaries, color = "black", fill = NA, size = 0.1) +
  theme_void()

ggsave("../built_environment/docs/img/boundary_map.png",  boundary_map, width = 6, units = "in", device = ragg_png)