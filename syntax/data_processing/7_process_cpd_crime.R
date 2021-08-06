# This file takes a large file of publicly available Chicago Police Department
# incident data, spatially joins it to year 2000 Illinois tracts, and then 
# joins it to the crosswalk file so it can be used with any geographical unit.
# Here I focus on homicide, robbery, agg. assault, agg. battery, burglary, and 
# larceny.

library(tidyverse)
library(sf)
library(janitor)
source("./syntax/project_functions.R")
# This file is ~1.6 gigs; if you're replicating, you'll have to change directory
cpd_crimes_raw <- vroom::vroom("D:/Projects/dissertation_data/chicago/chicago_police_data/Crimes_-_2001_to_Present.csv")

load("./data/chicago/derived/il_block.RData")
load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")

# Map these to blocks and tracts, then to NCs

nibrs_codes <- c("homicide"     = "01A",
                 "robbery"      = "03",
                 "agg. assault" = "04A",
                 "agg. battery" = "04B",
                 "burglary"     = "05",
                 "larceny"      = "06")

cpd_crimes <- cpd_crimes_raw %>%
  clean_names() %>%
  select(-case_number, -iucr, -block, -beat, -district, -ward, -community_area, -year, -location, -updated_on) %>%
  filter(!is.na(latitude) & domestic == FALSE) %>%
  filter(fbi_code %in% nibrs_codes) %>%
  mutate(crime_type = invert(nibrs_codes)[fbi_code]) %>%
  mutate(crime_type = ifelse(crime_type %in% c("agg. assault", "agg. battery"), "assault_battery", crime_type)) %>%
  mutate(gun_crime = ifelse(str_detect(description, "GUN"), 1, 0)) %>%
  select(-domestic, -fbi_code) %>%
  mutate(date = lubridate::mdy_hms(date)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(il_block)) %>%
  st_join(il_block) %>%
  filter(FIPSSTCO == 17031) %>%
  select(-FIPSSTCO, -STFID, -GISJOIN) %>%
  st_drop_geometry()

# blocks
valid_block_years <- complete_crosswalk %>% 
  select(census_tract_6, census_block) %>%
  tidyr::expand(census_tract_6, census_block, year = 2001:2012) 

cpd_block_year <- cpd_crimes %>%
  mutate(year = lubridate::year(date)) %>%
  count(TRACT, BLOCK, year, crime_type) %>% 
  complete(TRACT, BLOCK, year, crime_type, fill = list(n = 0)) %>%
  rename(census_tract_6 = TRACT, census_block = BLOCK) %>%
  pivot_wider(names_from = crime_type, values_from=n)

cpd_crimes_block_all <- valid_block_years %>%
  left_join(cpd_block_year) %>%
  mutate(violent = assault_battery + robbery + homicide,
         property = burglary + larceny,
         all_crime = violent + property) 

cpd_block_year_guns <- cpd_crimes %>%
  filter(gun_crime == 1) %>%
  mutate(year = lubridate::year(date)) %>%
  count(TRACT, BLOCK, year, crime_type) %>% 
  complete(TRACT, BLOCK, year, crime_type, fill = list(n = 0)) %>%
  rename(census_tract_6 = TRACT, census_block = BLOCK) %>%
  pivot_wider(names_from = crime_type, values_from=n) %>%
  mutate(violent = assault_battery + robbery) %>%
  rename_with(~paste0(., "_gun"), -c(census_tract_6, census_block, year))

cpd_gun_crimes_block <- valid_block_years %>%
  left_join(cpd_block_year_guns) 

cpd_crimes_block <- cpd_crimes_block_all %>%
  left_join(cpd_gun_crimes_block) %>%
  mutate(across(c(matches("gun")), ~ ifelse(is.na(.) & !is.na(homicide), 0, .))) %>%
  rename_with(~paste0("CRIME_", .), -c(census_tract_6, census_block, year))

save(cpd_crimes_block, file = "./data/chicago/derived/cpd_crimes_block.RData")

# tracts

cpd_crimes_tract <- cpd_crimes_block %>%
  select(-census_block) %>%
  group_by(census_tract_6, year) %>%
  summarize(across(everything(), ~sum(., na.rm=TRUE)))

save(cpd_crimes_tract, file = "./data/chicago/derived/cpd_crimes_tract.RData")

# nc

cpd_crimes_nc <- cpd_crimes_tract %>%
  right_join(complete_crosswalk %>% group_by(census_tract_6) %>% slice(1L)) %>%
  group_by(ccahs_nc, year) %>%
  summarize(across(starts_with("CRIME"), ~sum(.))) %>%
  ungroup()

save(cpd_crimes_nc, file = "./data/chicago/derived/cpd_crimes_nc.RData")

cpd_crimes_nc_2003 <- cpd_crimes_nc %>% filter(year==2003)

save(cpd_crimes_nc_2003, file = "./data/chicago/derived/cpd_crimes_nc_2003.RData")
