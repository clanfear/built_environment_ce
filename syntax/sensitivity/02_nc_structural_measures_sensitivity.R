# This script uses Logan et al.'s Longitudinal Tract Database (LTDB) to get
# consistent structural measures across the 1990 and 2000 census on 2000 tract
# geometries. Then I do factor analysis to get the factors from Sampson et al. 
# 1997, except with one less measure not in the LTDB.
#
# Note converting the LTDB 1990 and 2000 files to 2000 census tracts requires 
# the original files and Stata scripts from here (as of 3/24/2021): 
# https://s4.ad.brown.edu/projects/diversity/researcher/bridging.htm
# THIS SCRIPT REQUIRES THESE CONVERTED FILES
# I have used the following naming scheme:
# std_1990_fullcount_cw2000.dta # 1990 Census full count vars crosswalked to year 2000 tracts
# std_1990_sample_cw2000.dta # 1990 Census sample vars crosswalked to year 2000 tracts
# std_2000_full_cw2000.dta # 2000 Census full count vars crosswalked to year 2000 tracts
# std_2000_sample_cw2000.dta # 2000 Census sample vars crosswalked to year 2000 tracts
#
# So an interesting fact: You can actually use EITHER year 2000 or 1990 tracts
# from tigris here and get the same result. Always 866 perfect matches, which is
# definitely correct. So, some tracts are created or destroyed in IL change but 
# none in Chicago. Which is weird. According to this:
# https://www.lib.uchicago.edu/e/collections/maps/censusinfo.html
# The census tracts in Chicago change numbering schemes but typically not shapes.
# Good to know I probably didn't need to be so fancy here.

library(tidyverse)
library(sf)
source("./syntax/project_functions.R")
source("./syntax/file_path_index.R")

load("./data/derived/crosswalks/complete_crosswalk.RData")
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

nc_tracts <- complete_crosswalk %>%
  distinct(census_tract_6, ccahs_nc, phdcn_nc)

il_tracts <-  tigris::tracts("IL", "Cook", year = 2000)

nc_sf <- il_tracts %>% 
  rename(census_tract_6 = TRACTCE00) %>%
  inner_join(complete_crosswalk %>% 
               distinct(ccahs_nc, phdcn_nc, census_tract_6)) %>%  
  mutate(area_tract = as.numeric(units::set_units(st_area(.), "km^2"))) # Requires package lwgeom

# Get a quick queen neighbors list for spatial tests if needed

nc_boundaries <- nc_sf %>%
  select(ccahs_nc, phdcn_nc, geometry) %>%
  st_transform(3435)  %>%
  group_by(ccahs_nc) %>%
  summarize(geometry = st_union(geometry))

save(nc_boundaries, file = "./data/derived/nc_boundaries.RData")

nc_neighbors <- nc_boundaries  %>%
  mutate(neighbors = st_queen(.)) %>%
  st_drop_geometry() %>%
  tidyr::unnest(neighbors)

save(nc_neighbors, file = "./data/derived/nc_neighbors.RData")

nc_areas <- nc_sf %>%
  st_drop_geometry() %>%
  group_by(ccahs_nc, phdcn_nc) %>%
  summarize(area_nc = sum(area_tract)) %>%
  ungroup()



ltdb_1990 <- inner_join(haven::read_stata(ltdb_1990_fullcount_path),
           haven::read_stata(ltdb_1990_sample_path)) %>%
  mutate(year = 1990) %>%
  rename_with(~str_remove(., "90|00"))

ltdb_2000 <- inner_join(haven::read_stata(ltdb_2000_fullcount_path),
           haven::read_stata(ltdb_2000_sample_path)) %>%
    mutate(year = 2000) %>%
  rename_with(~str_remove(., "00")) %>%
  select(-rent) # whoops, left rent in

# Check to make sure all vars present and identically named
x_notin_y(names(ltdb_1990), names(ltdb_2000))

ltdb_il <- bind_rows(ltdb_1990, ltdb_2000) %>%
  filter(str_sub(trtid, 1, 5)=="17031") %>%
  mutate(census_tract_6 = str_sub(trtid, 6, -1)) %>%
  inner_join(nc_tracts) %>%
  select(-trtid) %>%
  rename(
    d_POP              = pop,
    n_POP_black        = nhblk,
    n_POP_hispanic     = hisp,
    n_POP_under18      = a18und,
    d_HU_total         = hu,
    d_HU_occupied      = ohu,
    n_HU_owned         = own,
    n_FAM_fhh          = fhh,
    d_FAM              = family,
    d_population       = popsf3,
    n_foreign          = fb,
    d_ED               = ag25up,
    n_ED_highschool    = hs,
    n_ED_college       = col,
    d_EMP              = clf,
    n_EMP_unemployed   = unemp,
    n_EMP_professional = prof,
    d_poverty          = dpov,
    n_poverty          = npov,
    n_HU_moved10yrs    = h10yrs,
    d_HU_occupied_sp   = ohusp,
  ) %>%
  mutate(
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_POP,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population_ltdb_nc   = d_POP
  ) %>%
  select(-matches("^(d_|n_)"))

# ten Berge scores from alpha scoring oblimin factor analysis
  
ltdb_factors_tract <- ltdb_il %>% 
  select(-ccahs_nc, -phdcn_nc, -census_tract_6, -year, -perc_edcollege, -perc_edhighschool, -perc_professional) %>% 
  psych::fa(nfactors = 3,  scores="tenBerge", rotate="oblimin") %>%
  {.$scores} %>%
  cbind(select(ltdb_il, ccahs_nc, phdcn_nc, year, census_tract_6), .) %>%
  rename(FAC_disadv    = MR1,
         FAC_stability = MR2,
         FAC_hispimm   = MR3)

ltdb_factors_tract_wide <- ltdb_factors_tract %>%
  mutate(across(matches("^FAC_"), ~ standardize(., 2))) %>%
  pivot_longer(c(FAC_disadv, FAC_hispimm, FAC_stability)) %>%
  mutate(name = paste(name, "tract", year, sep = "_")) %>%
  select(-year) %>%
  pivot_wider(names_from = name, values_from = value)

save(ltdb_factors_tract_wide, file = "./data/derived/sensitivity/ltdb_factors_tract_wide.RData")
