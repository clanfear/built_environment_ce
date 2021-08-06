# This script uses Logan et al.'s Longitudinal Tract Database (LTDB) to get
# consistent structural measures across the 1990 and 2000 census on 2000 tract
# geometries. Then I do factor analysis to get the factors from Sampson et al. 
# 1997, except with one less measure not in the LTDB.
# Note converting the LTDB 1990 and 2000 files to 2000 census tracts requires 
# the original files and Stata scripts from here (as of 3/24/2021): 
# https://s4.ad.brown.edu/projects/diversity/researcher/bridging.htm
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

load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")
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

save(nc_boundaries, file = "./data/chicago/derived/nc_boundaries.RData")

nc_neighbors <- nc_boundaries  %>%
  mutate(neighbors = st_queen(.)) %>%
  st_drop_geometry() %>%
  tidyr::unnest(neighbors)

save(nc_neighbors, file = "./data/chicago/derived/nc_neighbors.RData")

nc_areas <- nc_sf %>%
  st_drop_geometry() %>%
  group_by(ccahs_nc, phdcn_nc) %>%
  summarize(area_nc = sum(area_tract)) %>%
  ungroup()

ltdb_1990 <- inner_join(haven::read_stata("F:/LTDB/ltdb_interpolate_stata/std_1990_fullcount_cw2000.dta"),
           haven::read_stata("F:/LTDB/ltdb_interpolate_stata/std_1990_sample_cw2000.dta")) %>%
  mutate(year = 1990) %>%
  rename_with(~str_remove(., "90|00"))

ltdb_2000 <- inner_join(haven::read_stata("F:/LTDB/ltdb_interpolate_stata/std_2000_full_cw2000.dta"),
           haven::read_stata("F:/LTDB/ltdb_interpolate_stata/std_2000_sample_cw2000.dta")) %>%
    mutate(year = 2000) %>%
  rename_with(~str_remove(., "00")) %>%
  select(-rent) # whoops, left rent in

x_notin_y(names(ltdb_1990), names(ltdb_2000))

ltdb_il <- bind_rows(ltdb_1990, ltdb_2000) %>%
  filter(str_sub(trtid, 1, 5)=="17031") %>%
  mutate(census_tract_6 = str_sub(trtid, 6, -1)) %>%
  inner_join(nc_tracts)

ltdb_il_nc <- ltdb_il %>%
  select(-trtid, -census_tract_6) %>%
  group_by(ccahs_nc, phdcn_nc, year) %>%
  summarize(across(everything(), ~sum(.)), .groups = "drop") %>% 
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
  select(-matches("^(d_|n_)")) %>%
  left_join(nc_areas) %>%
  mutate(density_ltdb_nc = population_ltdb_nc / area_nc)
  
ltdb_factors <- ltdb_il_nc %>% 
  select(-ccahs_nc, -phdcn_nc, -year, -perc_edcollege, -perc_edhighschool, -perc_professional, -population_ltdb_nc, -density_ltdb_nc, -area_nc) %>% 
  psych::fa(nfactors = 3,  scores="tenBerge", rotate="oblimin", fm="alpha") %>%
  {.$scores} %>%
  cbind(select(ltdb_il_nc, ccahs_nc, phdcn_nc, year, population_ltdb_nc, density_ltdb_nc), .) %>%
  rename(FAC_disadv = alpha1,
         FAC_stability = alpha2,
         FAC_hispimm = alpha3)

ltdb_factors_wide <- ltdb_factors %>%
  mutate(across(matches("^FAC_"), ~ standardize(., 2))) %>%
  pivot_longer(c(FAC_disadv, FAC_hispimm, FAC_stability, population_ltdb_nc, density_ltdb_nc)) %>%
  mutate(name = paste(name, year, sep = "_")) %>%
  select(-year) %>%
  pivot_wider(names_from = name, values_from = value)

save(ltdb_factors_wide, file = "./data/chicago/derived/ltdb_factors_wide.RData")

# These are just notes on factor loadings across different measures in SR&E1997
# and the CCAHS for purposes of comparison.

# SR&E 1997
# CONCENTRATED DISADVANTAGE
# Below poverty line 0.93
# On public assistance 0.94 <- can't get this one in LTDB
# Female-headed families 0.93
# Unemployed 0.86
# Less than age 18 0.94
# Black 0.60

# IMMIGRANT CONCENTRATION
# Latino 0.88
# Foreign-born 0.70

# RESIDENTIAL STABILITY
# Same house as in 1985 0.77
# Owner-occupied house 0.86

# CCAHS
# DISADVANTAGE
# %Families with Income Less Than $10k 0.91 -0.24 -0.21 0.00
# %Families with Income $50k or Higher -0.83 0.45 -0.02 0.07
# %Families in Poverty 0.86 -0.37 -0.19 -0.15
# %Families on Public Assistance 0.75 -0.40 -0.41 -0.09
# %Unemployed in Civilian Labor Force 0.67 -0.41 -0.47 -0.07
# %Families Female Headed 0.71 -0.34 -0.57 -0.07
# %Never Married 0.61 0.25 -0.39 -0.55
# %Less than 12 years of education 0.40 -0.73 0.38 -0.26

# AFFLUENCE
# %16 or more years of education -0.26 0.93 0.00 -0.10
# %Professional/Managerial Occupation -0.23 0.92 -0.15 0.02

# HISP/IMMIG
# %Non-Hispanic Black 0.43 -0.26 -0.79 0.11
# %Hispanic -0.14 -0.34 0.77 -0.39
# %Foreign Born -0.16 -0.04 0.91 -0.07

# Older Age Comp
# %Homes Owner Occupied -0.81 -0.21 -0.17 0.36
# %In Same Residence in 1995 -0.20 -0.65 -0.41 0.41
# %0-17 Years Old 0.39 -0.85 -0.16 -0.18
# %18-29 Years Old 0.04 0.51 0.30 -0.71
# %30-39 Years Old -0.17 0.72 0.31 -0.38
# %50-69 Years Old -0.38 0.08 -0.38 0.70
# %70+ Years Old -0.15 0.20 -0.03 0.87