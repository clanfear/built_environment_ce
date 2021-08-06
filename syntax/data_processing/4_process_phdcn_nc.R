# This file pulls the pre-calculated NC-level measures from the PHDCN-CS. I 
# don't actually really use most of these but it is the only source for the 
# homicide and violent crime NC data. Naturally some of the homicide 
# calculations are kind of nonsensical as documented below. I calculate my own
# from counts and populations.

library(tidyverse)
source("./syntax/project_functions.R")

phdcn_neighb_raw <- haven::read_sav("F:/SecureData/da02766-0002_Matsueda_02062019.sav")

# Baffling thing about the NC data is there is no NC 792, which does exist in individual data
# Why is this missing from the neighborhood files?
# For whatever reason, they seem to be heavy on the NAs. Wonder if dropped for quality?
# I recalculate almost all of these myself using LTDB and measurement models
# but I can't get back homicide or violent crime, so that NC gets dropped in some models.

phdcn_nc <- phdcn_neighb_raw %>%
  select(NC_ID = link_nc,
         INF_1995           = ebcontro,
         COHTR_1995         = ebcohesi,
         PERC_DIS_1995      = ebdisord,
         PERC_VIOL_1995     = ebpviol,
         VICT_EVER_1995     = ebsntotv,
         VICT_6MO_1995      = ebrntotv,
         LOG_HOMICIDE_1990  = lhomr90,
         LOG_HOMICIDE_1995  = lhomr95,
         CNT_MURDER_1995    = murder95,
         CNT_MURDER_1989_1991 = hom,
         VIOLENT_CRIME_1995 = violcrim,
         VICTIM_6MO_1995    = rvict6mo,
         POPULATION_1995    = totpop,
         DISADVANTAGE_1995  = oblfac1,
         IMMIGRANT_1995     = oblfac2,
         STABILITY_1995     = oblfac3
  ) %>%
  mutate(NC_ID = as.character(NC_ID),
         HOM_RATE_1990 = 100000*((1/3)*CNT_MURDER_1989_1991/POPULATION_1995),
         LOG_HOM_RATE_1990 = log(HOM_RATE_1990 + 1),
         HOM_RATE_1995 = 100000*(CNT_MURDER_1995/POPULATION_1995),
         LOG_HOM_RATE_1995 = log(HOM_RATE_1995 + 1))


# IT is completely unknown what VIOLENT_CRIME_1995 is. It may be the construct
# from Sampson & Bartusch 1998 which combineds perceived violence, homicide,
# and victimization.



# lmhom90 and lhom95 are kinda weird and inconsistent:

# Well, that's what they did:
# They took murder95, divided by population, added one, then logged it, then rounded to 4 digits.
# cor(round(log(phdcn_nc$CNT_MURDER_1995/phdcn_nc$POPULATION_1995 + 1), 4), phdcn_nc$LOG_HOMICIDE_1995)
# cbind(round(log(phdcn_nc$CNT_MURDER_1995/phdcn_nc$POPULATION_1995 + 1), 4), phdcn_nc$LOG_HOMICIDE_1995)
# 
# # Here they took 1989-1991 homicide, took the mean (1/3), divided by total pop, multiplied by 100000, then added 1, then took the log.
# cor(log(100000*((1/3)*phdcn_nc$CNT_MURDER_1989_1991/phdcn_nc$POPULATION_1995) + 1), phdcn_nc$LOG_HOMICIDE_1990)
# cbind(log(100000*((1/3)*(phdcn_nc$CNT_MURDER_1989_1991/phdcn_nc$POPULATION_1995 + 3)), phdcn_nc$LOG_HOMICIDE_1990))

# This approach is problematic because it yields a linear relationship between 1995 rate and log rate and a log-linear one between the 1990 ones.

phdcn_nc <- phdcn_nc %>% select(-LOG_HOMICIDE_1990, -LOG_HOMICIDE_1995)
save(phdcn_nc, file = "./data/chicago/derived/phdcn_nc.RData")