# This file just documents what it takes to actually build this monster.
# You'll need a lot of restricted access data, some of which is likely to come
# to you in a different format than what I have (e.g. crosswalks). It is also
# tidyverse dependent, which means things might break. Warning: This will take
# an assload of time, due in large part to measurement models, but also big
# spatial joins.

# PACKAGES NECESSARY TO BUILD
required_packages   <- c("tidyverse", "areal", "sf", "janitor", "vroom", "tidycensus", "tigris", "psych", "lwgeom", "GPArotation")

# SCRIPT NAMES
processing_scripts  <- list.files("./syntax/chicago/data_processing/", full.names = T)
measurement_scripts <- list.files("./syntax/chicago/measurement/", full.names = T)

# FILES NECESSARY TO BUILD (in order of appearance)

# "F:/SecureData/Matsueda-tract_linknc.sav" is the PHDCN / CCAHS / tract crosswalk from ICPSR
# "F:/SecureData/CCAHS/DS0001/31142-0001-Data-REST.dta" is the CCAHS main file
# "F:/SecureData/CCAHS/DS0002/31142-0002-Data-REST.dta" is the CCAHS SSO file
# "./raw/blocks_1990/IL_block_1990.shp" is the 1990 Illinois census block line file
# "F:/LTDB/ltdb_interpolate_stata/std_1990_fullcount_cw2000.dta" is the standard 1990 fullcount LTDB interpolated to 2000 tracts using their Stata script file
# "F:/LTDB/ltdb_interpolate_stata/std_1990_sample_cw2000.dta" is the same for standard 1990 sample
# "F:/LTDB/ltdb_interpolate_stata/std_2000_full_cw2000.dta" is the same for standard 2000. Note this is still interpolated; LTDB uses 2010 as default tract
# "F:/LTDB/ltdb_interpolate_stata/std_2000_sample_cw2000.dta" is same for standard 2000 sample
# "F:/SecureData/PHDCN_Community_Survey_9497_ICPSR_02766/DS0001/02766-0001-Data-REST.sav" is the PHDCN-CS individual survey responses
# "F:/SecureData/da02766-0002_Matsueda_02062019.sav" is the PHDCN-CS NC-level data with census and crime measures, which for some reason aren't on ICPSR
## Note in all the NC-level data they are missing NC 792 which exists in individual level data though has a lot of missings
# "F:/SecureData/CCAHS/DS0003/31142-0003-Data-REST.dta" is the CCAHS imputation data file for pulling incomes
# "D:/Projects/dissertation_data/chicago/chicago_police_data/Crimes_-_2001_to_Present.csv" is a 1.6 gig 2001+ Chicago PD crime data file from Chicago's open data portal





# DATA FIRST
for(script in processing_scripts){
  message(paste0("Running: ", script))
  source(script)
}
# MEASUREMENT SECOND
for(script in measurement_scripts){
  message(paste0("Running: ", script))
  source(script)
}