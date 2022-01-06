# This is the master file path index file for all data required to reproduce this
# project. If you obtain all the secure data files and place them in the stated
# locations with the same names, the project is a one-push reproduction using
# 00_build_shared_data.R

# External data drive
ext_drive <- "G:/"
# Secure data directory. Typically an external drive.
secure_data_dir <- paste0(ext_drive, "SecureData/")

# "F:/SecureData/Matsueda-tract_linknc.sav" is the PHDCN / CCAHS / tract crosswalk from ICPSR
icpsr_tract_crosswalk_path <- paste0(secure_data_dir, "Matsueda-tract_linknc.sav")

# CCAHS
# "F:/SecureData/CCAHS/DS0001/31142-0001-Data-REST.dta" is the CCAHS main file
ccahs_main_path <- paste0(secure_data_dir, "CCAHS/DS0001/31142-0001-Data-REST.dta")

# "F:/SecureData/CCAHS/DS0002/31142-0002-Data-REST.dta" is the CCAHS SSO file
ccahs_sso_path <- paste0(secure_data_dir, "CCAHS/DS0002/31142-0002-Data-REST.dta")

# "F:/SecureData/CCAHS/DS0003/31142-0003-Data-REST.dta" is the CCAHS imputation data file for pulling incomes
ccahs_impute_path <- paste0(secure_data_dir, "CCAHS/DS0003/31142-0003-Data-REST.dta")

#PHDCN
# "F:/SecureData/PHDCN_Community_Survey_9497_ICPSR_02766/DS0001/02766-0001-Data-REST.sav" is the PHDCN-CS individual survey responses
phdcn_indiv_path <- paste0(secure_data_dir, "PHDCN_Community_Survey_9497_ICPSR_02766/DS0001/02766-0001-Data-REST.sav")

# "F:/SecureData/da02766-0002_Matsueda_02062019.sav" is the PHDCN-CS NC-level data with census and crime measures, which for some reason aren't on ICPSR
phdcn_cs_nc_path <- paste0(secure_data_dir, "da02766-0002_Matsueda_02062019.sav")
## Note in all the NC-level data they are missing NC 792 which exists in individual level data though has a lot of missings

# LTDB
# Not a contract-agreed secure dataset, so not in SecureData
ltdb_dir <- paste0(ext_drive, "LTDB/ltdb_interpolate_stata/")

# "F:/LTDB/ltdb_interpolate_stata/std_1990_fullcount_cw2000.dta" is the standard 1990 fullcount LTDB interpolated to 2000 tracts using their Stata script file
ltdb_1990_fullcount_path <- paste0(ltdb_dir, "std_1990_fullcount_cw2000.dta")

# "F:/LTDB/ltdb_interpolate_stata/std_1990_sample_cw2000.dta" is the same for standard 1990 sample
ltdb_1990_sample_path <- paste0(ltdb_dir, "std_1990_sample_cw2000.dta")

# "F:/LTDB/ltdb_interpolate_stata/std_2000_full_cw2000.dta" is the same for standard 2000. Note this is still interpolated; LTDB uses 2010 as default tract
ltdb_2000_fullcount_path <- paste0(ltdb_dir, "std_2000_full_cw2000.dta")

# "F:/LTDB/ltdb_interpolate_stata/std_2000_sample_cw2000.dta" is same for standard 2000 sample
ltdb_2000_sample_path <- paste0(ltdb_dir, "std_2000_sample_cw2000.dta")

# "./raw/blocks_1990/IL_block_1990.shp" is the 1990 Illinois census block line file
il_block_1990_path <- "./data/raw/blocks_1990/IL_block_1990.shp"




# "F:/chicago_police_data/Crimes_-_2001_to_Present.csv" is a 1.6 gig 2001+ Chicago PD crime data file from Chicago's open data portal
cpd_data_path <- paste0(ext_drive, "chicago_police_data/Crimes_-_2001_to_Present.csv")
