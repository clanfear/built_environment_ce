# This file processes the individual-level CCAHS data into something ready
# for the measurement models. It also tests directionality of items within 
# scales to make sure they're all correlated the same. That isn't important for
# SEM/CFA but it is vital for the hierarchical measurement models.
# This is all a bit more involved than the PHDCN-CS.

library(tidyverse)
source("./syntax/project_functions.R")
ccahs_dir    <- "F:/SecureData/CCAHS/"

# Note CCAHS has a .do file to change missings (e.g. -2, -5) to .
# I do this manually to avoid invoking Stata.
ccahs_individual_raw     <- haven::read_dta(paste0(ccahs_dir, "DS0001/31142-0001-Data-REST.dta"))
ccahs_individual_imputed <- haven::read_dta(paste0(ccahs_dir, "DS0003/31142-0003-Data-REST.dta"))

# Not in CCAHS: c_years_in_neighb + c_moves_last_5yr

imputed_incomes <- ccahs_individual_imputed %>%
  mutate(RESP_ID = as.character(V1)) %>%
  group_by(RESP_ID) %>%
  summarize(income = mean(V3521))

ccahs_individual <- ccahs_individual_raw %>%
  select(RESP_ID        = V1,
         NC_ID          = V5,
         TRACT_ID       = V6,
         BLOCK_ID       = V7,
         interview_date = V8,
         # NEed to get the proper vars for these
         COHTR_closeknit   = V511,
         COHTR_help        = V513,
         COHTR_getalong    = V514,
         COHTR_sharevalues = V517,
         COHTR_trust       = V516,
         INF_skip          = V480,
         INF_graffiti      = V481,
         INF_disrespect    = V482,
         INF_fight         = V483,
         INF_firestation   = V484,
         # PE_dealing_with_problems       = V566, # Only asked of limited number of respondents (long form)
         PE_working_with_residents      = V567,
         PE_fair_to_people              = V568,
         PE_trusted                     = V569,
         LC_madetobreak    = V620,
         LC_anythingokay   = V621,
         LC_norightwrong   = V622,
         LC_livefortoday   = V623,
         PV_weaponfight            = V543,
         PV_argument               = V544,
         PV_gangfight              = V545,
         PV_sexviolence            = V546,
         PV_robbery                = V547,
         VICT_violent_ever = V558,
         TE_favors                 = V493,
         TE_watch                  = V494,
         TE_advice                 = V495,
         TE_gettogethers           = V496,
         TE_visit                  = V497,
         # AT_likeneighb             = , # No attachment questions in CCAHS
         # AT_missneighb             = ,
         KT_relatives              = V488,
         KT_friends                = V489,
         # recognize_strangers       = V491, # Still mulling utility of ability to recognize strangers
         # VICT_violent_ever = V1103,
         #  VICT_violent_5yrs = V2262,
         FEMALE            = V104, # femal_02,
         FAM_married       = V2063, # marr_02
         separated         = V2064, # sepdv_02
         divorced          = V2065,
         FAM_single        = V2067, # singl_02
         HOMEOWN           = V2069, # ownhm_02
         RACE_latino       = V2048, # hisp_02
         RACE_black        = V2052, # black_02
         # MOBILITY          = , # imob_02 not found in data   # N moves in past 5 yrs
         AGE               = V2000, # R Age_02
         year_moved_in     = V1416,
         education         = V2001,
         # YRS_IN_NEIGHB     = , # iyrnh_02
         # SES               = , # No SES?   # First PCA of education, income, and occupational prestige
         # indiv_colleff_hlm      = V2812 # Individual HLM calculated CE scale with imputation
         ) %>%
  mutate(RESP_ID = as.character(RESP_ID),
         NC_ID = as.character(NC_ID),  
         TRACT_ID = as.character(round(TRACT_ID, 0)), # Dumping erroneous decimal in tract ID; doesn't seem to exist elsewhere.
         BLOCK_ID = as.character(BLOCK_ID)) %>% 
  mutate(across(c(matches("^(COHTR_|INF_|PE_|LC_|PV_|VICT_|TE_|KT_)"),
                FEMALE, FAM_married, FAM_single, HOMEOWN, RACE_latino, RACE_black),
                ~ as.numeric(ifelse(. < 0, NA, .)))) %>%
  mutate(across(matches("^(COHTR_|INF_|PE_|LC_|PV_|TE_)"), ~ (.*-1)+5),
         VICT_violent_ever = case_when(
           is.na(VICT_violent_ever) ~ NA_real_,
           VICT_violent_ever == 5 ~ 0,
           VICT_violent_ever == 1 ~ 1)) %>%
  left_join(imputed_incomes) %>%
  mutate(FAM_sepdiv = factor(ifelse(separated == 1 | divorced == 1, "Yes", "No")),
         interview_year = lubridate::year(lubridate::dmy(interview_date)),
         year_moved_in = ifelse(year_moved_in < 1900, NA, as.numeric(year_moved_in))) %>%
  mutate(years_in_home =  interview_year - year_moved_in) %>%
  select(-interview_year, -year_moved_in, -interview_date, -separated, -divorced) %>%
  mutate(across(c(-RESP_ID, -NC_ID, -TRACT_ID, -BLOCK_ID), ~as.numeric(.))) %>%
  filter(across(c(FEMALE, FAM_married, FAM_sepdiv, FAM_single, HOMEOWN, 
                  RACE_latino, RACE_black, AGE, years_in_home, income, education), ~ !is.na(.)))

ccahs_individual$SES <- ccahs_individual %>% 
  select(income, education) %>% 
  {prcomp(., scale=T, center=T)$x[,1]} %>%
  standardize()

ccahs_individual <- ccahs_individual %>% select(-income, -education)

ccahs_individual_long <- ccahs_individual %>%
  pivot_longer(matches("^(COHTR_|INF_|PE_|LC_|PV_|TE_|KT_)", ignore.case = FALSE), 
               names_to = "measure", values_to = "value") %>%
  filter(!is.na(value))

# SEM needs wide data
ccahs_individual_wide <- ccahs_individual_long %>%
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(across(matches("^(COHTR_|INF_|PE_|LC_|PV_|TE_|KT_)", ignore.case = FALSE), ~ ordered(.)))

# Test directionality of indicators within indices--all positive correlations
ind_dir_check <- function(pattern, r = FALSE, df = ccahs_individual_long){
  temp_cor <- df %>%
    mutate(value = as.numeric(value)) %>%
    pivot_wider(names_from = measure, values_from = value) %>%
    select(matches(pattern, ignore.case = FALSE)) %>% 
    na.omit() %>% 
    cor()
  if(r == TRUE){
    return(temp_cor)
  } else{
    return(all(temp_cor > 0))
  }
}

ind_vec <- c("^(COHTR_|INF_)", "^PE_", "^LC_", "^PV_", "^TE", "^KT")
if(!all(map_lgl(ind_vec, ind_dir_check))) message("Directionality problem detected.")
map(ind_vec, ~ind_dir_check(., r=TRUE))

ccahs_individual_long %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  select(matches("^(COHTR_|INF_|PE_|LC_|PV_|TE_|KT_)", ignore.case = FALSE)) %>% 
  na.omit() %>% 
  cor()

save(ccahs_individual_long, file = "./data/chicago/derived/ccahs_individual_long.RData")
save(ccahs_individual_wide, file = "./data/chicago/derived/ccahs_individual_wide.RData")

