# This file processes the individual-level PHDCN-CS data into something ready
# for the measurement models. It also tests directionality of items within 
# scales to make sure they're all correlated the same. That isn't important for
# SEM/CFA but it is vital for the hierarchical measurement models.

library(tidyverse)
source("./syntax/project_functions.R")

phdcn_cs_dir    <- "F:/SecureData/PHDCN_Community_Survey_9497_ICPSR_02766/"

phdcn_cs_individual_raw <- haven::read_sav(paste0(phdcn_cs_dir, "DS0001/02766-0001-Data-REST.sav"))


# The local exchange scale is based on five items in the survey that asked how often
# the respondent and neighbors exchange favors or goods (e.g., tools), have parties together,
# visit in each others’ homes, watch each others’ homes, and exchange advice
# or information. The measure of friend/kinship ties is derived from questions on the
# number of friends and relatives that respondents reported living in the neighborhood.
# Attachment is a two-item scale derived from questions on how sorry the respondent
# would be to move from the neighborhood and how satisfied he/she was with the
# neighborhood.

phdcn_cs_individual <- phdcn_cs_individual_raw %>%
  select(RESP_ID                   = RC_NUM,
         NC_ID                     = LINK_NC,
         COHTR_closeknit           = Q11B,
         COHTR_help                = Q11E,
         COHTR_getalong            = Q11F,
         COHTR_sharevalues         = Q11K,
         COHTR_trust               = Q11M,
         INF_skip                  = Q12A,
         INF_graffiti              = Q12B,
         INF_disrespect            = Q12C,
         INF_fight                 = Q12E,
         INF_firestation           = Q12F,
         PE_responsive_to_issues   = Q42A,
         PE_dealing_with_problems  = Q42B,
         PE_doing_good_job         = Q42C,
         PE_responding_to_victims  = Q42D,
         PE_maintain_order         = Q42E,
         PE_police_patrol_response = Q29H,
         PE_excessive_force        = Q29I,
         PE_protection_decline     = Q44D,
         LC_madetobreak            = Q41A,
         LC_anythingokay           = Q41B,
         LC_norightwrong           = Q41C,
         LC_nobodysbsns            = Q41D,
         LC_livefortoday           = Q41F,
         PV_weaponfight            = Q30A,
         PV_argument               = Q30B,
         PV_gangfight              = Q30C,
         PV_sexviolence            = Q30D,
         PV_robbery                = Q30E,
         TE_favors                 = Q18,
         TE_watch                  = Q19,
         TE_advice                 = Q20,
         TE_gettogethers           = Q21,
         TE_visit                  = Q22,
         AT_likeneighb             = Q4,
         AT_missneighb             = Q9,
         KT_relatives              = Q17A,
         KT_friends                = Q17B,
         # recognize_strangers       = Q16, # would be interesting to use at some point
         # VCE_burglary              = Q32,
         # VCE_theft                 = Q33,
         # VCE_damage                = Q34,
         VICT_violent_ever          = Q31,
         # VC6_burglary              = Q32A,
         # VC6_theft                 = Q33A,
         # VC6_damage                = Q34A,
         # VC6_violence              = Q31A,
         FEMALE                    = RFEMALE,
         FAM_married               = RMARRIED,
         FAM_sep_div               = RSEPDIV,
         FAM_single                = RSINGLE,
         HOMEOWN                   = ROWNHH,
         RACE_latino               = RHISPAN,
         RACE_black                = RNHBLACK,
         MOBILITY,                             # N moves in past 5 yrs
         AGE                       = RAGE,
         YRS_IN_NEIGHB             = IMPYRSNH,
         SES                       = PRIN1,   # First PCA of education, income, and occupational prestige
         ) %>%
  mutate(across(c(NC_ID, RESP_ID), ~ as.character(.)),
         across(c(-NC_ID, -RESP_ID), ~as.numeric(.))) %>%
  mutate(
    across(matches("^(VCE_|VC6_)"), 
           ~ case_when(
            is.na(.) ~ NA_real_,
            . == 2   ~ 0,
            . == 1   ~ 1,
            TRUE     ~ 99)),
    across(matches("^PV_"), 
           ~ (.*-1)+5),
    across(c(COHTR_getalong, COHTR_sharevalues, PE_doing_good_job, 
             PE_police_patrol_response, PE_maintain_order, PE_excessive_force), 
           ~ (.*-1)+6),
    VICT_violent_ever = case_when(
      is.na(VICT_violent_ever) ~ NA_real_,
      VICT_violent_ever == 2 ~ 0,
      VICT_violent_ever == 1 ~ 1
    )
    ) %>%
  mutate(across(matches("^(COHTR_|INF_|PE_|LC_)"), ~ (.*-1)+6),
         across(matches("^(AT|TE)"), ~ (.*-1)+5)) %>%
  filter(across(c(FEMALE, FAM_married, FAM_sep_div, FAM_single, HOMEOWN, 
                  RACE_latino, RACE_black, MOBILITY, AGE, YRS_IN_NEIGHB, SES), ~ !is.na(.)))


phdcn_cs_individual_long <- phdcn_cs_individual %>%
  pivot_longer(matches("^(COHTR_|INF_|PE_|LC_|PV_|AT_|TE_|KT_)", ignore.case = FALSE), names_to = "measure", values_to = "value", values_drop_na = TRUE)

phdcn_cs_individual_wide <- phdcn_cs_individual_long %>%
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(across(matches("^(COHTR_|INF_|PE_|LC_|PV_|AT_|TE_|KT_)", ignore.case = FALSE), ~ ordered(.)))

# DIRECTIONALITY:
## ISC_graffiti (- better)
## ISC_skip (- better)
## ISC_disrespect (- better)
## ISC_fight (- better)
## ISC_fire_station  (- better)

## COH_helping (- better)
## COH_close_knit (- better)
## COH_get_along (+ better)
## COH_share_values (+ better)
## COH_trust (- better)

## PE_dealing_with_problems (- better) # good job dealing with problems of concern to neighb
## PE_doing_good_job (+ better) # not doing good job preventing crime
## PE_responsive_to_issues (- better) # responsive to local issues
## PE_police_patrol_response (+ better) # how much problem is police not patrolling / responding
## PE_responding_to_victims (- better) # do good job responding to victims of crime
## PE_maintain_order (+ better) # not able to maintain order on streets and sidewalks
## PE_protection_decline (- better) # 
## PE_excessive_force (+ better) # how much of a problem is excessive force

# Test directionality of indicators within scales--all positive correlations

ind_dir_check <- function(pattern, r = FALSE, df = phdcn_cs_individual_long){
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

ind_vec <- c("^(COHTR_|INF_)", "^PE_", "^LC_", "^PV_", "^KT", "^TE", "^AT")
if(!all(map_lgl(ind_vec, ind_dir_check))) message("Directionality problem detected.")
map(ind_vec, ~ind_dir_check(., r=TRUE))



save(phdcn_cs_individual_long, file = "./data/chicago/derived/phdcn_cs_individual_long.RData")
save(phdcn_cs_individual_wide, file = "./data/chicago/derived/phdcn_cs_individual_wide.RData")
