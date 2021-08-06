library(tidyverse)
library(corrplot)
source("./syntax/project_functions.R")
load("./data/chicago/derived/phdcn_cs_individual_wide.RData")
load("./data/chicago/derived/ccahs_individual_wide.RData")
load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")

phdcn_cs_individual_wide %>%
  group_by(NC_ID) %>%
  summarize(across(matches("^PE_"), ~mean(as.numeric(.), na.rm=TRUE))) %>%
  ungroup() %>%
  rename_with(~paste0(., "_95"), -NC_ID) %>%
  left_join(complete_crosswalk %>% distinct(phdcn_nc, ccahs_nc), by = c("NC_ID"="phdcn_nc")) %>%
  inner_join(ccahs_individual_wide %>%
               group_by(NC_ID) %>%
               summarize(across(matches("^PE_"), ~mean(as.numeric(.), na.rm=TRUE))) %>%
               rename_with(~paste0(., "_03"), -NC_ID), 
             by = c("ccahs_nc"="NC_ID")) %>%
  select(-NC_ID, -ccahs_nc) %>% cor() %>% {.[1:8, 9:11]} %>% corrplot(addCoef.col = "black")
  
