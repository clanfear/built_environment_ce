library(tidyverse)
library(simstudy)
library(lme4)
source("./syntax/project_functions.R")

lme_reliability <- function(x){
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  n <- table(insight::get_random(x))
  J <- length(n) # number of neighbs
  return(sum(t00 / (t00 + s2 / n)) / J)
}

tau_b          <- 0.13
sigma2         <- 0.99
crime_var      <- tau_b # Same as tau_b for convenience
crime_ce_slope <- -0.5

defg <- defData(varname = "boj", formula=0, variance = tau_b, id = "NC_ID")
defy <- defDataAdd(varname = "ce", formula = "boj", variance = sigma2, dist = "normal")
defc <- defDataAdd(varname = "crime", formula = paste0(crime_ce_slope, " * boj"), dist = "normal", variance = crime_var)

cf_data <- genData(343, defg, id = "NC_ID") %>%
  addColumns(defc, .) %>% 
  genCluster(., "NC_ID", numIndsVar = 100, level1ID = "RESP_ID") %>%
  addColumns(defy, .) %>%
  as_tibble() 

sim_rep <- function(neighb_n, varying = FALSE){
  if(varying == TRUE){
    cf_df <- cf_data %>%
      group_by(NC_ID) %>%
      nest() %>%
      mutate(n = rpois(n(), neighb_n)) %>%
      mutate(n = ifelse(n < 3, 3, n)) %>% # Keep minimum of 1 observation in neighb for stability
      mutate(data = map2(data, n, sample_n)) %>%
      unnest(data)
  } else {
    cf_df <- cf_data %>% 
      group_by(NC_ID) %>%
      filter(RESP_ID %in% sample(unique(RESP_ID), neighb_n)) %>% 
      ungroup()
  }
    lmer_out <- lmer(ce ~ 1 + (1|NC_ID), data = cf_df, control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
    out_mat <- setNames(ranef(lmer_out)$NC_ID, paste0("CE", "_hlm")) %>% 
      tibble::rownames_to_column("NC_ID") %>% 
      as_tibble() %>% 
      mutate(NC_ID = as.character(NC_ID)) %>%
      inner_join(cf_df %>%
                   mutate(NC_ID = as.character(NC_ID)) %>%
                   group_by(NC_ID) %>% 
                   slice(1L), by = "NC_ID") %>%
      mutate(across(c(CE_hlm, crime), ~standardize(.))) %>%
      lm(crime ~ CE_hlm, data = .) %>% 
      summary() %>%
      {.$coefficients["CE_hlm", c(1,2)]} %>%
      c(., "reliability" = lme_reliability(lmer_out), neighb_n = neighb_n)
    return(out_mat)
}

# J_k of 3 to 50
# 50 replicates each

n_range <- seq(3, 50, by = 1)
replicates <- 50

# Iterate over n_range values, take averages

sim_reliability <- t(sapply(n_range, \(x) colMeans(t(replicate(replicates, sim_rep(x), simplify = TRUE)))))
sim_reliability_df <- sim_reliability %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(attenuation = 1 - (estimate/crime_ce_slope),
                   t = estimate / std_error)

lm(estimate ~ poly(reliability, 2, raw = TRUE), data = sim_reliability_df) %>% summary()

sim_reliability_df %>% 
  rename(Estimate = estimate, `T Ratio` = t) %>%
  pivot_longer(c(Estimate, `T Ratio`)) %>%
  ggplot(aes(x = reliability, y = value)) + 
  facet_wrap(~ name, scales = "free_y") + 
  geom_point() + 
  geom_smooth() + 
  xlim(0.2, 0.9) +
  ylab("") + xlab("Reliability") +
  geom_vline(xintercept = 0.503, color = "red") + 
  geom_vline(xintercept = 0.758, color = "blue", linetype = "dashed") +
  theme_minimal()

# Repeat while varying the J_ks within neighborhoods

sim_reliability_var <- t(sapply(n_range, \(x) colMeans(t(replicate(replicates, sim_rep(x, varying = TRUE), simplify = TRUE)))))
sim_reliability_var_df <- sim_reliability_var %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  mutate(attenuation = 1 - (estimate/crime_ce_slope),
         t = estimate / std_error)

lm(estimate ~ poly(reliability,2,raw = TRUE), data = sim_reliability_var_df) %>% summary()

sim_reliability_var_df %>% 
  rename(Estimate = estimate, `T Ratio` = t) %>%
  pivot_longer(c(Estimate, `T Ratio`)) %>%
  ggplot(aes(x = reliability, y = value)) + 
  facet_wrap(~ name, scales = "free_y") + 
  geom_point() + 
  geom_smooth() + 
  xlim(0.2, 0.9) +
  ylab("") + xlab("Reliability") +
  geom_vline(xintercept = 0.503, color = "red") + 
  geom_vline(xintercept = 0.758, color = "blue", linetype = "dashed") +
  theme_minimal()
