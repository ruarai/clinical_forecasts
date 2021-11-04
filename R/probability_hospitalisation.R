
library(tidyverse)
library(data.table)
library(lubridate)


source("R/model_parameters.R")
model_params <- get_model_parameters()

date_start <- ymd("2021-06-01")
state_modelled <- "NSW"




clinical_linelist <- read_rds("data/processed/clinical_linelist.rds") %>%
  filter(date_onset >= date_start,
         state == state_modelled) %>%
  
  mutate(age_class_factor = factor(age_class, levels = model_params$covariates_age)) %>%
  arrange(date_onset, age_class_factor)


dt_clinical_linelist <- data.table(clinical_linelist,
                                   key = c("age_class", "date_onset",
                                           "status_hospital", "status_ICU"))

data_date <- ymd("2021-10-28")


fn_score <- function(x, A, days_since_onset, delay_shape, delay_mean) {
  prob_already_observed <- pgamma(days_since_onset, shape = delay_shape, scale = delay_mean / delay_shape)
  
  
  A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
}


prob_naive <- function(data, numer_fn, denom_fn = identity) {
  n_numer <- nrow(data %>% numer_fn)
  n_denom <- nrow(data %>% denom_fn)
  
  return(n_numer / n_denom)
}

fix_prob <- function(p) {
  if(is.infinite(p)) return(0)
  if(is.na(p)) return(0)
  return(min(max(p, 0), 1))
}

pr_hosp_total <- prob_naive(clinical_linelist,
                            function(x) x %>% filter(status_hospital == 1))

pr_ICU_total <- prob_naive(clinical_linelist,
                           function(x) x %>% filter(status_ICU == 1),
                           function(x) x %>% filter(status_hospital == 1))


pr_total_by_age <- clinical_linelist %>%
  group_by(age_class) %>%
  do(pr_hosp_total_age = prob_naive(., function(x) x %>% filter(status_hospital == 1)),
     pr_ICU_total_age = prob_naive(.,
                               function(x) x %>% filter(status_ICU == 1),
                               function(x) x %>% filter(status_hospital == 1))) %>%
  
  unnest(c(pr_hosp_total_age, pr_ICU_total_age))



clinical_probs_ageless <- function(date_start) {
  n_days_forward <- 14
  dates <- seq(date_start, date_start + n_days_forward, by = 'day')
  
  cases_on_dates <- dt_clinical_linelist[.(dates), on = "date_onset",
                                         nomatch = NULL]
  
  cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
  cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
  
  
  nothosp_days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
  
  
  delay_shape <- model_params$delay_params$
    compartment_LoS_mean[cases_not_hospitalised$age_class, "symptomatic_to_ED"]
  delay_mean <- model_params$delay_params$
    compartment_LoS_shape[cases_not_hospitalised$age_class, "symptomatic_to_ED"]
  
  
  prob_hosp_MLE <- tryCatch(pracma::fzero(
    function(x) {fn_score(x, nrow(cases_hospitalised),
                          nothosp_days_since_onset,
                          delay_shape, delay_mean)},
    
    x = c(0+.Machine$double.eps,1-.Machine$double.eps),
  )$x,
  error = function(c) { return(pr_hosp_total) })
  
  
  
  cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
  cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
  
  
  
  
  
  tibble(
    prob_hosp_MLE = prob_hosp_MLE,
    
    prob_ICU = nrow(cases_ICU) / nrow(cases_not_ICU),
    
    n_cases_hospitalised = nrow(cases_hospitalised),
    
    n_cases_ICU = nrow(cases_ICU)
  )
}


results_ageless <- tibble(date = seq(min(clinical_linelist$date_onset),
                  max(clinical_linelist$date_onset) - 14,
                  by = 'days')) %>%
  group_by(date) %>%
  summarise(clinical_probs_ageless(date)) %>%
  
  mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 50),
         weight_use_ICU = pmin(1, n_cases_ICU / 50)) %>%
  
  mutate(pr_hosp = prob_hosp_MLE * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
         
         pr_ICU = prob_ICU * weight_use_ICU + pr_ICU_total * (1 - weight_use_ICU)) %>%
  
  select(date, pr_hosp, pr_ICU)



ggplot(results_ageless %>% pivot_longer(cols = c(pr_hosp, pr_ICU))) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~name)



clinical_probs_by_ageclass <- function(date_start, filter_age_class) {
  
  n_days_forward <- 14
  dates <- seq(date_start, date_start + n_days_forward, by = 'day')
  
  cases_on_dates <- dt_clinical_linelist[.(filter_age_class, dates), on = c("age_class", "date_onset"),
                                         nomatch = NULL]
  
  cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
  cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
  
  
  cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
  cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
  
  tibble(
    prob_hosp = fix_prob(nrow(cases_hospitalised) / nrow(cases_on_dates)),
    
    prob_ICU = fix_prob(nrow(cases_ICU) / nrow(cases_not_ICU)),
    
    n_cases_hospitalised = nrow(cases_hospitalised),
    
    n_cases_ICU = nrow(cases_ICU)
  )
}


results_by_ageclass <- expand_grid(
  date = seq(min(clinical_linelist$date_onset),max(clinical_linelist$date_onset) - 14, by = 'days'),
  age_class = unique(clinical_linelist$age_class)
  ) %>%
  
  group_by(date, age_class) %>%
  summarise(clinical_probs_by_ageclass(date, age_class)) %>%
  
  left_join(pr_total_by_age) %>%
  
  mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 50),
         weight_use_ICU = pmin(1, n_cases_ICU / 50)) %>%
  
  mutate(pr_hosp = prob_hosp * weight_use_hosp + pr_hosp_total_age * (1 - weight_use_hosp),
         
         pr_ICU = prob_ICU * weight_use_ICU + pr_ICU_total_age * (1 - weight_use_ICU)) %>%
  
  select(date, age_class, pr_hosp, pr_ICU)




ggplot(results_by_ageclass %>% pivot_longer(cols = c(pr_hosp, pr_ICU))) +
  geom_line(aes(x = date, y = value, color = age_class)) +
  facet_wrap(~name)



clinical_probabilities <- results_by_ageclass %>%
  ungroup() %>%
  mutate(date_onset = date - 7) %>%
  
  select(date_onset, age_class, pr_hosp, pr_ICU) %>%
  
  right_join(expand_grid(date_onset = seq(ymd("2021-01-01"), ymd("2022-01-01"), by = 'days'),
                         age_class = unique(clinical_linelist$age_class))) %>%
  arrange(date_onset) %>%
  
  group_by(age_class) %>%
  fill(pr_hosp, pr_ICU, .direction = 'updown') %>%
  ungroup()


write_rds(clinical_probabilities,
          "data/processed/clinical_probabilities.rds")


