
library(targets)
library(tidyverse)
library(lubridate)

morbidity_trajectories_state <- tar_read(morbidity_trajectories_state_NT)
forecast_dates <- tar_read(forecast_dates)

clinical_parameters <- tar_read(clinical_parameters)
case_trajectories <- tar_read(case_trajectories_NT)

clinical_parameter_samples <- tar_read(clinical_parameter_samples)
known_occupancy_ts <- tar_read(known_occupancy_ts_NT)

state_forecast_start <- tar_read(state_forecast_start_NT)




Sys.setenv(JULIA_PROJECT="/home/forecast/source/stochastic_progression/StochasticProgression/", JULIA_NUM_THREADS = 32)

library(JuliaCall)


case_curves <- case_trajectories$curve_set


morbidity_trajectories_state_ix <- morbidity_trajectories_state %>%
  mutate(t = match(date, unique(date)))



julia_source(
  "../stochastic_progression/sim_study.jl"
)



n_tests <- 20
test_set <- tibble(
  adj_pr_hosp = runif(n_tests, -1.5, 1.5),
  adj_los = runif(n_tests, -1.5, 1.5),
  log_importation_rate = runif(n_tests, -10, -4)
)

ggplot(test_set) +
  geom_point(aes(x = adj_pr_hosp, y = adj_los))

ggplot(test_set) +
  geom_point(aes(x = adj_pr_hosp, y = log_importation_rate))


test_results <- map(
  1:nrow(test_set),
  function(i) {
    print(str_c("Running test ", i))
    
    test_set_row <- test_set[i,]
    
    parameters <- c(test_set_row$adj_pr_hosp, test_set_row$adj_los, test_set_row$log_importation_rate)
    
    prior <- julia_call(
      "sample_from_prior",
      case_trajectories$n_days,
      4,
      
      case_curves,
      clinical_parameter_samples,
      morbidity_trajectories_state_ix,
      
      parameters
    )
    
    
    results_inference <- julia_call(
      "infer_abc_decimation",
      prior$context,
      prior$occupancy
    )
    
    return(results_inference$parameters)
  }
)


# 
# write_rds(test_set, "data/exps/test_set.rds")
# write_rds(test_results, "data/exps/test_results.rds")




test_set


test_results %>%
  bind_rows(.id = "id") %>%
  mutate(id = as.numeric(id)) %>% 
  left_join(
    test_set %>% mutate(id = row_number()) %>%
      rename(true_adj_pr_hosp = adj_pr_hosp,
             true_adj_los = adj_los,
             true_log_rate = log_importation_rate)
  ) %>%
  filter(threshold == max(threshold)) %>%
  
  ggplot() +
  
  # geom_histogram(aes(x = log_importation_rate)) +
  # geom_vline(aes(xintercept = true_log_rate), colour = "red") +
  # facet_wrap(~id)
  
  geom_point(aes(x = adj_los + adj_pr_hosp, y = log_importation_rate),
             size = 0.3, stroke = 0.3, alpha = 0.3) +
  
  geom_point(aes(x = true_adj_los + true_adj_pr_hosp, y = true_log_rate),
             colour = "red") +
  
  facet_wrap(~id)

