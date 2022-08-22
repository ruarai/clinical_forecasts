


run_progression_model <- function(
  case_trajectories,
  known_occupancy_ts,
  
  morbidity_trajectories_state,
  clinical_parameter_samples,
  
  forecast_dates,
  state_forecast_start,
  
  state_modelled,
  
  thresholds = c(0.1, 0.2, 0.3, 0.5, 1, 10, 1000),
  do_ABC = TRUE
) {
  
  require(tidyverse)
  require(lubridate)
  require(curvemush)
  
  
  date_seq <- seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days")
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  time_varying_estimates_input <- morbidity_trajectories_state %>%
    mutate(ix_age_group = match(age_group, age_groups) - 1,
           ix_sample = bootstrap - 1,
           ix_day = match(date, date_seq) - 1) %>%
    
    group_by(date, bootstrap) %>%
    mutate(pr_age_given_case = pr_age_given_case / sum(pr_age_given_case)) %>%
    ungroup()
  
  
  
  case_ensemble_input <- case_trajectories$curve_set %>%
    as_tibble() %>%
    mutate(date_onset = date_seq[1:nrow(case_trajectories$curve_set)], .before = 1) %>%
    
    complete(date_onset = date_seq) %>%
    fill(starts_with("sim"), .direction = "down") %>% 
    
    pivot_longer(starts_with("sim"), names_to = "sample", values_to = "cases") %>%
    arrange(sample, date_onset) %>% 
    mutate(ix_sample = match(sample, unique(sample)) - 1,
           ix_day = match(date_onset, date_seq) - 1)
  
  
  clinical_parameters_input <- clinical_parameter_samples %>%
    mutate(ix_sample = sample - 1,
           ix_age_group = match(age_group, age_groups) - 1)
  
  n_days <- length(date_seq)
  
  # Produce two vectors (for ward and ICU) of known occupancy to be fit to
  occupancy_input <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match = date > state_forecast_start - ddays(7) & date <= state_forecast_start) %>%
    
    left_join(
      known_occupancy_ts %>%
        select(date, group, count) %>%
        
        # Have to do this for... some reason
        group_by(date, group) %>%
        slice(1) %>%
        ungroup() %>%
        
        pivot_wider(names_from = group, values_from = count),
      
      by = "date") %>%
    
    mutate(ward_vec = if_else(do_match, ward, -1),
           ICU_vec = if_else(do_match, ICU, -1),
           
           ward_vec = replace_na(ward_vec, -1),
           ICU_vec = replace_na(ICU_vec, -1)) %>%
    
    mutate(ix_day = match(date, date_seq) - 1)
  
  
  print("Starting...")
  
  a <- Sys.time()
  
  if(do_ABC) {
    if(state_modelled == "NT") {
      state_min_threshold <- 0.1
    } else{
      state_min_threshold <- 0.05
    }
  } else{
    state_min_threshold <- 1
  }
  
  results <- curvemush::mush_abc_smc(
    case_ensemble = case_ensemble_input,
    time_varying_estimates = time_varying_estimates_input,
    clinical_parameters = clinical_parameters_input,
    known_occupancy = occupancy_input,
    
    thresholds_vec = seq(1, state_min_threshold, by = -0.05),
    n_particles = 1000,
    n_steps_per_day = 4,
    n_days = n_days,
    n_delay_samples = 512
  )
  
  b <- Sys.time()
  
  print(str_c("Simulation mush ran in ", round(b - a, 2), " ", units(b - a)))
  
  source("R/make_result_quants.R")
  
  group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
  compartment_labels <- c(
    "symptomatic_clinical", "ward", "discharged_ward", "died_ward", "ICU",
    "discharged_ICU", "died_ICU", "postICU_to_discharge", "postICU_to_death",
    "discharged_postICU", "died_postICU"
  )
  
  format_grouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           group = group_labels[compartment_group + 1])
  
  format_ungrouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           compartment = compartment_labels[compartment + 1])
  
  
  
  results_count_quants <- results$grouped_results %>%
    select(-transitions) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_count_quants <- results$results %>%
    select(-transitions) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  results_transitions_quants <- results$grouped_results %>%
    select(-count) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_transitions_quants <- results$results %>%
    select(-count) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  
  results_formatted <- results$grouped_results  %>%
    format_grouped()
  
  results_ungrouped_formatted <- results$results  %>%
    format_ungrouped()
  
  
  # posterior_data <- tibble(
  #   pr_hosp_scale = results$prior_pr_hosp[results$prior_chosen + 1],
  #   los_scale = results$prior_los_scale[results$prior_chosen + 1],
  #   source = "posterior"
  # )
  # 
  # prior_data <- tibble(
  #   los_scale = rnorm(1000, sd = prior_sigma_los),
  #   pr_hosp_scale = rnorm(1000, sd = prior_sigma_hosp),
  #   source = "prior"
  # )
  
  #ABC_parameters <- bind_rows(posterior_data, prior_data)
  
  list(
    trajectories = results_formatted,
    quants_count = results_count_quants,
    quants_transitions = results_transitions_quants,
    
    trajectories_ungrouped = results_ungrouped_formatted,
    quants_ungrouped_count = results_ungrouped_count_quants,
    quants_ungrouped_transitions = results_ungrouped_transitions_quants#,
    
    #ABC_fit_diagnostics = tibble(thresholds = thresholds, accepted = results$n_accepted),
    #ABC_parameters = ABC_parameters
  )
}


