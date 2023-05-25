


run_progression_model <- function(
  case_trajectories,
  known_occupancy_ts,
  
  morbidity_trajectories_state,
  clinical_parameter_samples,
  
  forecast_dates,
  state_forecast_start,
  
  state_modelled,
  n_traj_out = 1000,
  
  thresholds = c(0.1, 0.2, 0.3, 0.5, 1, 10, 1000),
  do_ABC = TRUE
) {
  
  require(tidyverse)
  require(lubridate)
  require(curvemush)
  
  estimates_to_matrix <- function(x, variable) {
    col <- deparse(substitute(variable))
    
    x %>%
      select(date, bootstrap, age_group, all_of(col)) %>%
      pivot_wider(names_from = bootstrap,
                  values_from = all_of(col)) %>%
      
      arrange(date, age_group) %>%
      select(-c(date, age_group)) %>%
      as.matrix()
  }
  
  mat_pr_age_given_case <- morbidity_trajectories_state %>%
    group_by(date, bootstrap) %>%
    mutate(pr_age_given_case = pr_age_given_case / sum(pr_age_given_case)) %>%
    ungroup() %>%
    estimates_to_matrix(pr_age_given_case)
  
  
  mat_pr_hosp <- morbidity_trajectories_state %>%
    estimates_to_matrix(pr_hosp)
  
  mat_pr_ICU <- morbidity_trajectories_state %>%
    estimates_to_matrix(pr_ICU)
  
  
  
  occupancy_curve_match <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match = date > state_forecast_start & date <= state_forecast_start + days(7)) %>%
    
    left_join(
      
      known_occupancy_ts %>%
        filter(date >= forecast_dates$simulation_start) %>%
        select(date, group, count) %>%
        
        # Have to do this for... some reason
        group_by(date, group) %>%
        slice(1) %>%
        ungroup() %>%
        
        pivot_wider(names_from = group, values_from = count)
      
    ) %>%
    
    mutate(ward_vec = if_else(do_match, ward, -1),
           ward_vec = replace_na(ward_vec, -1),
           ICU_vec = if_else(do_match, ICU, -1),
           ICU_vec = replace_na(ICU_vec, -1))
  
  
  print("Starting...")
  
  a <- Sys.time()
  
  if(do_ABC) {
    if(state_modelled == "NT") {
      prior_sigma_los <- 2
      prior_sigma_hosp <- 2
    } else if(state_modelled == "NSW") {
      prior_sigma_los <- 0.8
      prior_sigma_hosp <- 0.2
    } else{
      prior_sigma_los <- 0.5
      prior_sigma_hosp <- 0.8
    }
  } else{
    prior_sigma_los <- 0
    prior_sigma_hosp <- 0
  }
  
  results <- curvemush::mush_abc(
    n_samples = 4000,
    n_delay_samples = 512,
    
    n_outputs = n_traj_out,
    
    n_days = case_trajectories$n_days,
    steps_per_day = 4,
    
    thresholds_vec = thresholds,
    rejections_per_selections = 4000,
    do_ABC = do_ABC,

    prior_sigma_los = prior_sigma_los,
    prior_sigma_hosp = prior_sigma_hosp,
   
    
    ensemble_curves = case_trajectories$curve_set,
    
    forecasting_parameters = clinical_parameter_samples,
    
    known_ward_vec = occupancy_curve_match$ward_vec,
    known_ICU_vec = occupancy_curve_match$ICU_vec,
    
    mat_pr_age_given_case = mat_pr_age_given_case,
    mat_pr_hosp = mat_pr_hosp,
    mat_pr_ICU = mat_pr_ICU
    
    #delay_parameters = gengamma_delay
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
  
  
  posterior_data <- tibble(
    pr_hosp_scale = results$prior_pr_hosp[results$prior_chosen + 1],
    los_scale = results$prior_los_scale[results$prior_chosen + 1],
    source = "posterior"
  )
  
  prior_data <- tibble(
    los_scale = rnorm(1000, sd = prior_sigma_los),
    pr_hosp_scale = rnorm(1000, sd = prior_sigma_hosp),
    source = "prior"
  )
  
  ABC_parameters <- bind_rows(posterior_data, prior_data)
  
  list(
    trajectories = results_formatted,
    quants_count = results_count_quants,
    quants_transitions = results_transitions_quants,
    
    trajectories_ungrouped = results_ungrouped_formatted,
    quants_ungrouped_count = results_ungrouped_count_quants,
    quants_ungrouped_transitions = results_ungrouped_transitions_quants,
    
    ABC_fit_diagnostics = tibble(thresholds = thresholds, accepted = results$n_accepted),
    ABC_parameters = ABC_parameters,
    
    prior_chosen = results$prior_chosen
  )
}


