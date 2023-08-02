


run_progression_model <- function(
    case_trajectories,
    known_occupancy_ts,
    
    morbidity_trajectories_state,
    clinical_parameters,
    
    forecast_dates,
    state_forecast_start,
    
    state_modelled
) {
  
  
  Sys.setenv(
    JULIA_PROJECT="/home/forecast/source/stochastic_progression/StochasticProgression/",
    JULIA_NUM_THREADS = 32
  )
  
  require(JuliaCall)
  
  case_curves <- case_trajectories$curve_set
  n_steps_per_day <- 4
  
  
  occupancy_curve_match <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match_wide = date > state_forecast_start - days(120) & date <= state_forecast_start + days(7),
           do_match_refine = date > state_forecast_start & date <= state_forecast_start + days(7)) %>%
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
    
    mutate(ward_vec_wide = replace_na(if_else(do_match_wide, ward, -1), -1),
           ICU_vec_wide = replace_na(if_else(do_match_wide, ICU, -1), -1),
           ward_vec_refine = replace_na(if_else(do_match_refine, ward, -1), -1),
           ICU_vec_refine = replace_na(if_else(do_match_refine, ICU, -1), -1),
           obs_weight = if_else(do_match_refine, 4.0, 1.0))
  
  
  morbidity_trajectories_state_ix <- morbidity_trajectories_state %>%
    mutate(t = match(date, unique(date)))
  
  
  julia_source("../stochastic_progression/dependencies.jl")
  
  rates <- seq(-10, -6, by = 0.5)
  
  results_loose <- julia_call(
    "run_inference_loose",
    
    case_trajectories$n_days,
    4,
    
    1000,
    4,
    0.25,
    100,
    
    case_curves,
    clinical_parameters,
    morbidity_trajectories_state_ix,
    
    cbind(occupancy_curve_match$ward_vec_wide, occupancy_curve_match$ICU_vec_wide),
    occupancy_curve_match$obs_weight
  )
  
  prior_parameters <- results_loose$parameters %>%
    filter(threshold == max(threshold))
  
  results_refined <- julia_call(
    "run_inference_refine",
    
    case_trajectories$n_days,
    4,
    
    1000,
    c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 10.0),
    0.0004,
    
    case_curves,
    clinical_parameters,
    morbidity_trajectories_state_ix,
    
    prior_parameters,
    
    cbind(occupancy_curve_match$ward_vec_refine, occupancy_curve_match$ICU_vec_refine)
  )
  
  trajectories <- results_refined %>%
    select(sample, day, sim_ward, sim_ward_outbreak, sim_ICU) %>%
    pivot_longer(c(sim_ward, sim_ward_outbreak, sim_ICU),
                 names_to = "group",
                 values_to = "count",
                 names_prefix = "sim_") %>%
    mutate(date = forecast_dates$simulation_start + days(day - 1), .before = 1)

  source("R/make_result_quants.R")

  results_count_quants <- trajectories %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants()

  sim_results <- list(
    results_refined = results_refined,
    results_loose = results_loose,
    quants_count = results_count_quants,
    trajectories = trajectories
  )
  
  return(sim_results)
}





