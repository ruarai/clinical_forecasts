


run_progression_model <- function(
    case_trajectories,
    known_occupancy_ts,
    
    morbidity_trajectories_state,
    clinical_parameter_samples,
    
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
  )  %>%
    mutate(do_match = date > state_forecast_start - days(90)) %>%
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
           ICU_vec = replace_na(ICU_vec, -1)) %>%
    mutate(t = row_number())
  
  
  morbidity_trajectories_state_ix <- morbidity_trajectories_state %>%
    mutate(t = match(date, unique(date)))
  
  
  julia_source(
    "../stochastic_progression/inference_pf.jl"
  )
  
  
  results <- julia_call(
    "run_inference",
    case_trajectories$n_days,
    n_steps_per_day,
    8000,
    
    case_curves,
    clinical_parameter_samples,
    morbidity_trajectories_state_ix,
    
    cbind(occupancy_curve_match$ward_vec, occupancy_curve_match$ICU_vec)
  )
  
  
  
  
  trajectories <- results %>%
    select(sample = particle, day, sim_ward, sim_ward_outbreak, sim_ICU) %>%
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
    results_tbl = results,
    quants_count = results_count_quants,
    trajectories = trajectories
  )
}





