make_case_trajectories_oracle <- function(
    local_cases_latest,
    local_cases_state,
    
    forecast_dates,
    state_forecast_start
) {
  
  ## Forecasting
  
  # Transform the ensemble data CSV into a matrix of ~8,000 columns, 28 rows
  
  oracle_forecast_cases <- local_cases_latest %>% 
    filter(state == local_cases_state$state[1],
           date_onset > state_forecast_start,
           date_onset <= forecast_dates$forecast_horizon) %>%
    select(date_onset, count) %>%
    arrange(date_onset)
  
  n_curves <- 1
  
  ensemble_curves <- matrix(oracle_forecast_cases$count, 
                            nrow = nrow(oracle_forecast_cases),
                            ncol = n_curves)
  
  
  
  
  ## Nowcasting
  
  
  date_nowcast_start <- forecast_dates$simulation_start
  
  
  local_cases_to_impute <- local_cases_state %>%
    filter(date_onset >= date_nowcast_start,
           date_onset <= state_forecast_start) %>%
    select(date_onset, count, detection_probability)
  
  nowcasting_case_curves <- local_cases_to_impute$count %>%
    matrix(nrow = length(.), ncol = n_curves)
  
  detection_probability <- local_cases_to_impute$detection_probability
  
  nowcasting_case_curves_imputed <- apply(
    nowcasting_case_curves,
    2,
    function(x) rnbinom(nrow(nowcasting_case_curves), size = 25, mu = x / detection_probability)
  )
  
  
  final_curve_set <- rbind(
    nowcasting_case_curves_imputed,
    
    ensemble_curves
  )
  
  # At what time step do we need to switch from direct case input to sampling?
  step_sampling_start <- 0
  n_days <- nrow(nowcasting_case_curves_imputed) + nrow(ensemble_curves) + step_sampling_start
  
  list(
    curve_set = final_curve_set,
    step_sampling_start = step_sampling_start,
    
    n_days = n_days
  )
}