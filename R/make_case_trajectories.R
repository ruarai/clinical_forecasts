
make_case_trajectories <- function(
  ensemble_state,
  local_cases_state,
  
  forecast_dates,
  state_forecast_start
) {
  
  ## Forecasting
  
  # Transform the ensemble data CSV into a matrix of ~8,000 columns, 28 rows
  
  ensemble_curves_df <- ensemble_state %>%
    filter(date <= forecast_dates$forecast_horizon,
           date > state_forecast_start) %>%
    
    select(-c(state, forecast_origin)) %>%
    
    mutate(across(starts_with("sim"), ~ round(.))) %>%
    
    arrange(date) %>%
    select(-date)
  
  
  na_cols <- map_lgl(1:ncol(ensemble_curves_df), ~ all(is.na(ensemble_curves_df[,.])))
  
  print(paste0("Dropping ", sum(na_cols), " columns for being entirely NA. This is okay :)"))
  
  
  ensemble_curves <- as.matrix(ensemble_curves_df[, !na_cols])
  
  # Down-sample to 2,000 curves
  ensemble_curves <- ensemble_curves[,sample(1:ncol(ensemble_curves), 2000)] 
  
  n_curves <- ncol(ensemble_curves)
  
  
  
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
  
  if(any(is.na(final_curve_set))) {
    print("NA values in case_trajectories, imputing...")
    
    final_curve_set <- apply(final_curve_set, 2, function(x) zoo::na.locf(x))
  }
  
  
  list(
    curve_set = final_curve_set,
    step_sampling_start = step_sampling_start,
    
    n_days = n_days
  )
}

