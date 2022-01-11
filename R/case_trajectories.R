
make_case_trajectories <- function(
  ensemble_state,
  local_cases_state,
  
  linelist_state,
  linelist_state_date,
  
  forecast_dates
) {
  
  ## Forecasting
  
  # Transform the ensemble data CSV into a matrix of ~8,000 columns, 28 rows
  
  ensemble_curves_df <- ensemble_state %>%
    filter(date <= forecast_dates$forecast_horizon) %>%
    select(-c(state, forecast_origin)) %>%
    
    pivot_wider(names_from = ".model",
                values_from = starts_with("sim")) %>%
    
    mutate(across(starts_with("sim"), ~ round(.))) %>%
    
    arrange(date) %>%
    select(-date)
  
  
  na_cols <- map_lgl(1:ncol(ensemble_curves_df), ~ all(is.na(ensemble_curves_df[,.])))
  
  print(paste0("Dropping ", sum(na_cols), " columns for being entirely NA"))
  
  
  ensemble_curves <- as.matrix(ensemble_curves_df[, !na_cols])
  n_curves <- ncol(ensemble_curves)
  
  
  
  ## Nowcasting
  
  
  date_nowcast_start <- linelist_state_date - ddays(21)
  
  
  local_cases_to_impute <- local_cases_state %>%
    filter(date_onset > date_nowcast_start,
           date_onset <= forecast_dates$forecast_start) %>%
    select(date_onset, count, detection_probability)
  
  nowcasting_case_curves <- local_cases_to_impute$count %>%
    matrix(nrow = length(.), ncol = n_curves)
  
  detection_probability <- local_cases_to_impute$detection_probability
  
  nowcasting_case_curves_imputed <- apply(
    nowcasting_case_curves,
    2,
    function(x) rnbinom(nrow(nowcasting_case_curves), size = 25, mu = x / detection_probability)
  )
  
  
  ## Backcasting
  
  
  backcast_days <- seq(forecast_dates$simulation_start,
                       date_nowcast_start,
                       by = "days")
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  backcast_curve <- linelist_state %>%
    filter(date_onset <= date_nowcast_start) %>%
    filter(ever_in_hospital) %>%
    
    group_by(date_onset, age_group) %>%
    summarise(n_hosp = n(), .groups = "drop") %>%
    complete(date_onset = backcast_days, age_group = age_groups, fill = list(n_hosp = 0)) %>%
    
    arrange(date_onset, age_group)
  
  
  backcast_curves <- backcast_curve$n_hosp %>%
    matrix(nrow = length(backcast_curve$n_hosp), ncol = n_curves)
  
  
  
  final_curve_set <- rbind(
    backcast_curves,
    
    nowcasting_case_curves_imputed,
    
    ensemble_curves
  )
  
  # At what time step do we need to switch from direct case input to sampling?
  step_sampling_start <- nrow(backcast_curves) / 9
  n_days <- nrow(nowcasting_case_curves_imputed) + nrow(ensemble_curves) + step_sampling_start
  
  
  list(
    curve_set = final_curve_set,
    step_sampling_start = step_sampling_start,
    
    n_days = n_days
  )
}

