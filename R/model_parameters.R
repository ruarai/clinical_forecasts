


make_forecast_dates <- function(
  date_file_limit,
  date_simulation_start,
  
  local_cases_file,
  
  latest_mflux_files
) {
  local_cases <- read_csv(local_cases_file, show_col_types = FALSE)
  
  
  date_forecast_start <- local_cases %>%
    filter(state == "NSW", detection_probability > 0.95) %>%
    pull(date_onset) %>% max()
  
  date_forecast_horizon <- date_forecast_start + ddays(28)
  
  mf_dates_wide <- latest_mflux_files %>% 
    bind_rows() %>%
    select(type, date) %>% 
    pivot_wider(names_from = type, values_from = date)
  
  
  bind_cols(
    tibble(
      file_limit = date_file_limit,
      simulation_start = date_simulation_start,
      
      forecast_start = date_forecast_start,
      forecast_horizon = date_forecast_horizon
    ),
    
    mf_dates_wide
  )
  
  
}