


make_forecast_dates <- function(
  date_file_limit,
  date_simulation_start,
  
  local_cases_file,
  
  latest_mflux_files
) {
  local_cases <- read_csv(local_cases_file, show_col_types = FALSE)
  
  
  date_forecast_start <- local_cases %>%
    filter(detection_probability > 0.95)
  
  date_forecast_horizon <- max(date_forecast_start$date_onset) + ddays(28)
  
  mf_dates_wide <- latest_mflux_files %>% 
    bind_rows() %>%
    select(type, date) %>% 
    pivot_wider(names_from = type, values_from = date)
  
  
  bind_cols(
    tibble(
      file_limit = date_file_limit,
      simulation_start = date_simulation_start,
      
      forecast_horizon = date_forecast_horizon
    ),
    
    mf_dates_wide
  )
}

get_state_forecast_start <- function(
  local_cases_state,
  state_modelled
) {
  if(state_modelled == "VIC") {
    ymd("2022-04-11")
  } else {
    local_cases_state %>%
      filter(detection_probability > 0.95) %>%
      pull(date_onset) %>% max()
  }
  
}