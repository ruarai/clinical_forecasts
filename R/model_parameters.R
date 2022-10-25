


make_forecast_dates <- function(
  date_file_limit,
  date_simulation_start,
  
  local_cases_file,
  
  nindss_path,
  
  is_longterm
) {
  local_cases <- read_csv(local_cases_file, show_col_types = FALSE) %>%
    rename(detection_probability = completion_probability)
  
  
  date_forecast_start <- local_cases %>%
    filter(detection_probability > 0.95)
  
  days_horizon <- if_else(is_longterm, 30 * 6, 28)
  
  date_forecast_horizon <- max(date_forecast_start$date_onset) + ddays(days_horizon)
  
  if(tools::file_ext(nindss_path) == "fst"){
    nindss_date <- ymd(str_extract(nindss_path, "\\d{4}-\\d{2}-\\d{2}"))
  } else{
    nindss_date <- dmy(str_extract(nindss_path, "\\d{2}.{3}\\d{4}"))
  }
  
  tibble(
      file_limit = date_file_limit,
      simulation_start = date_simulation_start,
      
      forecast_horizon = date_forecast_horizon,
      NNDSS = nindss_date
    )
}

get_state_forecast_start <- function(
  local_cases_state,
  state_modelled
) {
  date <- local_cases_state %>%
    filter(detection_probability > 0.95) %>%
    pull(date_onset) %>% max()
  
  if(state_modelled == "QLD") {
    return(date - ddays(2))
  } else{
    return(date)
  }
}

