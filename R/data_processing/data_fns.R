get_forecast_dates <- function(local_cases_file,
                               state_modelled,
                               n_days_forward = 28) {
  local_cases <- read_csv(local_cases_file,
                          show_col_types = FALSE) %>%
    filter(state == state_modelled)
  
  date_minimum_onset <- local_cases %>%
    pull(date_onset) %>%
    min()
  
  date_last_onset_50 <- local_cases %>%
    filter(detection_probability > 0.5) %>%
    pull(date_onset) %>% max()
  
  date_last_infection_50 <- date_last_onset_50 - 5
  
  date_forecast_horizon = date_last_onset_50 + n_days_forward
  
  tibble(
    date_minimum_onset = date_minimum_onset,
    date_last_onset_50 = date_last_onset_50,
    date_last_infection_50 = date_last_infection_50,
    date_forecast_horizon = date_forecast_horizon,
  )
}