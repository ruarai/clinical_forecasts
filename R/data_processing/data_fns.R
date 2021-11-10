get_forecast_dates <- function(local_cases_file,
                               state_modelled,
                               date_simulation_start,
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
    minimum_onset = date_minimum_onset,
    last_onset_50 = date_last_onset_50,
    last_infection_50 = date_last_infection_50,
    forecast_horizon = date_forecast_horizon,
    simulation_start = date_simulation_start
  )
}

update_c19data <- function() {
  covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
  read_csv(covid19data_url) %>% write_rds("data/covid19data.rds")
}
