make_simulation_options <- function(run_name,
                                    state_modelled,
                                    n_trajectories,
                                    n_samples_per_trajectory,
                                    ED_daily_queue_capacity,
                                    n_days_forward = 28,
                                    
                                    clinical_linelist_source,
                                    
                                    parameters_source_dir) {
  simulation_options <- list(
    n_trajectories = n_trajectories,
    n_samples_per_trajectory = n_samples_per_trajectory,
    n_days_forward = n_days_forward,
    
    ED_daily_queue_capacity = ED_daily_queue_capacity,
    
    run_name = run_name,
    
    state_modelled = state_modelled
  )
  
  
  simulation_options$dirs <- list(
    plots = paste0("results/",simulation_options$run_name, "/plots/"),
    data = paste0("results/",simulation_options$run_name, "/data/"),
    input = paste0("results/",simulation_options$run_name, "/input/")
  )
  
  map(simulation_options$dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
  
  simulation_options$dirs$parameters_source <- parameters_source_dir
  
  
  
  simulation_options$files <- list(
    local_cases = paste0(simulation_options$dirs$input, "local_cases_input.csv"),
    
    vacc_prob_table = paste0(simulation_options$dirs$data, "vaccination_probability_table.rds"),
    vacc_raw = paste0(simulation_options$dirs$input, "effective_dose_data.csv"),
    clinical_prob_table = paste0(simulation_options$dirs$data, "clinical_probabilities.rds"),
    
    clinical_linelist = paste0(simulation_options$dirs$input, "clinical_linelist.rds"),
    clinical_linelist_source = clinical_linelist_source,
    
    NNDSS_linelist = paste0(simulation_options$dirs$input, "linelist_NNDSS.rds"),
    NNDSS_raw = paste0(simulation_options$dirs$input, "NNDSS.xlsx"),
    
    ensemble_samples = paste0(simulation_options$dirs$input, "ensemble_samples.csv"),
    
    
    input_trajectories = paste0(simulation_options$dirs$data, "input_trajectories.rds"),
    sim_results = paste0(simulation_options$dirs$data, "sim_results.rds")
    
  )
  
  simulation_options
}


get_forecast_dates <- function(local_cases_file,
                               state_modelled,
                               date_simulation_start,
                               mf_dates,
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
  
  mf_dates_wide <- mf_dates %>% 
    select(type, date) %>% 
    pivot_wider(names_from = type, values_from = date)
  
  tibble(
    minimum_onset = date_minimum_onset,
    last_onset_50 = date_last_onset_50,
    last_infection_50 = date_last_infection_50,
    forecast_horizon = date_forecast_horizon,
    simulation_start = date_simulation_start,
    mf_dates_wide
  )
}

update_c19data <- function() {
  covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
  read_csv(covid19data_url,
           show_col_types = FALSE) %>% 
    write_rds("data/covid19data.rds")
}
