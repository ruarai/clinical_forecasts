setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")

source("R/data_processing/mediaflux.R")

sync_latest_mediaflux_vacc()

# May want to overwrite these if they get it wrong
latest_ensemble_file <- get_latest_ensemble_file()
latest_nndss_file <- get_latest_NNDSS_file()

mf_files <- tribble(
  ~remote_file, ~local_file,
  paste0("forecast-outputs/", latest_ensemble_file), "data/input/ensemble_samples.csv",
  paste0("Health Uploads/", latest_nndss_file),      "data/input/NNDSS.xlsx",
)
download_mediaflux_files(mf_files)


source("R/data_processing/dropbox.R")
download_files(tibble(remote_file = "/covid_output/local_cases_input.csv",
                      local_file = "data/input/local_cases_input.csv"))



covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
read_csv(covid19data_url) %>% write_rds("data/covid19data.rds")



source("R/data_processing/read_NNDSS.R")
source("R/data_processing/read_vaccination.R")
process_NNDSS_linelist(minimum_case_date = ymd("2021-06-01"))

source("R/linelist_processing/read_NSW_linelist.R")
NSW_linelist <- read_NSW_linelist("/usr/local/forecasting/source/linelist_data/NSW/NSW_out_episode_201021.xlsx")
write_rds(NSW_linelist, "data/processed/clinical_linelist_NSW.rds")


process_vaccination_data()


source("R/model_parameters.R")
model_parameters <- get_model_parameters()


## NSW
simulation_options <- list(
  n_trajectories = 50,
  n_samples_per_trajectory = 4,
  
  date_simulation_start = ymd("2021-06-01"),
  n_days_forward = 28,
  
  run_name = "NSW-test-2021-10-20",
  
  state_modelled = "NSW",
  
  files = list(
    local_cases = "data/input/local_cases_input.csv",
    
    vacc_prob_table = "data/processed/vaccination_probability_table.rds",
    clinical_prob_table = "data/processed/clinical_probabilities.rds",
    
    clinical_linelist = "data/processed/clinical_linelist_NSW.rds",
    NNDSS_linelist = "data/processed/linelist_NNDSS.rds",
    
    ensemble_samples = "data/input/ensemble_samples.csv"
  )
)

simulation_options$dirs = list(
  plots = paste0("results/",simulation_options$run_name, "/plots"),
  data = paste0("results/",simulation_options$run_name, "/data")
)

map(simulation_options$dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))

source("R/data_processing/data_fns.R")

forecast_dates <- get_forecast_dates(simulation_options$files$local_cases,
                                     simulation_options$state_modelled,
                                     simulation_options$n_days_forward)

source("R/produce_input_trajectories.R")

input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters,
                                                 forecast_dates)

source("R/agent_based_model/run_simulations.R")

sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters,
                               forecast_dates)

source("R/agent_based_model/plot_abm_results.R")

plot_abm_results(sim_results, simulation_options, forecast_dates)

