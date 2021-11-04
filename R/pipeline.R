
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
process_NNDSS_linelist(minimum_case_date = ymd("2020-01-01"))

NSW_linelist <- read_NSW_linelist("/usr/local/forecasting/source/linelist_data/NSW/NSW_out_episode_011121.xlsx")
write_rds(NSW_linelist, "data/processed/clinical_linelist_NSW.rds")

process_vaccination_data()


source("R/model_parameters.R")
model_parameters <- get_model_parameters()


## NSW
simulation_options <- list(
  n_trajectories = 10,
  n_samples_per_trajectory = 4,
  
  date_simulation_start = ymd("2021-06-01"),
  n_days_forward = 28,
  
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
source("R/produce_input_trajectories.R")

input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters)







