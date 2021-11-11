setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
library(tidyverse)
library(lubridate)

source("R/data_processing/data_fns.R")

## VIC

clinical_linelist_dir <- "/usr/local/forecasting/source/linelist_data/VIC/"
clinical_linelist_date <- ymd("2021-11-10")


clinical_linelist_source <- paste0(clinical_linelist_dir,
                                   format(clinical_linelist_date, "%Y%m%d"),
                                   "_Individual_Stay_Data.csv")

simulation_options <- make_simulation_options(
  run_name = paste0("VIC-test-", clinical_linelist_date),
  state_modelled = "VIC",
  
  n_trajectories = 50,
  n_samples_per_trajectory = 4,
  n_days_forward = 28,
  
  clinical_linelist_source = clinical_linelist_source
)


source("R/data_processing/mediaflux.R")
mf_dates <- download_latest_mediaflux_files(simulation_options)


source("R/data_processing/dropbox.R")
download_files(tibble(remote_file = "/covid_output/local_cases_input.csv",
                      local_file = simulation_options$files$local_cases))


update_c19data()


simulation_options$dates <- get_forecast_dates(
  simulation_options$files$local_cases,
  simulation_options$state_modelled,
  date_simulation_start = ymd("2021-06-01"),
  clinical_linelist_date = clinical_linelist_date,
  mf_dates,
  simulation_options$n_days_forward
)


source("R/data_processing/read_NNDSS.R")
process_NNDSS_linelist(simulation_options)


source("R/data_processing/read_vaccination.R")
process_vaccination_data(simulation_options)


source("R/model_parameters.R")
model_parameters <- get_model_parameters()

source("R/probability_hospitalisation.R")
make_clinical_prob_table(simulation_options,
                         model_parameters)


source("R/linelist_processing/read_VIC_linelist.R")
VIC_linelist <- read_VIC_linelist(simulation_options) %>%
  filter(dt_onset >= simulation_options$dates$simulation_start)
VIC_linelist %>% write_rds(simulation_options$files$clinical_linelist)








simulation_options$dates$linelist_cutoff <- simulation_options$dates$last_infection_50 - 14



source("R/produce_input_trajectories.R")

input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters)

#write_rds(input_trajectories, simulation_options$files$input_trajectories)

source("R/agent_based_model/run_simulations.R")

sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters)

#write_rds(sim_results, simulation_options$files$sim_results)

source("R/agent_based_model/plot_abm_results.R")

plot_abm_results(sim_results, simulation_options)

