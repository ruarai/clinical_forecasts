setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
library(tidyverse)
library(lubridate)

source("R/data_processing/data_fns.R")

## NSW

clinical_linelist_dir <- "/usr/local/forecasting/linelist_data/NSW/"
clinical_linelist_date <- ymd("2021-11-08")

clinical_linelist_source <- paste0(clinical_linelist_dir,
                                   "NSW_out_episode_",
                                   format(clinical_linelist_date, "%d%m%y"),
                                   ".xlsx")

simulation_options <- make_simulation_options(
  run_name = paste0("NSW-test-", clinical_linelist_date),
  state_modelled = "NSW",
  
  n_trajectories = 100,
  n_samples_per_trajectory = 4,
  n_days_forward = 28,
  
  ED_daily_queue_capacity = 3945,
  
  clinical_linelist_source = clinical_linelist_source,
  
  parameters_source_dir = "results_length_of_stay/NSW-2021-11-08/"
)


source("R/data_processing/mediaflux.R")
mf_dates <- download_latest_mediaflux_files(simulation_options,
                                            date_limit = clinical_linelist_date)



update_c19data()


simulation_options$dates <- get_forecast_dates(
  simulation_options$files$local_cases,
  simulation_options$state_modelled,
  date_simulation_start = ymd("2021-06-01"),
  mf_dates,
  simulation_options$n_days_forward
)


source("R/data_processing/read_NNDSS.R")
process_NNDSS_linelist(simulation_options)


source("R/data_processing/read_vaccination.R")
process_vaccination_data(simulation_options)


source("R/model_parameters.R")
model_parameters <- get_model_parameters(simulation_options$dirs$parameters_source)

source("R/probability_hospitalisation.R")
make_clinical_prob_table(simulation_options,
                         model_parameters)


source("R/linelist_processing/read_NSW_linelist.R")
process_NSW_linelist(simulation_options)



simulation_options$dates$linelist_cutoff <- simulation_options$dates$last_infection_50

source("R/produce_input_trajectories.R")

input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters)

#write_rds(input_trajectories, simulation_options$files$input_trajectories)

source("R/agent_based_model/run_simulations.R")

sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters)

#write_rds(sim_results, simulation_options$files$sim_results)

source("R/sanity_checks.R")
perform_sanity_checks(simulation_options, model_parameters,
                      input_trajectories, sim_results)

source("R/agent_based_model/plot_abm_results.R")

plot_abm_results(sim_results, simulation_options)

