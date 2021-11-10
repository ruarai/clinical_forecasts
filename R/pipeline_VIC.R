library(tidyverse)
library(lubridate)


source("R/linelist_processing/read_VIC_linelist.R")
VIC_linelist <- read_VIC_linelist("2021-11-01")
write_rds(VIC_linelist, "data/processed/clinical_linelist_VIC.rds")

source("R/model_parameters.R")
model_parameters <- get_model_parameters()


## VIC
simulation_options <- list(
  n_trajectories = 500,
  n_samples_per_trajectory = 4,
  
  date_simulation_start = ymd("2021-06-01"),
  n_days_forward = 28,
  
  run_name = "VIC-test-2021-11-01",
  
  state_modelled = "VIC",
  
  files = list(
    local_cases = "data/input/local_cases_input.csv",
    
    vacc_prob_table = "data/processed/vaccination_probability_table.rds",
    clinical_prob_table = "data/processed/clinical_probabilities.rds",
    
    clinical_linelist = "data/processed/clinical_linelist_VIC.rds",
    NNDSS_linelist = "data/processed/linelist_NNDSS.rds",
    
    ensemble_samples = "data/input/ensemble_samples.csv"
  )
)

simulation_options$dirs = list(
  plots = paste0("results/",simulation_options$run_name, "/plots"),
  data = paste0("results/",simulation_options$run_name, "/data/")
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

library(future)
library(future.callr)
library(furrr)
plan(callr(workers = 16))
sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters,
                               forecast_dates)

source("R/agent_based_model/plot_abm_results.R")

plot_abm_results(sim_results, simulation_options, forecast_dates)
