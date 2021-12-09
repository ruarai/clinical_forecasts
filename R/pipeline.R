


## This is the primary script to run the clinical forecasts
## It performs a forecast over one state at a time (dependent upon "state_modelled")


setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
library(tidyverse)
library(lubridate)


source("R/data_processing/data_fns.R")

## Change these parameters depending upon what you're doing

# what state/territory (only NSW/VIC valid right now)
state_modelled <- "NSW" 
# What is the date (in the file name) of the state's clinical linelist
clinical_linelist_date <- ymd("2021-12-07")

# An additional name to track where our results go
run_label <- "validation"
# How many trajectories from our forecasting ensemble should we use? 1000 seems okay for real forecasting
n_trajectories <- 1000

# What LoS analysis do we use?
parameters_source_dir <- "results_length_of_stay/NSW-2021-11-25/"


## The results will be saved to /results/[state_modelled]-[run_label]-[clinical_linelist_date]/





simulation_options <- make_simulation_options(
  run_label = run_label,
  state_modelled = state_modelled,
  
  n_trajectories = n_trajectories,
  n_samples_per_trajectory = 4,
  n_days_forward = 28,
  
  clinical_linelist_date = clinical_linelist_date,
  
  ED_daily_queue_capacity = 3945,
  
  parameters_source_dir = parameters_source_dir
)


source("R/data_processing/mediaflux.R")
mflux_dates <- download_latest_mediaflux_files(simulation_options,
                                               date_limit = clinical_linelist_date)



simulation_options$dates <- get_forecast_dates(
  simulation_options$files$local_cases,
  simulation_options$state_modelled,
  
  date_simulation_start = ymd("2021-06-01"),
  
  clinical_linelist_date = clinical_linelist_date,
  backcast_cutoff_date = simulation_options$dates$last_onset_50,
  
  mflux_dates,
  simulation_options$n_days_forward
)




## Reading through the NNDSS linelist and performing basic cleaning
## This will be used later to produce the input case trajectories
source("R/data_processing/read_NNDSS.R")
process_NNDSS_linelist(simulation_options)


# Vaccination is not currently used, we can skip this.
# source("R/data_processing/read_vaccination.R")
# process_vaccination_data(simulation_options)


## Load in the model parameters from the "parameters_source_dir" defined above
## Where parameters_source_dir contains results from the length of stay analysis
source("R/model_parameters.R")
model_parameters <- get_model_parameters(simulation_options$dirs$parameters_source)


data.table::setDTthreads(threads = 1)

## Calculate our moving probabilities of hospitalization and ICU by age group
## This can take a few minutes
source("R/probability_hospitalisation.R")
make_clinical_prob_table(simulation_options,
                         model_parameters)

## Load in the state clinical occupancy linelist so we can plot our results against
## this in our reporting figures.
source("R/linelist_processing/read_NSW_linelist.R")
process_NSW_linelist(simulation_options)


## Upload the occupancy counts by ward/ICU (calculated from the above) to mediaflux
## so that other groups (UoA) can use them
source("R/data_processing/data_sharing.R")
make_timeseries_from_occupancy(simulation_options)
upload_mflux_sharing()


## Producing the input trajectories of cases that our ABM runs upon
source("R/produce_input_trajectories.R")
simulation_options$dates$backcast_cutoff <- simulation_options$dates$last_onset_50
input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters)

## Run the ABM and collect results
source("R/agent_based_model/run_simulations.R")
sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters)

## Save sim_results and options for later use if required
write_rds(sim_results, simulation_options$files$sim_results, compress = "gz")
write_rds(simulation_options, simulation_options$files$sim_options)


## Perform checks to make sure we're not producing obviously invalid outputs
## e.g. probabilities outside of [0, 1], consistent population counts
source("R/sanity_checks.R")
perform_sanity_checks(simulation_options, model_parameters,
                      input_trajectories, sim_results)

## Plot the results (saved to /results/[state_modelled]-[run_label]-[clinical_linelist_date]/)
source("R/agent_based_model/plot_abm_results.R")
plot_abm_results(sim_results, simulation_options)

