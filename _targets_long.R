library(targets)
library(tarchetypes)


library(future.callr)
plan(callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE)

state_tbl <- tibble::tibble(state_modelled = c("TAS", "VIC", "ACT", "WA", "SA", "NT", "NSW", "QLD"))
#state_tbl <- tibble::tibble(state_modelled = c("NSW"))

# Do not change this value
longterm <- TRUE 

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-08-17")),
  
  
  tar_target(date_simulation_start, ymd("2021-12-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final_longterm")),
  
  # Update these to the latest file path
  # /home/forecast/mfluxshared and /home/forecast/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 16Aug2022.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2022-08-16.csv"),
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_75asc2022-08-09.csv"),
  
  tar_target(quantium_zip_path, "~/mfluxunimelb/vaccine_data_products/raw_quantium_forecasts/2022-07-25-linen5332-booster-uptake-scenarios.zip"),
  
  
  
  tar_target(models_included, c("dst", "moss")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  tar_target(use_fitting, FALSE),
  
  tar_target(is_longterm, longterm),
  
  
  tar_target(do_upload_trajectories, FALSE)
)


source("t_dependencies.R")



t_forecast