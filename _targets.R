library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

# Do not change this value
is_retro <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2023-10-13")),
  
  
  tar_target(date_simulation_start, date_forecasting - days(28 * 6)),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 6months-12Oct2023.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2023-10-12.csv"),
  
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/hyndman_ensemble_paths_2023-10-06.parquet"),
  tar_target(occupancy_path, "data/occupancy/NAT_2023-10-12_Data for Uni of Melbourne.xlsx"),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  tar_target(pcr_only_states, c("VIC", "QLD", "NSW")),
  
  tar_target(do_upload_trajectories, FALSE),
  

  tar_target(n_traj_out, 1000),
  tar_target(use_fitting, TRUE)
)


source("t_dependencies.R")



t_forecast