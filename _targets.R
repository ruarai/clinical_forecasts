library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

# Do not change this value
is_retro <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2023-01-06")),
  
  
  tar_target(date_simulation_start, ymd("2022-05-25")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_test")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 6months-05Jan2023.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2023-01-05.csv"),
  
  ## NOTE - Moss ensemble models are being downweighted - so message "Dropping 2000 columns for being entirely NA" will appear repeatedly
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_varasc2022-12-30.csv"),
  tar_target(occupancy_path, "data/occupancy/NAT_2023-01-05_Data for Uni of Melbourne.xlsx"),
  
  tar_target(models_included, c("gar", "moss_varasc", "dst_new", "moss_varasc_unsmoothed")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "ACT", "VIC")),
  
  tar_target(do_upload_trajectories, FALSE),
  

  tar_target(n_traj_out, 1000),
  tar_target(use_fitting, TRUE)
)


source("t_dependencies.R")



t_forecast