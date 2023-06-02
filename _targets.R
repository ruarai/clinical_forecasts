library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

# Do not change this value
is_retro <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2023-06-02")),
  
  
  tar_target(date_simulation_start, ymd("2022-12-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 6months-01June2023.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2023-06-01.csv"),
  
  ## NOTE - Moss ensemble models are being downweighted - so message "Dropping 1000 columns for being entirely NA" will appear repeatedly
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_varasc2023-05-26.csv"),
  tar_target(occupancy_path, "data/occupancy/NAT_2023-06-01_Data for Uni of Melbourne.xlsx"),
  
  tar_target(models_included, c("gar", "moss_varasc", "dst_new", "moss_varasc_unsmoothed", "dst_behave")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "ACT")),
  
  tar_target(do_upload_trajectories, FALSE),
  

  tar_target(n_traj_out, 1000),
  tar_target(use_fitting, TRUE)
)


source("t_dependencies.R")



t_forecast