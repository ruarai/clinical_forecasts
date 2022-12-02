library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

# Do not change this value
longterm <- FALSE
is_retro <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-12-02")),
  
  
  tar_target(date_simulation_start, ymd("2022-06-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final2")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  #tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM "),
  tar_target(raw_nindss, "~/mfluxunimelb/NINDSS_and_state_data/nindss/COVID-19 UoM 01Dec2022.csv"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2022-12-01.csv"),
  
  ## NOTE - Moss ensemble models are being downweighted - so message "Dropping 2000 columns for being entirely NA" will appear repeatedly
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_50asc2022-11-26.csv"),
  tar_target(occupancy_path, "data/occupancy/NAT_2022-12-02_Data for Uni of Melbourne.xlsx"),
  
  tar_target(models_included, c("gar", "moss", "dst_new", "moss_unsmoothed")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "ACT")),
  
  tar_target(do_upload_trajectories, TRUE),
  

  tar_target(use_fitting, TRUE),
  tar_target(is_longterm, longterm)
)


source("t_dependencies.R")



t_forecast