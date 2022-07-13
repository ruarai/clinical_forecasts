library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))


# Do not change this value
longterm <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-07-08")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 30Jun2022.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2022-06-28.csv"),
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_75asc2022-06-14.csv"),
  
  #tar_target(models_included, c("gar", "moss", "dst")),
  tar_target(models_included, c("gar", "moss")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "TAS", "VIC")),
  tar_target(use_fitting, TRUE),
  
  tar_target(is_longterm, longterm),
  
  tar_target(do_upload_trajectories, FALSE)
)


source("t_dependencies.R")







t_forecast