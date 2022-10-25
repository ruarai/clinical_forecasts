library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

# Do not change these values
longterm <- FALSE
is_retro <- TRUE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-08-17")),
  
  
  tar_target(date_simulation_start, ymd("2021-12-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_retro2_oracle")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "results/fc_2022-08-17_final/archive/nindss.fst"),
  tar_target(raw_local_cases, "results/fc_2022-08-17_final/archive/local_cases.csv"),
  tar_target(raw_ensemble, "results/fc_2022-08-17_final/archive/ensemble.csv"),
  
  tar_target(models_included, c("gar", "moss", "dst", "uoa")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  
  tar_target(do_upload_trajectories, FALSE),
  
  
  tar_target(use_fitting, TRUE),
  tar_target(is_longterm, longterm)
)

source("t_dependencies.R")


t_forecast