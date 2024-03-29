library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

#state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))
state_tbl <- tibble::tibble(state_modelled = c("NSW"))


# Do not change these values
longterm <- FALSE
is_retro <- TRUE

t_parameters <- list(
  tar_target(date_forecasting, ymd("%fc_date%")),
  
  
  tar_target(date_simulation_start, ymd("2021-12-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_%fc_suffix%")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "%path_nindss%"),
  tar_target(raw_local_cases, "%path_local_cases%"),
  tar_target(raw_ensemble, "%path_ensemble%"),
  
  tar_target(models_included, c("gar", "moss", "dst", "uoa")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  
  tar_target(do_upload_trajectories, FALSE),
  
  
  tar_target(use_fitting, TRUE),
  tar_target(is_longterm, longterm)
)

source("t_dependencies.R")


t_forecast