library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
    "tidyverse",
    "lubridate",
    "curvemush"
  ),
  garbage_collection = TRUE
)


library(future.callr)
plan(callr)


state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-04-29")),
  tar_target(date_reporting_line, ymd("2022-04-29")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_retro_no_oracle")),
  
  tar_target(models_included, c("gar", "dst", "moss", "uoa")),
  
  #tar_target(nindss_bad_states, c("NT", "SA", "QLD", "VIC")),
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "WA")),
  
  tar_target(do_upload_trajectories, FALSE)
)


source("t_dependencies.R")







t_forecast