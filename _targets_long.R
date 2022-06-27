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

state_tbl <- tibble::tibble(state_modelled = c("TAS", "VIC", "ACT", "WA", "QLD", "SA", "NT", "NSW"))
longterm <- TRUE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-06-24")),
  tar_target(date_reporting_line, ymd("2022-06-24")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final_longterm_75asc")),
  
  tar_target(models_included, c("dst", "moss")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  tar_target(is_longterm, longterm),
  
  
  tar_target(do_upload_trajectories, FALSE)
)


source("t_dependencies.R")



t_forecast