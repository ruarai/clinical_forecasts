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

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))
longterm <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-07-04")),
  tar_target(date_reporting_line, ymd("2022-07-04")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final")),
  
  #tar_target(models_included, c("gar", "dst", "moss")),
  tar_target(models_included, c("gar", "moss")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "TAS")),
  tar_target(is_longterm, longterm),
  
  tar_target(do_upload_trajectories, TRUE)
)


source("t_dependencies.R")







t_forecast