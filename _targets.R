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
  tar_target(date_forecasting, ymd("2022-03-11")),
  tar_target(date_reporting_line, ymd("2022-03-11")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_final"))
)


source("t_dependencies.R")







t_forecast