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

state_tbl <- tibble::tibble(state_modelled = c("TAS", "VIC", "ACT", "WA", "SA", "NT", "NSW", "QLD"))
#state_tbl <- tibble::tibble(state_modelled = c("NSW"))

# Do not change this value
longterm <- TRUE 

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-07-01")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_redo")),
  
  # Update these to the latest file path
  # /home/forecast/mfluxshared and /home/forecast/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "/home/forecast/mfluxshared/Health Uploads/COVID-19 UoM 05Jul2022.zip"),
  tar_target(raw_local_cases, "/home/forecast/mfluxunimelb/local_cases_input/local_cases_input_2022-07-06.csv"),
  tar_target(raw_ensemble, "/home/forecast/mfluxshared/forecast-outputs/combined_samples_75asc2022-06-21.csv"),
  
  tar_target(quantium_zip_path, "~/mfluxunimelb/vaccine_data_products/raw_quantium_forecasts/2022-07-05-mintcream8315-booster-uptake-scenarios.zip"),
  
  
  
  tar_target(models_included, c("dst", "moss", "gar")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "TAS")),
  tar_target(is_longterm, longterm),
  
  
  tar_target(do_upload_trajectories, TRUE)
)


source("t_dependencies.R")



t_forecast