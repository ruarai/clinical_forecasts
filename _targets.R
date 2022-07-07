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
  
options(
  clustermq.scheduler = "ssh",
  clustermq.template = "/home/forecast/source/hpc_tests/slurm_template.template",
  clustermq.ssh.host = "ruarai@spartan.hpc.unimelb.edu.au",
  clustermq.ssh.timeout = 60
)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))


# Do not change 'longterm'
longterm <- FALSE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-07-05")),
  
  
  tar_target(date_simulation_start, ymd("2021-11-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_test_3")),
  
  # Update these to the latest file path
  # ~/mfluxshared and ~/mfluxunimelb should point to the (respective) mediaflux server
  tar_target(raw_nindss, "~/mfluxshared/Health Uploads/COVID-19 UoM 05Jul2022.zip"),
  tar_target(raw_local_cases, "~/mfluxunimelb/local_cases_input/local_cases_input_2022-07-06.csv"),
  tar_target(raw_ensemble, "~/mfluxshared/forecast-outputs/combined_samples_50asc2022-06-21.csv"),
  
  tar_target(models_included, c("gar", "moss", "dst")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD", "TAS")),
  tar_target(is_longterm, longterm),
  
  tar_target(do_upload_trajectories, TRUE)
)


source("t_dependencies.R")







t_forecast