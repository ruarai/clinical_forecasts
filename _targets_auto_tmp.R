library(targets)
library(tarchetypes)
library(future.callr)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "curvemush"), garbage_collection = TRUE, backoff = 60)

state_tbl <- tibble::tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))


# Do not change these values
is_retro <- TRUE

t_parameters <- list(
  tar_target(date_forecasting, ymd("2022-11-18")),
  
  
  tar_target(date_simulation_start, date_forecasting - days(120)),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_test_pf_b_2")),
  
  tar_target(raw_nindss, "results/fc_2022-11-18_final/archive/nindss.fst"),
  tar_target(raw_local_cases, "results/fc_2022-11-18_final/archive/local_cases.csv"),
  tar_target(raw_ensemble, "results/fc_2022-11-18_final/archive/ensemble.csv"),
  
  tar_target(models_included, c("gar", "moss_varasc", "dst_new", "moss_varasc_unsmoothed", "dst_behave",
                                "moss", "moss_unsmoothed")),
  
  tar_target(nindss_bad_states, c("NT", "SA", "QLD")),
  
  tar_target(do_upload_trajectories, FALSE),
  
  tar_target(occupancy_path, "data/occupancy/NAT_2023-06-01_Data for Uni of Melbourne.xlsx"),
  
  tar_target(n_traj_out, 1000),
  tar_target(use_fitting, TRUE),
  tar_target(is_longterm, longterm)
)

source("t_dependencies.R")


t_forecast