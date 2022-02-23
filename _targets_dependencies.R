library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  "tidyverse",
  "lubridate"
))


source("R/clinical_parameters.R")
source("R/mediaflux.R")
source("R/nindss.R")
source("R/model_parameters.R")

source("R/data_various.R")

source("R/morbidity_estimations.R")
source("R/ensemble.R")

source("R/occupancy_timeseries.R")

source("R/case_trajectories.R")
source("R/progression_model.R")

source("R/plotting/state_results.R")
source("R/plotting/state_results_capacity.R")
source("R/plotting/joint_results.R")

source("R/plotting/diagnostics.R")

source("t_absenteeism.R")
