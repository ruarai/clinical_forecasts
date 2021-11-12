require(tidyverse)
require(lubridate)
require(flexsurv)



setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")



state_modelled <- "NSW"
data_date_str <- "081121"
data_date <- dmy(data_date_str)

output_dir <- paste0("results_length_of_stay/", state_modelled, "-", data_date)

dir.create(output_dir, recursive = TRUE)

linelist_raw <- readxl::read_excel(paste0("/usr/local/forecasting/linelist_data/NSW/NSW_out_episode_",
                                          data_date_str, ".xlsx"),
                                   sheet = 2)

source("R/linelist_processing/read_NSW_linelist.R")

clinical_linelist <- read_NSW_linelist(linelist_raw) %>%
  filter(dt_hosp_admission >= ymd("2021-Jul-07"))

source("R/length_of_stay_analysis/process_survival.R")

source("R/length_of_stay_analysis/compare_estimations.R")

source("R/length_of_stay_analysis/survplots.R")
