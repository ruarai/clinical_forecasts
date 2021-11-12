require(tidyverse)
require(lubridate)
require(flexsurv)



setwd("/usr/local/forecasting/source/covid19_los_estimations")



state_modelled <- "NSW"
data_date_str <- "081121"
data_date <- dmy(data_date_str)

output_dir <- paste0("output/", state_modelled, "/", data_date)

dir.create(output_dir, recursive = TRUE)

linelist_raw <- readxl::read_excel(paste0("../linelist_data/NSW/NSW_out_episode_", data_date_str, ".xlsx"),
                                   sheet = 2)

source("R/read_NSW_linelist.R")

clinical_linelist <- read_NSW_linelist(linelist_raw) %>%
  filter(dt_hosp_admission >= ymd("2021-Jul-07"))

source("R/process_survival.R")

source("R/compare_estimations.R")

source("R/survplots.R")
