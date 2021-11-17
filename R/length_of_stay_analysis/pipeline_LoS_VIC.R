require(tidyverse)
require(lubridate)
require(flexsurv)



setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")



state_modelled <- "VIC"
data_date_str <- "20211115"
data_date <- ymd(data_date_str)

output_dir <- paste0("results_length_of_stay/", state_modelled, "-", data_date)

dir.create(output_dir, recursive = TRUE)

linelist_raw <- paste0("/usr/local/forecasting/linelist_data/VIC/", data_date_str, "_Individual_Stay_Data.csv")

source("R/linelist_processing/read_VIC_linelist.R")

clinical_linelist <- read_VIC_linelist(linelist_raw,
                                       data_date)

source("R/length_of_stay_analysis/process_survival.R")

source("R/length_of_stay_analysis/compare_estimations.R")

source("R/length_of_stay_analysis/survplots.R")
