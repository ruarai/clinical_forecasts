
library(tidyverse)


forecast_set <- expand_grid(
  fc_date = c(
    # "2022-03-24",
    # "2022-04-01",
    # "2022-04-08",
    # "2022-04-14", 
    #"2022-04-22",
    "2022-04-29"),
  fc_suffix = "retro_no_oracle"
)


targets_template <- read_file("_targets_auto.R")


for(i in 1:nrow(forecast_set)) {
  i_row <- forecast_set[i, ]
  
  targets_template %>%
    str_replace_all("%fc_date%", i_row$fc_date) %>%
    str_replace_all("%fc_suffix%", i_row$fc_suffix) %>%
    write_file("_targets_auto_tmp.R")
  
  tar_make(script = "_targets_auto_tmp.R")
}
