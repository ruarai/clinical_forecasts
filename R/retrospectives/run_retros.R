library(tidyverse)
library(targets)


forecast_inputs <- tibble::tribble(
  ~date,
  "2022-02-05",
  "2022-02-11",
  "2022-02-18",
  "2022-02-25",
  "2022-03-04",
  "2022-03-11",
  "2022-03-18",
  "2022-03-24",
  "2022-04-01",
  "2022-04-08",
  "2022-04-14",
  "2022-04-22",
  "2022-04-29",
  "2022-05-04",
  "2022-05-12",
  "2022-05-19",
  "2022-05-27",
  "2022-06-03",
  "2022-06-10",
  "2022-06-17",
  "2022-06-24",
  "2022-07-01",
  "2022-07-08",
  "2022-07-15",
  "2022-07-22",
  "2022-07-28",
  "2022-08-03",
  "2022-08-11",
  "2022-08-17"
)

forecast_suffix <- "retro3_knowncases"


all_retros <- forecast_inputs %>%
  mutate(
    plausible_directory = str_c("results/fc_", date, "_final/archive"),
  ) %>%
  
  filter(dir.exists(plausible_directory)) %>%
  
  
  mutate(local_cases = str_c(plausible_directory, "/local_cases.csv"),
         nindss = str_c(plausible_directory, "/nindss.fst"),
         ensemble = str_c(plausible_directory, "/ensemble.csv"),
         
         out_dir = str_c("results/fc_", date, "_", forecast_suffix)) %>%
  
  filter(file.exists(local_cases), file.exists(nindss), file.exists(ensemble))





targets_template <- read_file("_targets.template.R")


for(i in 1:nrow(all_retros)) {
  
  i_row <- all_retros[i, ]
  
  print(str_c(i, " of ", nrow(all_retros), ", ", i_row$date))
  
  targets_template %>%
    str_replace_all("%fc_date%", i_row$date) %>%
    str_replace_all("%fc_suffix%", forecast_suffix) %>%
    str_replace_all("%path_local_cases%", i_row$local_cases) %>%
    str_replace_all("%path_nindss%", i_row$nindss) %>%
    str_replace_all("%path_ensemble%", i_row$ensemble) %>%
    write_file("_targets_auto_tmp.R")
  
  
  tar_make_future(script = "_targets_auto_tmp.R", workers = 8)
}






