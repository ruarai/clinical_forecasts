library(tidyverse)
library(targets)


forecast_inputs <- tibble::tribble(
  ~date,
  "2022-11-18", "2022-11-25",
  "2022-12-02", "2022-12-09", "2022-12-15",# "2023-01-06",
  "2023-01-12",
  "2023-01-19", "2023-01-27", "2023-02-02", "2023-02-09", "2023-02-16",
  "2023-02-23", "2023-03-03", "2023-03-09", "2023-03-17", "2023-03-23",
  "2023-03-31",
  "2023-04-06", "2023-04-14", "2023-04-20", "2023-04-27", "2023-05-05",
  "2023-05-12", "2023-05-19"
)

forecast_suffix <- "test_pf_b_2"


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
  
  
  #tar_make_future(script = "_targets_auto_tmp.R", workers = 8)
  tar_make(c(backup_trajs, starts_with("state_archive")), script = "_targets_auto_tmp.R")
}






