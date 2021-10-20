
source("R/data_processing/mediaflux.R")

sync_latest_mediaflux_vacc()


latest_ensemble_file <- get_latest_ensemble_file()
latest_nndss_file <- get_latest_NNDSS_file()

mf_files <- tribble(
  ~remote_file, ~local_file,
  paste0("forecast-outputs/", latest_ensemble_file), "data/input/ensemble_samples.csv",
  paste0("Health Uploads/", latest_nndss_file), "data/input/NNDSS.xlsx",
)
download_mediaflux_files(mf_files)


source("R/data_processing/dropbox.R")
download_files(tibble(remote_file = "/covid_output/local_cases_input.csv",
                      local_file = "data/input/local_cases_input.csv"))
