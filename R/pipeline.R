
setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")

source("R/data_processing/mediaflux.R")

sync_latest_mediaflux_vacc()

# May want to overwrite these if they get it wrong
latest_ensemble_file <- get_latest_ensemble_file()
latest_nndss_file <- get_latest_NNDSS_file()

mf_files <- tribble(
  ~remote_file, ~local_file,
  paste0("forecast-outputs/", latest_ensemble_file), "data/input/ensemble_samples.csv",
  paste0("Health Uploads/", latest_nndss_file),      "data/input/NNDSS.xlsx",
)
download_mediaflux_files(mf_files)


source("R/data_processing/dropbox.R")
download_files(tibble(remote_file = "/covid_output/local_cases_input.csv",
                      local_file = "data/input/local_cases_input.csv"))



covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
read_csv(covid19data_url) %>% write_rds("data/covid19data.rds")



source("R/data_processing/read_NNDSS.R")
process_NNDSS_linelist(minimum_case_date = ymd("2020-01-01"))
process_vaccination_data()


source("R/model_parameters.R")
model_parameters <- get_model_parameters()


source("R/compartment_model/define_compartment_model.R")
compartment_model_definition <- get_compartment_model_definition(model_parameters)







