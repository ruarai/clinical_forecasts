setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")


## NSW
simulation_options <- list(
  n_trajectories = 50,
  n_samples_per_trajectory = 4,
  n_days_forward = 28,
  
  run_name = "NSW-test-2021-11-08",
  
  state_modelled = "NSW"
)


simulation_options$dirs <- list(
  plots = paste0("results/",simulation_options$run_name, "/plots/"),
  data = paste0("results/",simulation_options$run_name, "/data/"),
  input = paste0("results/",simulation_options$run_name, "/input/")
)

map(simulation_options$dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))

simulation_options$files <- list(
  local_cases = paste0(simulation_options$dirs$input, "local_cases_input.csv"),
  
  vacc_prob_table = paste0(simulation_options$dirs$data, "vaccination_probability_table.rds"),
  clinical_prob_table = paste0(simulation_options$dirs$data, "clinical_probabilities.rds"),
  
  clinical_linelist = paste0(simulation_options$dirs$input, "clinical_linelist.rds"),
  clinical_linelist_source = "/usr/local/forecasting/source/linelist_data/NSW/NSW_out_episode_081121.xlsx",
  
  NNDSS_linelist = paste0(simulation_options$dirs$input, "linelist_NNDSS.rds"),
  NNDSS_raw = paste0(simulation_options$dirs$input, "NNDSS.xlsx"),
  
  ensemble_samples = paste0(simulation_options$dirs$input, "ensemble_samples.csv")
)



source("R/data_processing/mediaflux.R")

latest_ensemble_file <- get_latest_file_path(
  "forecast-outputs",
  "combined_samples", "\\d{4}-\\d{2}-\\d{2}", ymd)

latest_nndss_file <- get_latest_file_path(
  "Health Uploads",
  "COVID-19 UoM", "\\ \\d{1,2}\\D{3}\\d{4}", dmy)


latest_vacc_file <- get_latest_file_path(
  "vaccine_allocation/vaccine_cumulative_medicare/tabular",
  "effective_dose_data", "\\d{4}-\\d{2}-\\d{2}", ymd)

mf_files <- tribble(
  ~remote_file, ~local_file,
  paste0("forecast-outputs/", latest_ensemble_file), simulation_options$files$ensemble_samples,
  paste0("Health Uploads/", latest_nndss_file),      simulation_options$files$NNDSS_raw,
)
download_mediaflux_files(mf_files)


source("R/data_processing/dropbox.R")
download_files(tibble(remote_file = "/covid_output/local_cases_input.csv",
                      local_file = "data/input/local_cases_input.csv"))



covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
read_csv(covid19data_url) %>% write_rds("data/covid19data.rds")



source("R/data_processing/read_NNDSS.R")
source("R/data_processing/read_vaccination.R")
process_NNDSS_linelist(minimum_case_date = ymd("2021-06-01"))

source("R/data_processing/fn_age_classes.R")
source("../covid19_los_estimations/R/read_NSW_linelist.R")
linelist_raw <- readxl::read_xlsx(,
                                  sheet = 2)

NSW_linelist <- read_NSW_linelist(linelist_raw) %>%
  mutate(age_class = assign_age_class(age))

write_rds(NSW_linelist, "data/processed/clinical_linelist_NSW.rds")


process_vaccination_data()


source("R/model_parameters.R")
model_parameters <- get_model_parameters()


source("R/data_processing/data_fns.R")

simulation_options$dates <- get_forecast_dates(
  simulation_options$files$local_cases,
  simulation_options$state_modelled,
  date_simulation_start = ymd("2021-06-01"),
  simulation_options$n_days_forward)

source("R/produce_input_trajectories.R")

input_trajectories <- produce_input_trajectories(simulation_options,
                                                 model_parameters)

input_trajectories %>%
  write_rds(paste0(simulation_options$dirs$data, "/input_trajectories.rds"))

source("R/agent_based_model/run_simulations.R")

sim_results <- run_simulations(input_trajectories,
                               simulation_options,
                               model_parameters)

sim_results %>%
  write_rds(paste0(simulation_options$dirs$data, "/sim_results.rds"))

source("R/agent_based_model/plot_abm_results.R")

plot_abm_results(sim_results, simulation_options)

