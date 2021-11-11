

source("R/data_processing/data_fns.R")
clinical_linelist_dir <- "/usr/local/forecasting/source/linelist_data/NSW/"
clinical_linelist_date <- ymd("2021-11-08")

clinical_linelist_source <- paste0(clinical_linelist_dir,
                                   "NSW_out_episode_",
                                   format(clinical_linelist_date, "%d%m%y"),
                                   ".xlsx")

simulation_options <- make_simulation_options(
  run_name = paste0("NSW-test-", clinical_linelist_date),
  state_modelled = "NSW",
  
  n_trajectories = 50,
  n_samples_per_trajectory = 4,
  n_days_forward = 28,
  
  clinical_linelist_source = clinical_linelist_source
)

source("R/linelist_processing/read_NSW_linelist.R")
process_NSW_linelist(simulation_options)


clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)


clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)


days <- seq(min(clinical_linelist$dt_hosp_admission, na.rm = TRUE),
            max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE),
            by ='days') %>% as_date()


linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist %>%
           filter(dt_hosp_discharge >= date, dt_hosp_admission <= date) %>%
           nrow(),
         
         count_ICU = clinical_linelist %>%
           drop_na(dt_first_icu) %>%
           filter(dt_last_icu >= date, dt_first_icu <= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")


ggplot(linelist_data_counts) +
  geom_line(aes(x = date, y = count, color = group))



