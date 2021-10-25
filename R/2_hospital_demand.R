

ensemble_spec <- cols(
  state = col_character(),
  date = col_date(),
  forecast_origin = col_date(),
  .model = col_character(),
  .default = col_double()
)

ensemble_samples <- read_csv("data/input/ensemble_samples.csv", col_types = ensemble_spec) %>%
  filter(state == "NSW") %>%
  select(-c(forecast_origin, state)) %>%
  pivot_wider(names_from = ".model", values_from = starts_with("sim"))




probability_hospitalisation <- read_rds("data/processed/probability_hospitalisation.rds")

hospitalisation_ensemble <- ensemble_samples %>%
  inner_join(probability_hospitalisation, by = c('date' = 'date_onset')) %>%
  
  mutate(across(.cols = starts_with("sim"), ~ . * prob_hosp))


for(i in 1:nrow(ensemble_samples)) {
  
  
  
}