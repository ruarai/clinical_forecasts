

source("R/model_parameters.R")
model_params <- get_model_parameters()

state_modelled <- "NSW"
date_0 <- lubridate::ymd("2021-06-01")
n_days_forward <- 30
n_simulations <- 10


## Building the backcast case linelist:

vaccination_prob_table <- read_rds("data/processed/vaccination_probability_table.rds")

# Ignoring true vaccination rates, expecting that ~10% of hospitalisation
# are vaccinated
vaccination_prob_table <- vaccination_prob_table %>%
  filter(state == state_modelled) %>%
  group_by(state, date, age_class) %>%
  mutate(proportion = case_when(name == "none" ~ 0.9,
                                TRUE ~ 0.1 * proportion),
         proportion = proportion / sum(proportion))




clinical_linelist <- read_rds("data/processed/clinical_linelist_NSW.rds") %>%
  mutate(dt_onset = dt_hosp_admission - ddays(6),
         date_onset = date(dt_onset)) # No onset date in NSW data

case_linelist_with_vacc_prob <- clinical_linelist %>%
  mutate(state = state_modelled) %>%
  mutate(t_onset = (dt_onset - as.POSIXct(date_0)) / ddays(1),
         
         delay_onset_to_hospital = (dt_hosp_admission - dt_onset) / ddays(1),
         delay_ward_to_ICU = (dt_first_icu - dt_hosp_admission) / ddays(1),
         
         ix = row_number()) %>%
  filter(t_onset >= 0) %>%
  
  left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date")) %>%
  left_join(clinical_prob_table %>% select(-pr_hosp)) %>%
  
  mutate(pr_ICU = if_else(ever_in_icu, 1, 0))

backcast_case_linelist <- case_linelist_with_vacc_prob %>%
  group_by(ix) %>%
  
  slice_sample(n = 1, weight_by = proportion) %>%
  ungroup() %>%
  select(-c(ix, proportion)) %>% rename(vaccine = name)


## Forecast case line-list creation:
ensemble_spec <- cols(
  state = col_character(), .model = col_character(),
  date = col_date(), forecast_origin = col_date(),
  .default = col_double()
)

ensemble_data <- read_csv("data/input/ensemble_samples.csv",
                          col_types = ensemble_spec)
clinical_prob_table <- read_rds("data/processed/clinical_probabilities.rds")

ensembles_wide <- ensemble_data %>%
  select(-forecast_origin) %>%
  pivot_wider(names_from = ".model", values_from = starts_with("sim"))

sample_hospitalisation <- function(n_onset, pr_hosp) {
  n_onset <- round(n_onset)
  
  rbinom(n_onset, n_onset, pr_hosp)
}

forecast_age_class_samples <- clinical_linelist %>%
  filter(date_onset >= max(date_onset) - 14) %>%
  pull(age_class) %>%
  table()

forecast_vacc_prob <- vaccination_prob_table %>%
  ungroup() %>% filter(date == max(date))

forecast_vaccine_status_samples <- model_params$covariates_age %>%
  map(function(age_class) {
    age_class_probs <- forecast_vacc_prob %>% filter(age_class == !!age_class)
    
    sample(age_class_probs$name, 1000, age_class_probs$proportion, replace = TRUE)
  }) %>%
  `names<-`(model_params$covariates_age)




ensemble_trajectories <- ensembles_wide %>%
  filter(state == state_modelled,
         date <= min(date) + n_days_forward) %>%
  select(date_onset = date, sample(3:ncol(.), size = n_simulations)) %>%
  # 
  # left_join(clinical_prob_table %>% filter(age_class == "40-44")) %>%
  # 
  # mutate(across(starts_with("sim"), ~ sample_hospitalisation(., pr_hosp))) %>%
  
  select(date_onset, starts_with("sim"))

forecast_linelists <- map(1:n_simulations, function(i_sim) {
  ensemble_traj <- ensemble_trajectories %>%
    select(date_onset, !!(i_sim + 1)) %>%
    `colnames<-`(c("date_onset", "sim")) %>%
    group_by(date_onset) %>%
    summarise(tibble(daily_ix = 1:sim), .groups = "drop") %>%
    select(-daily_ix) %>%
    
    mutate(age_class = sample(names(forecast_age_class_samples), n(),
                              prob = forecast_age_class_samples, replace = TRUE),
           
           state = state_modelled,
           ix = row_number()) %>%
    
    left_join(clinical_prob_table, by = c("date_onset", "age_class")) %>%
    
    
    
    left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date")) %>%
    
    group_by(ix) %>%
    slice_sample(n = 1, weight_by = proportion) %>%
    ungroup() %>%
    select(-c(ix, proportion)) %>% rename(vaccine = name)
})





