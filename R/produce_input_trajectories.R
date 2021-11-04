

get_forecast_dates <- function(local_cases_file, state_modelled) {
  local_cases <- read_csv(local_cases_file) %>%
    filter(state == state_modelled)
  
  date_minimum_onset <- local_cases %>%
    pull(date_onset) %>%
    min()
  
  date_last_onset_50 <- local_cases %>%
    filter(detection_probability > 0.5) %>%
    pull(date_onset) %>% max()
  
  date_last_infection_50 <- date_last_onset_50 - 5
  
  date_forecast_horizon = date_last_onset_50 + 28
  
  tibble(
    date_minimum_onset = date_minimum_onset,
    date_last_onset_50 = date_last_onset_50,
    date_last_infection_50 = date_last_infection_50,
    date_forecast_horizon = date_forecast_horizon,
  )
}



source("R/model_parameters.R")
model_params <- get_model_parameters()

state_modelled <- "NSW"
forecast_dates <- get_forecast_dates("data/input/local_cases_input.csv", state_modelled)
date_0 <- lubridate::ymd("2021-06-01")
n_days_forward <- 28
n_simulation <- 50


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
  
  filter(date_onset <= forecast_dates$date_last_infection_50 - 5) %>%
  
  mutate(state = state_modelled) %>%
  mutate(t_onset = (dt_onset - as.POSIXct(date_0)) / ddays(1),
         
         delay_onset_to_hospital = (dt_hosp_admission - dt_onset) / ddays(1),
         delay_ward_to_ICU = (dt_first_icu - dt_hosp_admission) / ddays(1),
         
         ix = row_number()) %>%
  filter(t_onset >= 0) %>%
  
  left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date")) %>%
  
  mutate(pr_ICU = if_else(ever_in_icu, 1, 0),
         pr_hosp = 1)

backcast_case_linelist <- case_linelist_with_vacc_prob %>%
  group_by(ix) %>%
  
  slice_sample(n = 1, weight_by = proportion) %>%
  ungroup() %>%
  select(-c(ix, proportion)) %>% rename(vaccine = name)

## Nowcast & forecast case linelist creation:


local_cases <- read_csv("data/input/local_cases_input.csv") %>%
  filter(state == state_modelled,
         date_onset > max(backcast_case_linelist$date_onset),
         date_onset <= forecast_dates$date_last_onset_50)


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
  filter(date_onset >= forecast_dates$date_last_onset_50 - 14) %>%
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
         date <= forecast_dates$date_forecast_horizon) %>%
  select(date_onset = date, sample(3:ncol(.), size = n_simulation))

assembled_linelists <- map(1:n_simulation, function(i_sim) {
  # This formulation might be wrong, to be clarified
  nowcast_traj <- local_cases %>%
    select(date_onset, count, detection_probability) %>%
    mutate(count = rnbinom(count, 10, mu = count / detection_probability),
           t_onset = as.numeric(date_onset - date_0)) %>%
    select(date_onset, t_onset, count) %>%
    uncount(count, .remove = TRUE)
  
  
  
  
  ensemble_traj <- ensemble_trajectories %>%
    select(date_onset, !!(i_sim + 1)) %>%
    `colnames<-`(c("date_onset", "sim")) %>%
    mutate(sim = round(sim),
           t_onset = as.numeric(date_onset - date_0)) %>%
    uncount(sim, .remove = TRUE)
  
  joined_trajs_filled <- bind_rows(nowcast_traj, ensemble_traj)  %>%
    
    mutate(age_class = sample(names(forecast_age_class_samples), nrow(.),
                              prob = forecast_age_class_samples, replace = TRUE),
           
           state = state_modelled) %>%
    
    left_join(clinical_prob_table, by = c("date_onset", "age_class")) %>%
    
    group_by(age_class) %>%
    
    mutate(vaccine = sample(forecast_vaccine_status_samples[[first(age_class)]],
                            n(), replace = TRUE)) %>%
    
    ungroup()
  
  bind_rows(backcast_case_linelist, joined_trajs_filled)
})





