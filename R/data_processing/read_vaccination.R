
process_vaccination_data <- function(simulation_options) {
  effective_dose_data <- read_csv(simulation_options$files$vacc_raw) %>%
    
    drop_na(effective_doses)  %>%
    
    # Important: ensure ordering by age class
    mutate(age_first_val = as.numeric(str_extract(age_class, "^[0-9]{1,2}(?=[-,+])"))) %>%
    arrange(age_first_val) %>% select(-age_first_val)
  
  
  age_classes <- unique(effective_dose_data$age_class)
  print(paste0("Using age classes: ", str_c(age_classes, collapse = ", ")))
  
  
  daily_dose_data <- expand_grid(
    date = seq(min(effective_dose_data$date), max(effective_dose_data$date), by = 'days'),
    
    state = unique(effective_dose_data$state),
    age_class = unique(effective_dose_data$age_class),
    vaccine = unique(effective_dose_data$vaccine),
    dose_number = c(1,2),
    
  ) %>%
    left_join(effective_dose_data) %>%
    
    group_by(state, age_class, vaccine, dose_number) %>%
    
    mutate(effective_doses = zoo::na.approx(effective_doses)) %>%
    
    select(-doses)
  
  #write_rds(daily_dose_data, "data/processed/daily_dose_data.rds")
  
  
  age_dist_by_state <- read_csv("data/demographics/age_distribution_by_state.csv")
  
  empty_probability_table <- expand_grid(
    date = seq(ymd("2020-01-01"), ymd("2022-01-01"), by = "day"),
    state = unique(daily_dose_data$state),
    age_class = unique(daily_dose_data$age_class),
    name = c("none", "az1", "az2", "pf1", "pf2")
  )
  
  probability_table <- expand_grid(
    date = seq(min(daily_dose_data$date), max(daily_dose_data$date), by = "day"),
    state = unique(daily_dose_data$state),
    age_class = unique(daily_dose_data$age_class),
    dose_number = c(1,2),
    vaccine = c("pf", "az")
  ) %>%
    left_join(daily_dose_data) %>%
    left_join(age_dist_by_state) %>%
    mutate(proportion = pmin(effective_doses / population, 1),
           name = str_c(vaccine, dose_number)) %>%
    
    right_join(empty_probability_table) %>%
    
    group_by(state, name, age_class) %>%
    arrange(date) %>%
    fill(proportion, .direction = 'updown') %>%
    
    ungroup() %>%
    
    select(state, date, age_class, name, proportion) %>%
    
    group_by(state, date, age_class) %>%
    mutate(total_prop = sum(proportion, na.rm = TRUE)) %>%
    mutate(proportion = case_when(name == "none" ~ max(0, 1 - total_prop),
                                TRUE ~ proportion)) %>%
    
    select(-total_prop)
  
  
  
  write_rds(probability_table,
            simulation_options$files$vacc_prob_table)
}

  
  
  
