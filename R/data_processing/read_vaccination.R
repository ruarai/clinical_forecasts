
process_vaccination_data <- function() {
  effective_dose_data <- read_csv("data/input/effective_dose_data.csv") %>%
    
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
  
  write_rds(daily_dose_data, "data/processed/daily_dose_data.rds")
  
  daily_dose_data
}

  
  
  
