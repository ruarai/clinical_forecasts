

produce_input_trajectories <- function(simulation_options,
                                       model_params) {
  require(tidyverse)
  require(lubridate)
  
  ### Backcast linelist creation:
  
  vaccination_prob_table <- read_rds(simulation_options$files$vacc_prob_table)
  
  # Ignoring population vaccination rates, expecting that ~10% of hospitalizations
  # are vaccinated
  vaccination_prob_table <- vaccination_prob_table %>%
    filter(state == simulation_options$state_modelled) %>%
    group_by(state, date, age_class) %>%
    mutate(proportion = case_when(name == "none" ~ 0.9,
                                  TRUE ~ 0.1 * proportion),
           proportion = proportion / sum(proportion))
  
  
  
  
  clinical_linelist <- read_rds(simulation_options$files$clinical_linelist) %>%
    mutate(dt_onset = dt_hosp_admission - ddays(6),
           date_onset = date(dt_onset)) # No onset date in NSW data
  
  full_linelist <- read_rds(simulation_options$files$NNDSS_linelist) %>%
    filter(state == simulation_options$state_modelled)
  
  # Produce a backcast linelist over the period (simulation_options$dates$simulation_start, date_last_infection_50 - 5)
  # Assigning pr_hosp, pr_ICU according to known values
  case_linelist_with_vacc_prob <- clinical_linelist %>%
    
    filter(date_onset <= simulation_options$dates$last_infection_50 - 5) %>%
    
    mutate(state = simulation_options$state_modelled) %>%
    mutate(t_onset = (dt_onset - as.POSIXct(simulation_options$dates$simulation_start)) / ddays(1),
           case_ix = row_number()) %>%
    filter(t_onset >= 0) %>%
    
    left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date")) %>%
    
    mutate(pr_ICU = if_else(ever_in_icu, 1, 0),
           pr_hosp = 1)
  
  # Use slice_sample across our vaccination options to produce
  # a vaccination status. To be replaced by actual vaccine status eventually
  backcast_case_linelist <- case_linelist_with_vacc_prob %>%
    group_by(case_ix) %>%
    
    slice_sample(n = 1, weight_by = proportion) %>%
    ungroup() %>% 
    rename(vaccine = name) %>%
    select(state, date_onset, t_onset, age_class, vaccine,
           pr_hosp, pr_ICU)
  
  ## Nowcast & forecast case linelist creation:
  
  
  local_cases <- read_csv(simulation_options$files$local_cases,
                          show_col_types = FALSE) %>%
    filter(state == simulation_options$state_modelled,
           date_onset > max(backcast_case_linelist$date_onset),
           date_onset <= simulation_options$dates$last_onset_50)
  
  
  ensemble_spec <- cols(
    state = col_character(), .model = col_character(),
    date = col_date(), forecast_origin = col_date(),
    .default = col_double()
  )
  
  ensemble_data <- read_csv(simulation_options$files$ensemble_samples,
                            col_types = ensemble_spec)
  clinical_prob_table <- read_rds(simulation_options$files$clinical_prob_table)
  
  ensembles_wide <- ensemble_data %>%
    select(-forecast_origin) %>%
    pivot_wider(names_from = ".model", values_from = starts_with("sim"))
  
  # Create a table of age_class samples over recent cases (and not just clinical cases)
  # to be sampled from for forecasted cases
  forecast_age_class_samples <- full_linelist %>%
    filter(date_onset >= simulation_options$dates$last_onset_50 - 14) %>%
    pull(age_class) %>%
    table()
  
  # The forecast vaccination table
  forecast_vacc_prob <- vaccination_prob_table %>%
    ungroup() %>% filter(date == max(date))
  
  # Create a table of vaccination status for each age_class to be sampled from
  # for forecasted cases
  forecast_vaccine_status_samples <- model_params$covariates_age %>%
    map(function(age_class) {
      age_class_probs <- forecast_vacc_prob %>% filter(age_class == !!age_class)
      
      sample(age_class_probs$name, 1000, age_class_probs$proportion, replace = TRUE)
    }) %>%
    `names<-`(model_params$covariates_age)
  
  
  
  # Select some trajectories for our state
  ensemble_trajectories <- ensembles_wide %>%
    filter(state == simulation_options$state_modelled,
           date <= simulation_options$dates$forecast_horizon) %>%
    select(date_onset = date,
           sample(3:ncol(.), size = simulation_options$n_trajectories))
  
  assembled_linelists <- map(1:simulation_options$n_trajectories, function(i_sim) {
    
    # Sampling cases over the nowcast period according to observed count
    # and detection_probability, using a nbinom parameterisation with dispersion=10
    # This formulation might be wrong, to be clarified
    nowcast_traj <- local_cases %>%
      select(date_onset, count, detection_probability) %>%
      mutate(count = rnbinom(count, 10, mu = count / detection_probability)) %>%
      select(date_onset, count) %>%
      uncount(count, .remove = TRUE)
    
    
    
    # Select the ith ensemble_trajectory for this linelist
    ensemble_traj <- ensemble_trajectories %>%
      select(date_onset, !!(i_sim + 1)) %>%
      `colnames<-`(c("date_onset", "sim")) %>%
      mutate(sim = round(sim)) %>%
      uncount(sim, .remove = TRUE)
    
    # Combine our nowcast and forecast trajectories
    # then fill them with sampled age_class and vaccination status
    joined_trajs_filled <- bind_rows(nowcast_traj, ensemble_traj)  %>%
      
      mutate(t_onset = as.numeric(date_onset - simulation_options$dates$simulation_start),
             age_class = sample(names(forecast_age_class_samples), nrow(.),
                                prob = forecast_age_class_samples, replace = TRUE),
             
             state = simulation_options$state_modelled) %>%
      
      left_join(clinical_prob_table, by = c("date_onset", "age_class")) %>%
      
      group_by(age_class) %>%
      
      mutate(vaccine = sample(forecast_vaccine_status_samples[[first(age_class)]],
                              n(), replace = TRUE)) %>%
      
      ungroup()
    
    # Finally return a linelist with backcast, nowcast and forecast
    bind_rows(backcast_case_linelist, joined_trajs_filled)
  })
  
  
  
  assembled_linelists
}

