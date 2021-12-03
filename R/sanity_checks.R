

perform_sanity_checks <- function(simulation_options,
                                  model_parameters,
                                  
                                  input_trajectories,
                                  sim_results) {
  
  ## Clinical prob table
  
  clinical_prob_table <- read_rds(simulation_options$files$clinical_prob_table)
  
  print("Checking clinical_prob_table")
  
  stopifnot(all(clinical_prob_table$pr_hosp <= 1))
  stopifnot(all(clinical_prob_table$pr_ICU <= 1))
  
  stopifnot(all(clinical_prob_table$pr_hosp >= 0))
  stopifnot(all(clinical_prob_table$pr_ICU >= 0))
  
  
  ## Model parameters
  
  print("Checking model_parameters")
  
  stopifnot(all(model_parameters$morbidity_params$prob_table <= 1))
  stopifnot(all(model_parameters$morbidity_params$prob_table >= 0))
  
  stopifnot(all(model_parameters$delay_params$compartment_LoS_mean >= 0))
  stopifnot(all(model_parameters$delay_params$compartment_LoS_shape >= 0))
  
  
  ## Input trajectories
  
  print("Checking input_trajectories")
  
  # Check first (at most) 10 trajectories for errors
  for(i in 1:min(10, simulation_options$n_trajectories)) {
    traj <- input_trajectories[[i]]
    
    all(traj$t_onset >= 0)
    
    all(traj$age_class %in% model_parameters$covariates_age)
    #all(traj$vaccine %in% model_parameters$covariates_vaccine_status)
  }
  
  ## Simulation results
  
  print("Checking sim_results")
  
  decreasing_compartments <- c("susceptible")
  
  increasing_compartments <- c("ward_died", "ward_discharged",
                               "ICU_died", "ICU_discharged",
                               "postICU_died", "postICU_discharged")
  
  # Check first (at most) 10 result outputs for errors
  for(i in 1:min(10, simulation_options$n_trajectories)) {
    
    # Filter by trajectory *-i-*
    tbl_count_i <- sim_results$tbl_count %>%
      filter(str_detect(ix, paste0("-", i, "-")))
    
    for(j in 1:simulation_options$n_samples_per_trajectory) {
      tbl_count_j <- tbl_count_i %>%
        filter(str_detect(ix, paste0(i, "-", j)))
      
      # Does the total number of people in all compartments stay constant?
      daily_num_people <- tbl_count_j %>%
        group_by(date) %>%
        summarise(n_pop = sum(count))
      
      stopifnot(all(daily_num_people$n_pop == daily_num_people$n_pop[1]))
      
      
      # Do compartments we expect to be increasing ever decrease?
      increasings <- tbl_count_j %>% 
        filter(compartment %in% increasing_compartments) %>%
        group_by(ix, compartment) %>% 
        mutate(valid = count >= lag(count, default = 0))
      
      stopifnot(all(increasings$valid))
      
      # Do compartments we expect to be decreasing ever increase?
      decreasings <- tbl_count_j %>% 
        filter(compartment %in% decreasing_compartments) %>%
        group_by(ix, compartment) %>% 
        mutate(valid = count >= lead(count, default = 0))
      
      stopifnot(all(decreasings$valid))
    }
  }
  
  
  
  
  
  
  
}
