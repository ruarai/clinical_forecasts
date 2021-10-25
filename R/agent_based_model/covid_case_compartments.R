
# Susceptible (i.e. waiting to be a case)

initialize_susceptible <- function(time_of_infection) {
  private$current_compartment <- compartment_indices["susceptible"]
  
  private$compartment_threshold_time <- time_of_infection
}

transition_susceptible <- function() {
  self$initialize_symptomatic()
  
  return(private$current_compartment)
}


# Symptomatic

initialize_symptomatic <- function() {
  private$current_compartment <- compartment_indices["symptomatic"]
  
  
  LoS_mean <- self$model_params$delay_params$compartment_LoS_mean[self$age_class, "symptomatic_to_ED"]
  LoS_shape <- self$model_params$delay_params$compartment_LoS_shape[self$age_class, "symptomatic_to_ED"]
  
  # Double check this
  private$compartment_threshold_time <- rgamma(1, LoS_shape, LoS_shape / LoS_mean)
  
  return(private$current_compartment)
}

transition_symptomatic <- function() {
  private$current_compartment <- compartment_indices["ward"]
  
  return(private$current_compartment)
}