
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
  self$initialize_ward()
  
  return(private$current_compartment)
}


# Ward

initialize_ward <- function() {
  private$current_compartment <- compartment_indices["ward"]
  
  pr_ward_death <- self$model_params$morbidity_params$prob_death_ward_lookup[self$age_class, self$vaccine_status]
  
  pr_ward_ICU <- 0.1
  
  pr_ward_discharge <- 1 - pr_ward_ICU - pr_ward_death
  
  if(pr_ward_discharge < 0) {
    stop("invalid discharge probability")
  }
  
  ward_fate <- sample(c("ward_to_death", "ward_to_ICU", "ward_to_discharge"), size = 1,
                      prob = c(pr_ward_death, pr_ward_ICU, pr_ward_discharge))
  
  LoS_mean <- self$model_params$delay_params$compartment_LoS_mean[self$age_class, ward_fate]
  LoS_shape <- self$model_params$delay_params$compartment_LoS_shape[self$age_class, ward_fate]
  
  next_compartment <- c("ward_to_death" = "ward_died",
                        "ward_to_ICU" = "ICU",
                        "ward_to_discharge" = "ward_discharged")
  
  private$next_compartment <- compartment_indices[next_compartment[ward_fate]]
  
  #private$compartment_threshold_time <- rgamma(1, LoS_shape, LoS_shape / LoS_mean)
  private$compartment_threshold_time <- 100000
  
  return(private$current_compartment)
}



