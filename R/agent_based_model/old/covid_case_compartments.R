
# Susceptible (i.e. waiting to be a case)

initialize_susceptible <- function() {
  private$current_compartment <- compartment_indices["susceptible"]
  
  private$compartment_threshold_time <- self$case_delay_samples$time_of_infection
}

transition_from_susceptible <- function() {
  self$initialize_symptomatic()
}


# Symptomatic

initialize_symptomatic <- function() {
  private$current_compartment <- compartment_indices["symptomatic"]
  
  private$compartment_threshold_time <- self$case_delay_samples$symptomatic_to_ED
}

transition_from_symptomatic <- function() {
  self$initialize_ward()
}


# Ward

initialize_ward <- function() {
  private$current_compartment <- compartment_indices["ward"]
  
  pr_ward_death <- self$case_morbidity_samples$pr_death_ward
  
  pr_ward_ICU <- 0.3
  
  pr_ward_discharge <- 1 - pr_ward_ICU - pr_ward_death
  
  if(pr_ward_discharge < 0) {
    stop("invalid ward discharge probability")
  }
  
  ward_fate <- sample(c("ward_to_death", "ward_to_ICU", "ward_to_discharge"), size = 1,
                      prob = c(pr_ward_death, pr_ward_ICU, pr_ward_discharge))
  
  next_compartment <- c("ward_to_death" = "ward_died",
                        "ward_to_ICU" = "ICU",
                        "ward_to_discharge" = "ward_discharged")
  
  private$next_compartment <- compartment_indices[next_compartment[ward_fate]]
  
  private$compartment_threshold_time <- self$case_delay_samples[[ward_fate]]
}

transition_from_ward <- function() {
  next_comp <- private$next_compartment
  
  if(next_comp == compartment_indices["ICU"]) {
    
    self$initialize_ICU()
    
    
  } else {
    
    # Nothing happens. Discharged/dead
    private$compartment_threshold_time <- NA
    private$current_compartment <- next_comp
  }
}

initialize_ICU <- function() {
  private$current_compartment <- compartment_indices["ICU"]
  
  pr_ICU_death <- self$case_morbidity_samples$pr_death_ICU
  
  # This needs to be thought through properly
  pr_ICU_to_postICU_death <- 0.5 
  
  pr_ICU_to_postICU_discharge <- 1 - pr_ICU_death - pr_ICU_to_postICU_death
  
  if(pr_ICU_to_postICU_discharge < 0) {
    stop("invalid ICU discharge probability")
  }
  
  
  ICU_fate <- sample(c("ICU_to_death", "ICU_to_postICU_death", "ICU_to_postICU_discharge"), size = 1, 
                     prob = c(pr_ICU_death, pr_ICU_to_postICU_death, pr_ICU_to_postICU_discharge))
  
  ## TODO
  
  
  next_compartment <- c("ICU_to_death" = "ICU_died",
                        "ICU_to_postICU_death" = "postICU_to_death",
                        "ICU_to_postICU_discharge" = "postICU_to_discharge")
  
  private$next_compartment <- compartment_indices[next_compartment[ICU_fate]]
  
  private$compartment_threshold_time <- self$case_delay_samples[[ICU_fate]]
}

transition_from_ICU <- function() {
  next_comp <- private$next_compartment
  
  if(next_comp == compartment_indices["postICU_to_death"] | 
     next_comp == compartment_indices["postICU_to_discharge"]) {
    
    self$initialize_postICU(next_comp)
    
    
  } else {
    
    # Nothing happens. Gone!
    private$compartment_threshold_time <- NA
    private$current_compartment <- next_comp
  }
}

initialize_postICU <- function(current_comp) {
  private$current_compartment <- current_comp
  
  current_comp_name <- names(compartment_indices)[current_comp]
  
  next_compartment <- c("postICU_to_death" = "postICU_died",
                        "postICU_to_discharge" = "postICU_discharged")
  
  
  private$next_compartment <- compartment_indices[next_compartment[current_comp_name]]
  
  private$compartment_threshold_time <- self$case_delay_samples[[current_comp_name]]
}

transition_from_postICU <- function() {
  # No simulation past this point
  
  next_comp <- private$next_compartment
  private$compartment_threshold_time <- NA
  private$current_compartment <- next_comp
}
