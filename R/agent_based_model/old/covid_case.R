library(R6)


source("R/agent_based_model/covid_case_compartments.R")

compartment_indices <- c(
  "susceptible" = 1,
  "symptomatic" = 2,
  "ward" = 3,
  "ward_died" = 4,
  "ward_discharged" = 5,
  "ICU" = 6,
  "ICU_died" = 7,
  "postICU_to_death" = 8,
  "postICU_to_discharge" = 9,
  "postICU_died" = 10,
  "postICU_discharged" = 11
  )

CovidCase <- R6Class(
  classname = "CovidCase",
  public = list(
    age_class = NA_character_,
    vaccine_status = NA_character_,
    
    initialize_susceptible = initialize_susceptible,
    transition_from_susceptible = transition_from_susceptible,
    
    initialize_symptomatic = initialize_symptomatic,
    transition_from_symptomatic = transition_from_symptomatic,
    
    initialize_ward = initialize_ward,
    transition_from_ward = transition_from_ward,
    
    initialize_ICU = initialize_ICU,
    transition_from_ICU = transition_from_ICU,
    
    initialize_postICU = initialize_postICU,
    transition_from_postICU = transition_from_postICU,
    
    case_delay_samples = list(),
    case_morbidity_samples = list(),
    
    initialize = function(age_class, vaccine_status,
                          case_delay_samples,
                          case_morbidity_samples) {
      self$age_class <- age_class
      self$vaccine_status <- vaccine_status
      
      self$case_delay_samples <- case_delay_samples
      self$case_morbidity_samples <- case_morbidity_samples
      
      self$initialize_susceptible()
      
    },
    
    print = function() {
      print(paste0("Case (age ", self$age_class,
                   ", ", self$vaccine_status, ")",
                   " in compartment '", private$current_compartment, "'"))
      
      invisible(self)
    },
    
    trigger_transition = function() {
      if(private$current_compartment == compartment_indices["susceptible"]) {
        self$transition_from_susceptible()
      } 
      else if(private$current_compartment == compartment_indices["symptomatic"]) {
        self$transition_from_symptomatic()
      } 
      else if(private$current_compartment == compartment_indices["ward"]) {
        self$transition_from_ward()
      } 
      else if(private$current_compartment == compartment_indices["ICU"]) {
        self$transition_from_ICU()
      } 
      else if(private$current_compartment == compartment_indices["postICU_to_death"] | 
              private$current_compartment == compartment_indices["postICU_to_discharge"]) {
        self$transition_from_postICU()
      }
      
      return(private$current_compartment)
    },
    
    
    
    get_compartment_threshold_time = function() {
      private$compartment_threshold_time
    },
    get_current_compartment = function() {
      private$current_compartment
    }
  ),
  private = list(
    current_compartment = NA_integer_,
    next_compartment = NA_character_,
    
    compartment_threshold_time = NA_real_
  ))
