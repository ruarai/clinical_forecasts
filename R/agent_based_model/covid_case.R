library(R6)


source("R/agent_based_model/covid_case_compartments.R")

compartment_indices <- c(
  "susceptible" = 1,
  "symptomatic" = 2,
  "ward" = 3
  )

CovidCase <- R6Class(
  classname = "CovidCase",
  public = list(
    age_class = NA_character_,
    vaccine_status = NA_character_,
    
    index = NA_integer_,
    
    initialize_susceptible = initialize_susceptible,
    transition_susceptible = transition_susceptible,
    
    initialize_symptomatic = initialize_symptomatic,
    transition_symptomatic = transition_symptomatic,
    
    model_params = list(),
    
    initialize = function(age_class, vaccine_status,
                          time_of_infection, model_params,
                          index) {
      self$age_class <- age_class
      self$vaccine_status <- vaccine_status
      
      self$model_params <- model_params
      
      self$initialize_susceptible(time_of_infection)
      
      self$index <- index
    },
    
    print = function() {
      print(paste0("Case (age ", self$age_class,
                   ", ", self$vaccine_status, ")",
                   " in compartment '", private$current_compartment, "'"))
      
      invisible(self)
    },
    
    trigger_transition = function() {
      if(private$current_compartment == compartment_indices["susceptible"]) {
        return(self$transition_susceptible())
      } 
      else if(private$current_compartment == compartment_indices["symptomatic"]) {
        return(self$transition_symptomatic())
      } else{
        return(compartment_indices["ward"])
      }
    },
    
    get_compartment_threshold_time = function() {
      private$compartment_threshold_time
    }
  ),
  private = list(
    current_compartment = NA_integer_,
    
    compartment_threshold_time = NA_real_
  ))
