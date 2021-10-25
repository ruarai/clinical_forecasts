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
    
    initialize_susceptible = initialize_susceptible,
    transition_susceptible = transition_susceptible,
    
    initialize_symptomatic = initialize_symptomatic,
    transition_symptomatic = transition_symptomatic,
    
    model_params = list(),
    
    initialize = function(age_class, vaccine_status, time_of_infection, model_params) {
      self$age_class <- age_class
      self$vaccine_status <- vaccine_status
      
      self$model_params <- model_params
      
      self$initialize_susceptible(time_of_infection)
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


source("R/model_parameters.R")
model_params <- get_model_parameters()

case_list <- vector(mode = "list", length = 10000)
case_timings <- matrix(0, nrow = 10000, ncol = 2) %>%
  `colnames<-`(c("time_in_compartment", "threshold_time"))


local_cases <- read_csv()


for(i in 1:10000) {
  case <- CovidCase$new("0-4", "pf1", time_of_infection = runif(1, 0, 365), model_params)
  
  case_list[[i]] <- case
  case_timings[i, "threshold_time"] <- case$get_compartment_threshold_time()
}


case_states <- matrix(NA, ncol = 10000, nrow = 365)
case_states[1,] <- 1


for(i in 1:(100*365)) {
  case_timings[, "time_in_compartment"] <- case_timings[, "time_in_compartment"] + 0.01
  
  case_triggered <- case_timings[,"time_in_compartment"] > case_timings[, "threshold_time"]
  
  case_timings[case_triggered, "time_in_compartment"] <- 0
  for(i_case in which(case_triggered)) {
    case <- case_list[[i_case]]
    
    case_states[i %/% 100, i_case] <- case$trigger_transition()
    case_timings[i_case, "threshold_time"] <- case$get_compartment_threshold_time()
  }
}

case_states <- case_states %>% as_tibble() %>% fill(everything(), .direction = 'down') %>% as.matrix()


plot_state <- tibble(i = 1:365) %>%
  mutate(n_susceptible = matrixStats::rowCounts(case_states, value = compartment_indices["susceptible"]),
         n_symptomatic = matrixStats::rowCounts(case_states, value = compartment_indices["symptomatic"]),
         n_ward = matrixStats::rowCounts(case_states, value = compartment_indices["ward"]))

ggplot(plot_state) +
  geom_line(aes(x = i, y = n_symptomatic))

