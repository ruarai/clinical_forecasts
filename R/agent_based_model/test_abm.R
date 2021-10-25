
source("R/model_parameters.R")
source("R/agent_based_model/covid_case.R")
model_params <- get_model_parameters()



local_cases <- read_csv("data/input/local_cases_input.csv") %>%
  mutate(t = as.numeric(365 - (lubridate::today() - date_onset))) %>%
  filter(state == "VIC", date_onset >= lubridate::today() - 365)


compartment_indices <- c(
  "susceptible" = 1,
  "symptomatic" = 2,
  "ward" = 3
)

n_cases <- sum(local_cases$count)

case_list <- vector(mode = "list", length = n_cases)
case_timings <- matrix(0, ncol = 2, nrow = n_cases) %>%
  `colnames<-`(c("time_in_compartment", "threshold_time"))

i_case <- 1
for(i in 1:nrow(local_cases)) {
  i_row <- local_cases[i,]
  
  if(i_row$count > 0) {
    for(j in 1:i_row$count){
      case <- CovidCase$new("0-4", "pf1", time_of_infection = i_row$t,
                            model_params, index = i_case)
      
      case_list[[i_case]] <- case
      case_timings[i_case, "threshold_time"] <- case$get_compartment_threshold_time()
      
      i_case <- i_case + 1
    }
  }
}



Rcpp::sourceCpp("R/agent_based_model/abm_loop.cpp")

a <- Sys.time()
process_loop(case_list, case_timings[, "threshold_time"])
print(Sys.time() - a)