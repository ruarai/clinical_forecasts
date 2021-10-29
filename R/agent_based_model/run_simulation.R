





run_simulation <- function(case_linelist) {
  case_list <- vector(mode = "list", length = n_cases)
  
  for(i in 1:nrow(case_linelist)) {
    i_row <- case_linelist[i,]
    
    case_params <- list(
      time_of_infection = i_row$t,
      delay_
    )
    
    
    case <- CovidCase$new(i_row$age_class, i_row$vaccine,
                          time_of_infection = i_row$t)
    
    
    case_list[[i]] <- case
  }
  
  Rcpp::sourceCpp("R/agent_based_model/abm_loop.cpp")
  results <- process_loop(case_list, days_sim, dt = 0.1)
  
  
  
  
  tbl_transitions <- do.call(rbind, results$transitions) %>%
    `colnames<-`(c("case_index", "t", "new_comp")) %>%
    as_tibble() %>%
    mutate(new_comp = names(compartment_indices)[new_comp]) %>%
    group_by(t, new_comp) %>%
    summarise(n = n(), .groups = "drop")
  
  
  tbl_count <- apply(results$compartment_counts, MARGIN = 2, cumsum) %>%
    `colnames<-`(names(compartment_indices)) %>%
    as_tibble() %>%
    
    mutate(t = row_number()) %>%
    
    pivot_longer(cols = -t, names_to = "compartment", values_to = "count")
  
  
  summary_groups <- list("ward" = c("ward", "postICU_to_death", "postICU_to_discharge"), 
                         "ICU" = c("ICU"),
                         "died" = c("ward_died", "ICU_died", "postICU_died"),
                         "discharged" = c("ward_discharged", "postICU_discharged")) %>%
    map_dfr(~ tibble(compartment = .), .id="group")  %>%
    select(compartment, group) %>% deframe()
  
  tbl_count_grouped <- tbl_count %>%
    mutate(group = summary_groups[compartment]) %>%
    
    group_by(t, group) %>%
    summarise(count = sum(count), .groups = "drop")
  
  tbl_transitions_grouped <- tbl_transitions %>%
    mutate(group = summary_groups[new_comp]) %>%
    
    group_by(t, group) %>%
    summarise(count = sum(n), .groups = "drop")
  
  
  
  return(list(
    tbl_count = tbl_count,
    tbl_count_grouped = tbl_count_grouped,
    tbl_transitions = tbl_transitions,
    tbl_transitions_grouped = tbl_transitions_grouped
  ))
}