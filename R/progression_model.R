
run_progression_model <- function(
  case_trajectories,
  clinical_table_state,
  nindss_state,
  
  forecast_dates
) {
  require(tidyverse)
  require(lubridate)
  
  require(curvemush)
  
  
  clinical_table_ordered <- clinical_table_state %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset <= forecast_dates$forecast_horizon) %>%
    arrange(date_onset, age_group)
  
  forecasting_clinical_table <- clinical_table_ordered %>%
    filter(date_onset == max(date_onset)) %>%
    select(age_group, pr_hosp, pr_ICU)
  
  
  clinical_parameters <- tar_read(clinical_parameters)
  
  # Make our estimated pr_ICU compatible with estimates from our one-off analysis
  forecasting_table_adj_ICU <- clinical_parameters %>%
    select(age_group, pr_ward_to_ICU, pr_ward_to_discharge) %>%
    mutate(pr_ward_to_death = 1 - pr_ward_to_ICU - pr_ward_to_discharge) %>%
    
    left_join(forecasting_clinical_table %>% select(age_group, pr_ICU), by = "age_group") %>%
    
    mutate(pr_not_ICU_true = 1 - pr_ICU,
           pr_ward_to_discharge_given_not_ICU = pr_ward_to_discharge / (pr_ward_to_discharge + pr_ward_to_death)) %>%
    
    mutate(pr_ward_to_discharge = pr_ward_to_discharge_given_not_ICU * pr_not_ICU_true,
           pr_ward_to_ICU = pr_ICU) %>%
    
    select(age_group, pr_ward_to_discharge, pr_ward_to_ICU)
  
  
  nindss_recent <- nindss_state %>%
    filter(date_onset <= forecast_dates$forecast_start,
           date_onset > forecast_dates$forecast_start - ddays(14))
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  recent_age_dist <- nindss_recent %>%
    group_by(age_group) %>%
    summarise(pr_age_given_case = n() / nrow(.)) %>%
    
    complete(age_group = age_groups, fill = list(pr_age_given_case = 0))
  
  forecasting_parameters <- clinical_parameters %>%
    select(-c(pr_ward_to_discharge, pr_ward_to_ICU)) %>%
    
    left_join(forecasting_table_adj_ICU, by = "age_group") %>%
    
    left_join(forecasting_clinical_table %>% select(-pr_ICU), by = 'age_group') %>%
    left_join(recent_age_dist, by = 'age_group')
  
  case_curves <- case_trajectories$curve_set
  
  
  a <- Sys.time()
  results <- curvemush::mush(
    n_samples = 8000,
    n_delay_samples = 512,
    
    n_days = case_trajectories$n_days,
    steps_per_day = 16,
    
    t_forecast_start = case_trajectories$step_sampling_start,
    
    ensemble_curves = case_curves,
    
    forecasting_parameters = forecasting_parameters
  )
  b <- Sys.time()
  
  print(str_c("Simulation mush ran in ", round(b - a, 2), " ", units(b - a)))
  
  
  group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
  compartment_labels <- c(
    "symptomatic_clinical", "ward", "discharged_ward", "died_ward", "ICU",
    "discharged_ICU", "died_ICU", "postICU_to_discharge", "postICU_to_death",
    "discharged_postICU", "died_postICU"
  )
  
  format_grouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           group = group_labels[compartment_group + 1])
  
  format_ungrouped <- . %>%
    mutate(date = forecast_dates$simulation_start + ddays(t_day),
           compartment = compartment_labels[compartment + 1])
    
    
  
  results_count_quants <- results$grouped_results %>%
    select(-transitions) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_count_quants <- results$results %>%
    select(-transitions) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  results_transitions_quants <- results$grouped_results %>%
    select(-count) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_grouped()
  
  results_ungrouped_transitions_quants <- results$results %>%
    select(-count) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "transitions") %>%
    make_results_quants() %>%
    format_ungrouped()
  
  
  results_formatted <- results$grouped_results  %>%
    format_grouped()
  
  results_ungrouped_formatted <- results$results  %>%
    format_ungrouped()
  
  list(
    trajectories = results_formatted,
    quants_count = results_count_quants,
    quants_transitions = results_transitions_quants,
    
    trajectories_ungrouped = results_ungrouped_formatted,
    quants_ungrouped_count = results_ungrouped_count_quants,
    quants_ungrouped_transitions = results_ungrouped_transitions_quants
  )
}



make_results_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}
