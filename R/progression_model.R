
run_progression_model <- function(
  case_trajectories,
  known_occupancy_ts,
  
  morbidity_trajectories_state,
  clinical_parameter_samples,
  
  forecast_dates,
  
  state_modelled
) {
  
  require(tidyverse)
  require(lubridate)
  require(curvemush)
  
  if(state_modelled == "NSW") {
    source("R/experiments/adj_for_missing_RATs.R")
    
    morbidity_trajectories_state <- morbidity_trajectories_state %>%
      
      select(-c(pr_age_given_case, pr_hosp)) %>%
      
      left_join(
        adj_data %>%
          select(bootstrap, age_group, date, pr_age_given_case = pr_age_given_case_adj, pr_hosp = pr_hosp_adj) %>%
          rowwise() %>%
          mutate(age_group = case_when(
            age_group == "0-19" ~ list(c("0-9", "10-19")),
            age_group == "70+" ~ list(c("70-79", "80+")),
            TRUE ~ list(age_group)
          )) %>%
          ungroup() %>%
          unnest(age_group) %>%
          
          mutate(pr_age_given_case = case_when(
            age_group %in% c("0-9", "10-19", "70-79", "80+") ~ pr_age_given_case / 2,
            TRUE ~ pr_age_given_case
          ))
      ) %>%
      
      group_by(age_group, bootstrap) %>%
      arrange(date) %>%
      fill(pr_hosp, pr_ICU, pr_age_given_case, .direction = "downup") %>%
      ungroup()
  }
  
  
  
  mat_pr_age_given_case <- morbidity_trajectories_state %>%
    select(date, bootstrap, age_group, pr_age_given_case) %>%
    pivot_wider(names_from = bootstrap, values_from = pr_age_given_case) %>%
    
    arrange(date, age_group) %>% select(-c(date, age_group)) %>% as.matrix()
  
  
  mat_pr_hosp <- morbidity_trajectories_state %>%
    select(date, bootstrap, age_group, pr_hosp) %>%
    pivot_wider(names_from = bootstrap, values_from = pr_hosp) %>%
    
    arrange(date, age_group) %>% select(-c(date, age_group)) %>% as.matrix()
  
  mat_pr_ICU <- morbidity_trajectories_state %>%
    select(date, bootstrap, age_group, pr_ICU) %>%
    pivot_wider(names_from = bootstrap, values_from = pr_ICU) %>%
    
    arrange(date, age_group) %>% select(-c(date, age_group)) %>% as.matrix()
  
  
  
  
  occupancy_curve_match <- tibble(
    date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
  ) %>%
    mutate(do_match = date >= forecast_dates$forecast_start & date <= forecast_dates$forecast_start + ddays(7)) %>%
    
    left_join(
      
      known_occupancy_ts %>%
        filter(date >= forecast_dates$simulation_start) %>%
        select(date, group, count) %>%
        pivot_wider(names_from = group, values_from = count)
      
    ) %>%
    
    mutate(ward_vec = if_else(do_match, ward, -1),
           ICU_vec = if_else(do_match, ICU, -1))
    
  thresholds <- c(0.1, 0.2, 0.3, 0.5, 1, 10, 1000)
  
  print("Starting...")
  
  a <- Sys.time()
  prior_sigma_los <- 0.5
  prior_sigma_hosp <- 0.8
  
  results <- curvemush::mush_abc(
    n_samples = 8000,
   n_delay_samples = 512,
    
    n_outputs = 1000,
    
    n_days = case_trajectories$n_days,
    #steps_per_day = 16,
    steps_per_day = 4,
    
    thresholds_vec = thresholds,
    rejections_per_selections = 100,
    do_ABC = TRUE,

    prior_sigma_los = prior_sigma_los,
    prior_sigma_hosp = prior_sigma_hosp,
    # 
    # prior_sigma_los = 0,
    # prior_sigma_hosp = 0,
    
    ensemble_curves = case_trajectories$curve_set,
    
    forecasting_parameters = clinical_parameter_samples,
    
    known_ward_vec = occupancy_curve_match$ward_vec,
    known_ICU_vec = occupancy_curve_match$ICU_vec,
    
    mat_pr_age_given_case = mat_pr_age_given_case,
    mat_pr_hosp = mat_pr_hosp,
    mat_pr_ICU = mat_pr_ICU
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
  
  
  posterior_data <- tibble(
    pr_hosp_scale = results$prior_pr_hosp[results$prior_chosen + 1],
    los_scale = results$prior_los_scale[results$prior_chosen + 1],
    source = "posterior"
  )
  
  prior_data <- tibble(
    los_scale = rnorm(1000, sd = prior_sigma_los),
    pr_hosp_scale = rnorm(1000, sd = prior_sigma_hosp),
    source = "prior"
  )
  
  ABC_parameters <- bind_rows(posterior_data, prior_data)
  
  list(
    trajectories = results_formatted,
    quants_count = results_count_quants,
    quants_transitions = results_transitions_quants,
    
    trajectories_ungrouped = results_ungrouped_formatted,
    quants_ungrouped_count = results_ungrouped_count_quants,
    quants_ungrouped_transitions = results_ungrouped_transitions_quants,
    
    ABC_fit_diagnostics = tibble(thresholds = thresholds, accepted = results$n_accepted),
    ABC_parameters = ABC_parameters
  )
}



make_results_quants <- function(tbl, probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
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
