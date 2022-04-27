make_absentee_case_trajectories <- function(
  ensemble_state,
  local_cases_state,
  nindss_state,
  
  forecast_dates,
  state_forecast_start
) {
  
  
  ensemble_curves_df <- ensemble_state %>%
    filter(date <= forecast_dates$forecast_horizon) %>%
    
    select(-c(state, forecast_origin)) %>%
    
    pivot_wider(names_from = ".model",
                values_from = starts_with("sim")) %>%
    
    mutate(across(starts_with("sim"), ~ round(.))) %>%
    
    arrange(date) %>%
    select(-date) %>%
    
    fill(ends_with("gar"), .direction = "up") # Hacky solution for when GAR model is a day late (??)
  
  
  
  
  na_cols <- map_lgl(1:ncol(ensemble_curves_df), ~ all(is.na(ensemble_curves_df[,.])))
  
  print(paste0("Dropping ", sum(na_cols), " columns for being entirely NA"))
  
  
  ensemble_curves <- as.matrix(ensemble_curves_df[, !na_cols])
  n_curves <- ncol(ensemble_curves)
  
  
  ## Nowcasting (during period of right truncation-ish)
  
  
  local_cases_to_impute <- local_cases_state %>%
    filter(date_onset > forecast_dates$simulation_start,
           date_onset <= state_forecast_start) %>%
    select(date_onset, count, detection_probability)
  
  nowcasting_case_curves <- local_cases_to_impute$count %>%
    matrix(nrow = length(.), ncol = n_curves)
  
  detection_probability <- local_cases_to_impute$detection_probability
  
  nowcasting_case_curves_imputed <- apply(
    nowcasting_case_curves,
    2,
    function(x) rnbinom(nrow(nowcasting_case_curves), size = 25, mu = x / detection_probability)
  )
  
  curve_set_not_yet_age_sampled <- rbind(
    nowcasting_case_curves_imputed,
    
    ensemble_curves
  )
  
  
  if(any(is.na(curve_set_not_yet_age_sampled))) {
    print("NA values in curve_set_not_yet_age_sampled, imputing...")
    
    curve_set_not_yet_age_sampled <- apply(curve_set_not_yet_age_sampled, 2, function(x) zoo::na.locf(x))
  }
  
  nindss_date_at_least_1000 <- nindss_state %>% 
    group_by(date_onset) %>% summarise(n = n()) %>% 
    arrange(desc(date_onset)) %>% mutate(n_c = cumsum(n)) %>%
    filter(n_c > 1000) %>% slice(1) %>% pull(date_onset)
  
  date_cutoff <- min(state_forecast_start - ddays(14), nindss_date_at_least_1000)
  
  nindss_recent <- nindss_state %>%
    filter(date_onset <= state_forecast_start,
           date_onset >= date_cutoff)
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  recent_age_dist <- nindss_recent %>%
    
    group_by(age_group) %>%
    summarise(pr_age_given_case = n() / nrow(.)) %>%
    
    complete(age_group = age_groups, fill = list(pr_age_given_case = 0)) %>%
    arrange(age_group)
  
  Rcpp::sourceCpp("cpp/repeated_rmultinom.cpp")
  curve_set_age_sampled <- apply(
    curve_set_not_yet_age_sampled,
    2,
    function(x) repeated_rmultinom(x, recent_age_dist$pr_age_given_case)
  )
  
  
  curve_set_age_sampled %>%
    as_tibble()
}




