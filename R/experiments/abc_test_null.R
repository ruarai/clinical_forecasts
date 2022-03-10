

results_null <- curvemush::mush_abc(
  n_samples = 4000,
  n_delay_samples = 512,
  
  n_outputs = 1000,
  
  n_days = case_trajectories$n_days,
  steps_per_day = 16,
  
  ward_threshold = 1000000,
  ICU_threshold = 1000000,
  
  prior_sigma_los = 0,
  prior_sigma_hosp = 0,
  
  t_forecast_start = case_trajectories$step_sampling_start,
  
  ensemble_curves = case_curves,
  
  forecasting_parameters = forecasting_parameters,
  
  known_ward_vec = occupancy_curve_match$ward_vec,
  known_ICU_vec = occupancy_curve_match$ICU_vec,
  
  mat_pr_age_given_case = mat_pr_age_given_case,
  mat_pr_hosp = mat_pr_hosp,
  mat_pr_ICU = mat_pr_ICU
)


results_count_quants_null <- results_null$grouped_results %>%
  select(-c(transitions, los_scale, pr_hosp_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()
