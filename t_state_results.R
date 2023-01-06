
state_results <- tar_map(
  values = state_tbl,
  unlist = FALSE,
  
  tar_target(
    local_cases_state,
    read_csv(raw_local_cases, show_col_types = FALSE) %>%
      filter(state == state_modelled) %>% 
      rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x)),
    deployment = "main"
  ),
  
  
  tar_target(
    ensemble_state,
    read_ensemble_state(raw_ensemble, state_modelled, models_included),
    
    format = "fst",
    deployment = "main"
  ),
  
  tar_target(
    nindss_state,
    nindss %>%
      filter(state == state_modelled),

    format = "fst"
  ),
  
  tar_target(
    known_occupancy_ts,
    occupancy_data %>% filter(state == state_modelled)
  ),
  
  tar_target(
    state_forecast_start,
    get_state_forecast_start(local_cases_state, state_modelled)
  ),
  tar_target(state_forecast_start_tbl, tibble(date = state_forecast_start, state = state_modelled)),
  
  
  tar_target(
    case_trajectories,
    make_case_trajectories(
      ensemble_state,
      local_cases_state,

      forecast_dates,
      state_forecast_start
    )
  ),
  
  # tar_target(
  #   case_trajectories,
  #   make_case_trajectories_oracle(
  #     read_csv("~/mfluxunimelb/local_cases_input/local_cases_input_2022-08-23.csv", show_col_types = FALSE),
  #     local_cases_state,
  # 
  #     forecast_dates,
  #     state_forecast_start
  #   )
  # ),

  t_state_results_immunity,
  
  tar_target(
    unadjusted_morbidity_trajectories_state,

    get_time_varying_morbidity_estimations(
      nindss_state,

      forecast_dates,

      clinical_parameters,

      state_modelled,
      nindss_bad_states,

      morbidity_trajectories_national,
      morbidity_window_width
    )
  ),

  tar_target(
    morbidity_trajectories_state,
    adjust_morbidity_trajectories(
      is_longterm,
      immune_predictions_state,
      unadjusted_morbidity_trajectories_state,
      forecast_dates,
      state_modelled
    ),
    format = "fst_tbl"
  ),
  
  
  # tar_target(
  #   morbidity_trajectories_state,
  #   
  #   #get_time_varying_as_known(date_forecasting, forecast_dates),
  #   get_time_varying_with_future(date_forecasting, forecast_dates),
  #   format = "fst_tbl"
  # ),
  
  tar_target(
    morbidity_trajectories_plot,
    plot_morbidity_trajectories(
      morbidity_trajectories_state,
      state_modelled,
      forecast_dates,
      morbidity_window_width,
      plot_dir
    )
  ),
  

  

  
  tar_target(
    sim_thresholds,
    if(is_longterm) {
      c(0.2, 0.3, 0.5, 1, 10, 1000)
    } else {
      c(0.1, 0.2, 0.3, 0.5, 1, 10, 1000)
    }
  ),
  
  tar_target(
    sim_results,
    
    run_progression_model(
      case_trajectories,
      known_occupancy_ts,
      
      morbidity_trajectories_state,
      clinical_parameter_samples,
      
      forecast_dates,
      state_forecast_start,
      
      state_modelled,
      n_traj_out,
      
      thresholds = sim_thresholds,
      do_ABC = use_fitting
    ),
    format = "qs",
    resources = tar_resources(
      qs = tar_resources_qs(preset = "fast")
    ),
    
    memory = "transient",
    garbage_collection = TRUE
  ),
  
  
  tar_target(
    sim_results_prior,
    
    run_progression_model(
      case_trajectories,
      known_occupancy_ts,
      
      morbidity_trajectories_state,
      clinical_parameter_samples,
      
      forecast_dates,
      state_forecast_start,
      
      state_modelled,
      n_traj_out,
      
      do_ABC = FALSE
    ),
    format = "qs",
    resources = tar_resources(
      qs = tar_resources_qs(preset = "fast")
    ),
    
    memory = "transient",
    garbage_collection = TRUE
  ),
  
  tar_target(
    prior_posterior_plots,
    make_prior_posterior_plot(
      sim_results_prior, sim_results,
      
      forecast_dates, state_forecast_start,
      known_occupancy_ts,
      plot_dir, state_modelled
    ),
    format = "file",
    deployment = "main"
  ),
  
  tar_target(state_ABC_parameters, sim_results$ABC_parameters %>% mutate(state = state_modelled)),
  tar_target(state_ABC_diagnostics, sim_results$ABC_fit_diagnostics %>% mutate(state = state_modelled)),
  
  tar_target(
    state_plots,
    
    plot_state_results(
      sim_results,
      
      state_modelled,
      forecast_dates, 
      state_forecast_start,
      forecast_name,
      plot_dir
    ),
    deployment = "main"
  ),
  
  
  tar_target(
    state_capacity_table,
    
    plot_state_results_capacity(
      sim_results,
      
      state_modelled,
      forecast_dates,
      state_forecast_start,
      forecast_name,
      plot_dir
    )
  ),
  
  
  tar_target(
    state_result_quants,
    
    sim_results$quants_count %>%
      mutate(state = state_modelled),
    
    format = "fst"
  ),
  
  tar_target(
    state_results_traj,
    
    sim_results$trajectories %>%
      filter(group %in% c("ward", "ICU")) %>%
      mutate(state = state_modelled),
    
    format = "fst"
  ),
  
  tar_target(
    state_archive,
    
    archive_model_results(
      sim_results,
      morbidity_trajectories_state,
      immune_predictions_state,
      state_modelled,
      forecast_name,
      archive_dir
    ),
    
    format = "file",
    deployment = "main"
  )
)
