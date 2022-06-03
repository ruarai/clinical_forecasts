
state_results <- tar_map(
  values = state_tbl,
  unlist = FALSE,
  
  tar_target(
    local_cases_state,
    read_csv(raw_local_cases, show_col_types = FALSE) %>%
      filter(state == state_modelled)
  ),
  
  
  tar_target(
    ensemble_state,
    read_ensemble_state(raw_ensemble, state_modelled, models_included),
    
    format = "fst"
  ),
  
  tar_target(
    nindss_state,
    nindss %>%
      filter(state == state_modelled),
    
    format = "fst"
  ),
  
  tar_target(
    known_occupancy_ts,
    make_occupancy_timeseries(
      c19data,
      NULL,
      
      state_modelled
    )
  ),
  
  tar_target(
    state_forecast_start,
    get_state_forecast_start(local_cases_state, state_modelled)
  ),
  tar_target(state_forecast_start_tbl, tibble(date = state_forecast_start, state = state_modelled)),
  
  tar_target(
    morbidity_trajectories_state,
    
    get_time_varying_morbidity_estimations(
      nindss_state,
      
      forecast_dates,
      
      clinical_parameters,
      
      state_modelled,
      nindss_bad_states,
      
      morbidity_trajectories_national
    )
  ),
  
  tar_target(
    morbidity_trajectories_plot,
    
    plot_morbidity_trajectories(
      morbidity_trajectories_state,
      
      state_modelled,
      plot_dir
    )
  ),
  

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
  #     read_csv("historical_inputs/2022-05-19/local_cases.csv", show_col_types = FALSE),
  #     local_cases_state,
  # 
  #     forecast_dates,
  #     state_forecast_start
  #   )
  # ),
  
  
  tar_target(
    sim_results,
    
    run_progression_model(
      case_trajectories,
      known_occupancy_ts,
      
      morbidity_trajectories_state,
      clinical_parameter_samples,
      
      forecast_dates,
      state_forecast_start,
      
      state_modelled
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
    )
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
    )
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
  )
  
  #t_state_absenteeism,
  
  
  # tar_target(
  #   state_diag_plots,
  #   
  #   plot_diagnostics(
  #     case_trajectories,
  #     forecast_dates,
  #     nindss_state,
  #     morbidity_estimates_state,
  #     plot_dir,
  #     
  #     state_modelled
  #   )
  # )
)