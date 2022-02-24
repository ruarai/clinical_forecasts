
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
    read_ensemble_state(raw_ensemble, state_modelled),
    
    format = "fst"
  ),
  
  tar_target(
    nindss_state,
    nindss %>%
      filter(state == state_modelled),
    
    format = "fst"
  ),
  
  tar_target(
    linelist_state,
    get_state_linelist(
      state_modelled,
      nindss_state,
      
      NSW_linelist_path
    ),
    
    format = "fst"
  ),
  
  tar_target(
    linelist_state_date,
    get_state_linelist_date(
      state_modelled,
      NSW_linelist_path,
      
      forecast_dates$NNDSS
    )
  ),
  
  tar_target(
    known_occupancy_ts,
    make_occupancy_timeseries(
      linelist_state,
      c19data,
      state_modelled
    )
  ),
  
  
  tar_target(
    morbidity_estimates_state,
    make_morbidity_estimates(
      nindss_state,
      
      forecast_dates$NNDSS,
      forecast_dates$simulation_start,
      
      clinical_parameters,
      
      state_modelled,
      national_morbidity_estimates
    )
  ),
  
  
  tar_target(
    case_trajectories,
    make_case_trajectories(
      ensemble_state,
      local_cases_state,
      
      linelist_state,
      linelist_state_date,
      
      forecast_dates
    )
  ),
  
  
  tar_target(
    sim_results,
    
    run_progression_model(
      case_trajectories,
      nindss_state,
      
      morbidity_estimates_state,
      clinical_parameter_samples,
      
      forecast_dates
    ),
    format = "qs",
    resources = tar_resources(
      qs = tar_resources_qs(preset = "fast")
    ),
    
    memory = "transient",
    garbage_collection = TRUE
    
  ),
  
  tar_target(
    state_plots,
    
    plot_state_results(
      sim_results,
      
      state_modelled,
      forecast_dates,
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
  
  t_state_absenteeism,
  
  
  tar_target(
    state_diag_plots,
    
    plot_diagnostics(
      case_trajectories,
      forecast_dates,
      nindss_state,
      morbidity_estimates_state,
      plot_dir,
      
      state_modelled
    )
  )
)