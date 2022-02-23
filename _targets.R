
source("_targets_dependencies.R")

state_tbl <- tibble(state_modelled = c("VIC", "ACT", "QLD", "NSW", "NT", "WA", "SA", "TAS"))



pre_forecasting <- list(
  
  tar_target(date_forecasting, ymd("2022-02-22")),
  tar_target(date_reporting_line, ymd("2022-02-22")),
  
  tar_target(NSW_linelist_path, "~/data_private/NSW/NSW_out_episode_2022_02_15.xlsx"),
  
  
  tar_target(date_simulation_start, ymd("2021-07-01")),
  tar_target(forecast_name, str_c("fc_", date_forecasting, "_test_sampled")),
  
  tar_target(
    plot_dir,
    {
      dir <- str_c("results/", forecast_name, "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
    }
  ),
  
  
  tar_target(latest_mflux_files, get_latest_mflux_files(date_forecasting)),
  
  
  tar_target(
    clinical_parameters, 
    {
      read_csv(
        "/home/forecast/source/los_analysis_competing_risks/results/NSW_2022-02-08_omi_mix/clinical_parameters.csv",
        show_col_types = FALSE
      ) %>%
        # Can't produce meaningful onset-to-ward estimates from the NSW data as-is, so use Delta estimates (via JWalk, somehow) (7/02/2022)
        mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41, 
                                       3.35, 3.35, 3.24, 3.24),
               shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                       1.7, 1.9, 1.9, 1.3))
    }
  ),
  
  tar_target(
    clinical_parameter_samples, {
      read_csv(
        "../los_analysis_competing_risks/results/NSW_2022-02-08_omi_mix/estimate_samples_share_wide.csv"
      ) %>%
        mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41, 
                                       3.35, 3.35, 3.24, 3.24) %>% rep(times = 1000),
               shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                       1.7, 1.9, 1.9, 1.3) %>% rep(times = 1000))
    }
  ),
  
  
  tar_target(c19data, get_c19data()),
  
  tar_target(
    raw_local_cases,
    download_mflux_file(
      remote_file = latest_mflux_files$local_cases$file,
      local_file = "data/mflux/downloads/raw_local_cases.csv"
    ),
    format = "file"),
  
  tar_target(
    raw_nindss,
    download_mflux_file(
      remote_file = latest_mflux_files$nindss$file,
      local_file = "data/mflux/downloads/raw_nindss.xlsx"
    ),
    format = "file"),
  
  tar_target(
    raw_ensemble,
    download_mflux_file(
      remote_file = latest_mflux_files$ensemble$file,
      local_file = "data/mflux/downloads/raw_ensemble.csv"
    ),
    format = "file"),
  
  
  tar_target(
    nindss,
    process_NINDSS_linelist(raw_nindss, date_simulation_start),
    format = "qs",
    resources = tar_resources(
      qs = tar_resources_qs(preset = "fast")
    )
  ),
  
  
  tar_target(
    forecast_dates,
    make_forecast_dates(
      date_file_limit = date_forecasting,
      date_simulation_start = date_simulation_start,
      
      local_cases_file = raw_local_cases,
      latest_mflux_files = latest_mflux_files
    )
  ),
  
  
  tar_target(
    national_morbidity_estimates,
    
    make_morbidity_estimates(
      nindss,
      forecast_dates$NNDSS,
      forecast_dates$simulation_start,
      clinical_parameters,
      "national",
      
      NULL
    ),
    
    garbage_collection = TRUE # Clear memory after reading NINDSS
  )
)

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
    format = "qs",
    resources = tar_resources(
      qs = tar_resources_qs(preset = "fast")
    )
  ),
  
  tar_target(
    nindss_state,
    nindss %>%
      filter(state == state_modelled)
  ),
  
  tar_target(
    linelist_state,
    get_state_linelist(
      state_modelled,
      nindss_state,
      
      NSW_linelist_path
    )
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
      mutate(state = state_modelled)
  ),
  
  tar_target(
    state_results_traj,
    
    sim_results$trajectories %>%
      filter(group %in% c("ward", "ICU")) %>%
      mutate(state = state_modelled)
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


t_forecast <- list(
  pre_forecasting,
  
  state_results,
  
  
  tar_combine(
    all_state_absenteeism_trajs,
    state_results[["absentee_trajs"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_target(
    absenteeism_plots,
    
    make_absenteeism_plots(all_state_absenteeism_trajs, forecast_dates, plot_dir)
  ),
  
  tar_combine(
    all_state_quants,
    state_results[["state_result_quants"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_combine(
    all_state_trajs,
    state_results[["state_results_traj"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_target(
    backup_trajs,
    {
      file_path <- paste0(plot_dir, "/trajectories.fst")
      fst::write_fst(all_state_trajs, path = file_path, compress = 100)
      return(file_path)
    },
    
    format = "file"
  ),
  
  tar_target(
    backup_fc_dates,
    {
      file_path <- paste0(plot_dir, "/forecast_dates.csv")
      forecast_dates %>%
        write_csv(file_path)
      return(file_path)
    },
    
    format = "file"
  ),
  
  tar_combine(
    all_state_known_occupancy_ts,
    state_results[["known_occupancy_ts"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_combine(
    all_state_capacity,
    state_results[["state_capacity_table"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_target(
    national_plots,
    
    plot_joint_results(
      all_state_quants,
      forecast_dates,
      date_reporting_line,
      
      forecast_name,
      plot_dir
    )
  ),
  
  
  tar_target(
    state_capacity_report,
    
    all_state_capacity %>% 
      filter(date == forecast_dates$forecast_horizon - ddays(1)) %>%
      select(state, group, date, prob = y_adj) %>%
      
      write_csv(paste0(plot_dir, "/clinical_capacity_", date_forecasting ,".csv"))
  )
)

t_forecast