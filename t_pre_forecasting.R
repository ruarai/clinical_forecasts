
pre_forecasting <- c(
  t_parameters,
  list(
    tar_target(
      plot_dir,
      {
        dir <- str_c("results/", forecast_name, "/")
        dir.create(dir, showWarnings = FALSE)
        return(dir)
      },
      deployment = "main"
    ),
    
    tar_target(
      archive_dir,
      {
        path <- str_c(plot_dir, "archive/")
        dir.create(path, showWarnings = FALSE)
        return(path)
      },
      deployment = "main"
    ),
    
    tar_target(
      clinical_parameters_means_file,
      "../los_analysis_competing_risks/results/NSW_2022-05-03_omi_primary/clinical_parameters.csv",
      format = "file"
    ),
    
    tar_target(
      clinical_parameter_samples_file,
      "../los_analysis_competing_risks/results/NSW_2022-05-03_omi_primary/estimate_samples_share_wide.csv",
      format = "file"
    ),
    
    tar_target(
      clinical_parameters, 
      {
        read_csv(
          clinical_parameters_means_file,
          show_col_types = FALSE
        ) %>%
          # Can't produce meaningful onset-to-ward estimates from the NSW data as-is, so use Delta estimates (via CamWalk, somehow) (7/02/2022)
          mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41,
                                         3.35, 3.35, 3.24, 3.24) * 0.2,
                 shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                         1.7, 1.9, 1.9, 1.3) * 0.2)
      }
    ),
    
    tar_target(
      clinical_parameter_samples, {
        read_csv(
          clinical_parameter_samples_file
        ) %>%
          mutate(scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41,
                                         3.35, 3.35, 3.24, 3.24) %>% rep(times = 1000) * 0.2,
                 shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7,
                                         1.7, 1.9, 1.9, 1.3) %>% rep(times = 1000) * 0.2)
      
      }
    ),
    
    
    tar_target(occupancy_data, read_occupancy_data(occupancy_path)),
    
    tar_target(
      nindss,
      process_NINDSS_linelist(raw_nindss, date_simulation_start),
      format = "fst",
      deployment = "main"
    ),

    tar_target(
      forecast_dates,
      make_forecast_dates(
        date_file_limit = date_forecasting,
        date_simulation_start = date_simulation_start,
        
        local_cases_file = raw_local_cases,
        
        nindss_path = raw_nindss
      ),
      deployment = "main"
    ),
    
    tar_target(morbidity_window_width, 14),

    tar_target(
      morbidity_trajectories_national,

      get_time_varying_morbidity_estimations(
        nindss, nindss,

        forecast_dates,

        clinical_parameters,

        "national",
        nindss_bad_states,

        NULL,
        morbidity_window_width
      ),

      garbage_collection = TRUE # Clear memory after reading NINDSS
    )
  ),
  
  t_backup_inputs
)