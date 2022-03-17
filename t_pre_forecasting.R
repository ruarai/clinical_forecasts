
pre_forecasting <- c(
  t_parameters,
  list(
    tar_target(
      plot_dir,
      {
        dir <- str_c("results/", forecast_name, "/")
        dir.create(dir, showWarnings = FALSE)
        return(dir)
      }
    ),
    
    tar_target(
      backup_dir,
      {
        path <- str_c("historical_inputs/", date_forecasting)
        dir.create(path, showWarnings = FALSE)
        return(path)
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
    
    
    tar_target(c19data, { print(date_reporting_line); get_c19data() }),

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
      format = "fst"
    ),
    # 
    # tar_target(raw_local_cases, paste0(backup_dir, "/local_cases.csv")),
    # tar_target(raw_ensemble, paste0(backup_dir, "/ensemble.csv")),
    # tar_target(nindss, fst::read_fst(paste0(backup_dir, "/nindss.fst"))),
    
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
      morbidity_trajectories_national,
      
      get_time_varying_morbidity_estimations(
        nindss,
        
        forecast_dates,
        
        clinical_parameters,
        
        "national",
        
        NULL
      ),

      garbage_collection = TRUE # Clear memory after reading NINDSS
    )
  ),
  
  
  t_backup_inputs
)