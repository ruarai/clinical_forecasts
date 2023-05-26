
t_forecast <- list(
  pre_forecasting,
  
  state_results,
  
  tar_combine(forecast_starts, state_results[["state_forecast_start_tbl"]],
              command = dplyr::bind_rows(!!!.x)),
  
  tar_combine(all_state_trajs, state_results[["state_results_traj"]],
              command = dplyr::bind_rows(!!!.x), format = "fst"),
  
  tar_target(
    backup_trajs,
    {
      file_path <- paste0(plot_dir, "/trajectories.fst")
      fst::write_fst(all_state_trajs, path = file_path, compress = 100)
      
      if(do_upload_trajectories) {
        sync_file <- paste0("/home/forecast/mfluxunimelb/clinical_forecasts/trajectories/trajectories_", forecast_name, ".fst")
        file.copy(file_path, sync_file, overwrite = TRUE)
      }
      
      return(file_path)
    },
    
    format = "file",
    deployment = "main"
  ),
  
  tar_target(
    backup_fc_dates,
    {
      file_path <- paste0(plot_dir, "/forecast_dates.csv")
      forecast_dates %>%
        write_csv(file_path)
      return(file_path)
    },
    
    format = "file",
    deployment = "main"
  )
  #,
  
  # tar_combine(all_state_ABC_parameters, state_results[["state_ABC_parameters"]]),
  # tar_combine(all_state_ABC_diagnostics, state_results[["state_ABC_diagnostics"]]),
  
  # tar_target(
  #   ABC_diagnostic_plots,
  #   plot_ABC_diagnostics(all_state_ABC_diagnostics, all_state_ABC_parameters, plot_dir),
  #   
  #   format = "file",
  #   
  #   deployment = "main"
  # ),
  
  # tar_combine(all_state_capacity, state_results[["state_capacity_table"]],
  #             command = dplyr::bind_rows(!!!.x)),
  # 
  # tar_target(
  #   state_capacity_report,
  #   {
  #     file_out <- paste0(plot_dir, "/clinical_capacity_", date_forecasting ,".csv")
  #     all_state_capacity %>% 
  #       filter(date == forecast_dates$forecast_horizon - ddays(1)) %>%
  #       select(state, group, multiplier, date, prob = y_adj) %>%
  #       
  #       write_csv(file_out)
  #     
  #   },
  #   
  #   cue = tar_cue_skip(is_retro),
  #   deployment = "main"
  #   
  # )
)



