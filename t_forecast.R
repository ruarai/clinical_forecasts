
t_forecast <- list(
  pre_forecasting,
  
  state_results,
  
  
  tar_combine(all_state_absenteeism_trajs, state_results[["absentee_trajs"]],
    command = dplyr::bind_rows(!!!.x), format = "fst"),
  
  tar_target(
    absenteeism_plots,
    
    make_absenteeism_plots(all_state_absenteeism_trajs, forecast_dates, plot_dir)
  ),
  
  tar_combine(all_state_quants, state_results[["state_result_quants"]],
    command = dplyr::bind_rows(!!!.x), format = "fst"),
  
  tar_combine(all_state_trajs, state_results[["state_results_traj"]],
    command = dplyr::bind_rows(!!!.x), format = "fst"),
  
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
  
  tar_combine(all_state_known_occupancy_ts, state_results[["known_occupancy_ts"]],
    command = dplyr::bind_rows(!!!.x)),
  
  tar_combine(all_state_capacity, state_results[["state_capacity_table"]],
    command = dplyr::bind_rows(!!!.x)),
  
  tar_combine(all_state_ABC_parameters, state_results[["state_ABC_parameters"]]),
  tar_combine(all_state_ABC_diagnostics, state_results[["state_ABC_diagnostics"]]),
  
  
  tar_target(
    ABC_diagnostic_plots,
    plot_ABC_diagnostics(all_state_ABC_diagnostics, all_state_ABC_parameters, plot_dir)
  ),
  
  
  tar_target(
    national_plots,
    
    plot_joint_results(
      all_state_quants,
      all_state_known_occupancy_ts,
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