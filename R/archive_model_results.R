


archive_model_results <- function(
  sim_results,
  morbidity_trajectories_state,
  
  state_modelled,
  forecast_name,
  
  archive_dir
) {
  
  require(qs)
  
  
  archive_data <- list(
    "sim_results" = sim_results,
    "morbidity_trajectories_state" = morbidity_trajectories_state,
    "state_modelled" = state_modelled,
    "forecast_name" = forecast_name
  )
  
  file_out <- paste0(archive_dir, "/", state_modelled, "_archive.qs")
  
  qsave(
    archive_data,
    file = file_out,
    preset = "archive", 
    nthreads = 16
  )
  
  return(file_out)
}
