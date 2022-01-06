


plot_state_results <- function(
  sim_results,
  
  state_modelled,
  forecast_dates,
  forecast_name,
  plot_dir
) {
  1
  
  source("R/plotting/state_results_diagnostic.R")
  plot_state_results_diagnostic(
    sim_results,
    
    state_modelled,
    forecast_dates,
    forecast_name,
    plot_dir
  )
}

