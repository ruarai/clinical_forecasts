


plot_state_results <- function(
  sim_results,
  
  state_modelled,
  forecast_dates,
  forecast_name,
  plot_dir
) {
  3
  source("R/plotting/state_results_diagnostic.R")
  plot_state_results_diagnostic(
    sim_results,
    
    state_modelled,
    forecast_dates,
    forecast_name,
    plot_dir
  )
  
  source("R/plotting/state_results_capacity_ED.R")
  plot_state_results_capacity_ED(
    sim_results,
    
    state_modelled,
    forecast_dates,
    forecast_name,
    plot_dir
  )
}

