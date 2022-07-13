


plot_state_results_capacity <- function(
  sim_results,
  
  state_modelled,
  forecast_dates, state_forecast_start,
  forecast_name,
  plot_dir
) {
  source("R/capacity_table.R")
  
  source("R/plotting/group_capacity_plot.R")
  
  limit_multipliers <- 1:4
  
  results_ward_ICU <- map_dfr(
    limit_multipliers,
    function(i_limit_mult) {
      
      results_ward <- group_capacity_plot(
        "ward", capacity_limits[[state_modelled]]$ward * i_limit_mult,
        sim_results, "Ward occupancy", "darkorchid", forecast_dates, state_forecast_start
      ) %>%
        pluck("table") %>%
        mutate(multiplier = i_limit_mult, group = "ward")
      
      
      results_ICU <- group_capacity_plot(
        "ICU", capacity_limits[[state_modelled]]$ICU * i_limit_mult, 
        sim_results, "ICU occupancy", "green4", forecast_dates, state_forecast_start
      ) %>%
        pluck("table") %>%
        mutate(multiplier = i_limit_mult, group = "ICU")
      
      bind_rows(
        results_ward,
        results_ICU
      )
    }
  )
    
  
  source("R/plotting/admission_capacity_plot.R")
  
  results_admission <- admission_capacity_plot(
    capacity_limits[[state_modelled]]$admissions,
    sim_results, state_modelled, forecast_dates, state_forecast_start
  ) %>%
    pluck("table") %>%
    mutate(multiplier = 1, group = "admissions")

  
  results_tbl <- bind_rows(
    results_ward_ICU,
    results_admission
  ) %>%
    mutate(state = state_modelled)
  
  results_tbl
}

