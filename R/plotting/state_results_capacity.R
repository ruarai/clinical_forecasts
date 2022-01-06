


plot_state_results_capacity <- function(
  sim_results,
  
  state_modelled,
  forecast_dates,
  forecast_name,
  plot_dir
) {
  capacity_limits <- list(
    "ACT" = list("ward" = 488,  "ICU" = 42,  "admissions" = 202),
    "NSW" = list("ward" = 8832, "ICU" = 782, "admissions" = 3945),
    "NT"  = list("ward" = 276,  "ICU" = 24,  "admissions" = 172),
    "QLD" = list("ward" = 5099, "ICU" = 329, "admissions" = 2071),
    "SA"  = list("ward" = 1915, "ICU" = 129, "admissions" = 694),
    "TAS" = list("ward" = 557,  "ICU" = 41,  "admissions" = 222),
    "VIC" = list("ward" = 6158, "ICU" = 380, "admissions" = 2456),
    "WA"  = list("ward" = 2471, "ICU" = 121, "admissions" = 1173)
  )
  
  source("R/plotting/group_capacity_plot.R")
  
  results_ward <- group_capacity_plot(
    "ward", capacity_limits[[state_modelled]]$ward,
    sim_results, "Ward occupancy", "darkorchid", forecast_dates
  )
  
  results_ICU <- group_capacity_plot(
    "ICU", capacity_limits[[state_modelled]]$ICU, 
    sim_results, "ICU occupancy", "green4", forecast_dates
  )
  
  source("R/plotting/admission_capacity_plot.R")
  
  results_admission <- admission_capacity_plot(
    capacity_limits[[state_modelled]]$admissions,
    sim_results, state_modelled, forecast_dates
  )
  
  
  cowplot::plot_grid(
    results_admission$plot, NULL, results_ward$plot, NULL, results_ICU$plot,
    ncol = 5, rel_widths = c(0.5, 0.01, 0.5, 0.01, 0.5)
  )
  
  
  scale <- 9
  ggsave(paste0(plot_dir, state_modelled, "_capacity.png"),
         bg = 'white',
         width = 2.7 * scale, height = 1 * scale,
         dpi = 200)
  
  
  results_tbl <- bind_rows(
    results_ward$table %>% mutate(group = "ward"),
    results_ICU$table %>% mutate(group = "ICU"),
    results_admission$table %>% mutate(group = "admissions")
  ) %>%
    mutate(state = state_modelled)
  
  results_tbl
}

