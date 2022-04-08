
plot_diagnostics <- function(
  case_trajectories,
  forecast_dates,
  nindss_state,
  plot_dir,
  
  state_modelled
) {
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  plot_forecast <- case_trajectories$curve_set[(t_start * 9 + 1):(t_start * 9 + n_days - t_start), ] %>%
    as_tibble() %>%
    mutate(t = (t_start + 1):n_days,
           
           date = t + forecast_dates$simulation_start) %>%
    
    pivot_longer(starts_with("sim"),
                 names_to = c("sample", "model"),
                 names_sep = "_") %>%
    
    mutate(sample = as.numeric(str_remove(sample, "sim")))
  
  
  
  plot_fc <- function(i_model) {
    
    ggplot(plot_forecast %>% filter(model == i_model, sample %% 50 == 0)) +
      geom_line(aes(x = date, y = value, group = sample), size = 0.1, alpha = 0.5) +
      
      xlab(NULL) + ylab(NULL) +
      ggtitle(NULL, str_c("Case nowcast and forecast (", i_model, ")")) +
      
      geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dotted') +
      
      theme_minimal()
  }
  
  cowplot::plot_grid(
    plot_fc("gar"), plot_fc("uoa"), plot_fc("moss"), plot_fc("dst"), ncol = 1
  )
  
  
  
  ggsave(
    paste0(plot_dir, "/diagnostics_", state_modelled, ".png"),
    bg = "white",
    height = 10, width = 12
  )
  
}
