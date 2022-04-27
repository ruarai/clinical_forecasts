
make_absenteeism_plots <- function(
  absenteeism_trajs,
  forecast_dates,
  forecast_starts,
  
  plot_dir
  
) {
  
  source("R/absenteeism/traj_plotting.R")
  
  summ_across_age <- absenteeism_trajs %>%
    group_by(state, date) %>%
    summarise(across(starts_with("sim"), ~ sum(.)), .groups = "drop")
  
  plot_lims <- tribble(
    ~state, ~y_lim,
    "WA", 150000
  )
  
  quant_state_data <- summ_across_age %>%
    make_results_quants() %>%
    
    left_join(plot_lims) %>%
    
    mutate(upper = if_else(is.na(y_lim), upper, pmin(upper, y_lim)),
           lower = if_else(is.na(y_lim), lower, pmin(lower, y_lim)))
  
  
  p_common <- list(
    scale_y_continuous(
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    ),
    theme(legend.position = "none"),
    coord_cartesian(xlim = c(ymd("2021-12-01"), NA)),
    xlab(NULL),
    ylab("Count")
  )
  
  quant_national_data <- summ_across_age %>%
    group_by(date) %>%
    summarise(across(starts_with("sim"), ~ sum(.)), .groups = "drop") %>%
    make_results_quants()
  
  
  
  ggplot(quant_state_data) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    geom_vline(aes(xintercept = date), data = forecast_starts, alpha = 0.5) +
    
    scale_fill_brewer(palette = "PuBu") +
    
    facet_wrap(~state, ncol = 2,
               scales = "free_y") +
    
    theme_minimal() +
    
    p_common 
  
  
  
  
  ggsave(paste0(plot_dir, "/_absenteeism_state.png"),
         bg = "white",
         width = 8, height = 8)
  
  
  ggplot(quant_national_data) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    geom_vline(xintercept = min(forecast_starts$date), alpha = 0.5) +
    
    scale_fill_brewer(palette = "PuBu") +
    
    theme_minimal() +
    
    p_common 
  
  ggsave(paste0(plot_dir, "/_absenteeism_national.png"),
         bg = "white",
         width = 8, height = 5)
}


