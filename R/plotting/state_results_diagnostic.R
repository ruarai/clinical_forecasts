plot_state_results <- function(
  sim_results,
  
  state_modelled,
  forecast_dates,
  state_forecast_start,
  forecast_name,
  plot_dir
) {
  plot_common <- list(
    geom_vline(xintercept = state_forecast_start,
               lty = 2, colour = "grey60"),
    
    scale_x_date(
      breaks = "months", labels = scales::label_date_short()
    ),
    scale_y_continuous(
      breaks = scales::breaks_extended(n = 5),
      labels = scales::label_comma()
    ),
    
    labs(caption = " "),
    
    theme_minimal()
  )
  
  
  p_count_trace <- ggplot(sim_results$trajectories %>%
                            filter(sample < 100)) +
    
    geom_line(aes(x = date, y = count, group = sample), alpha = 0.1) +
    
    facet_grid(rows = vars(group), scales = "free_y") +
    
    plot_common +
    
    ggtitle("Trace counts", "By compartment group")
  
  
  p_trans_trace <- ggplot(sim_results$trajectories %>%
                            filter(sample < 100)) +
    
    geom_line(aes(x = date, y = transitions, group = sample), alpha = 0.1) +
    
    facet_grid(rows = vars(group), scales = "free_y") +
    
    plot_common +
    
    ggtitle("Trace transitions (into)", " ") +
    labs(caption = forecast_name)
  
  
  cowplot::plot_grid(p_count_trace, p_trans_trace, nrow = 1)
  
  
  ggsave(
    paste0(plot_dir, state_modelled, "_trace_plots.png"),
    bg = "white",
    width = 8, height = 10
  )
  
  
  
  
  
  
  p_count_quant <- ggplot(sim_results$quants_count) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'blue2', alpha = 0.2) +
    
    facet_grid(rows = vars(group), scales = "free_y") +
    
    plot_common +
    
    ggtitle("Occupancy count quantiles", "By compartment group")
  
  p_trans_quant <- ggplot(sim_results$quants_transitions) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'blue2', alpha = 0.2) +
    
    facet_grid(rows = vars(group), scales = "free_y") +
    
    plot_common +
    
    ggtitle("Transition count quantiles", "") +
    labs(caption = forecast_name)
  
  
  cowplot::plot_grid(p_count_quant, p_trans_quant, nrow = 1)
  
  
  ggsave(
    paste0(plot_dir, state_modelled, "_quants_plots.png"),
    bg = "white",
    width = 8, height = 10
  )
}

