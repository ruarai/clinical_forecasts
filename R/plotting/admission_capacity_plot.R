
admission_capacity_plot <- function(
  capacity_limit,
  
  sim_results,
  
  state_modelled,
  forecast_dates, 
  state_forecast_start
) {
  forecast_weeks <- seq(state_forecast_start,
                        forecast_dates$forecast_horizon,
                        by = "weeks")
  
  plots_common <- list(
    geom_vline(xintercept = state_forecast_start,
               lty = 2, colour = "grey60"),
    scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)),
    cowplot::theme_cowplot(),
    theme(legend.position = "none",
          axis.text.x = element_text(angle=0),
          text=element_text(size=18),
          axis.text=element_text(size=14),
          strip.background =element_rect(fill="white"))
  )
  
  p1 <- ggplot(sim_results$quants_ungrouped_transitions %>%
                 filter(compartment == "ward")) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = '#009E73', alpha = 0.2) +
    
    geom_hline(yintercept = capacity_limit,
               linetype = 'dashed') +
    
    scale_y_continuous("Daily admissions",
                       breaks = scales::breaks_extended(),
                       position = 'right') +
    
    
    coord_cartesian(xlim = c(state_forecast_start - ddays(4),
                             forecast_dates$forecast_horizon)) +
    
    ggtitle(state_modelled, "Hospital admissions") +
    
    plots_common +
    
    theme(
      axis.title.x = element_blank()
    )
  
  
  over_capacity_trajectories <- sim_results$trajectories_ungrouped %>% 
    filter(compartment == "ward",
           date >= state_forecast_start) %>%
    group_by(sample) %>%
    
    filter(any(transitions > capacity_limit))
  
  
  n_trajs_total <- length(unique(sim_results$trajectories_ungrouped$sample))
  
  
  forecast_days <- seq(state_forecast_start,
                       forecast_dates$forecast_horizon - ddays(1),
                       by = "days")
  
  over_capacity_trajectories_summary <- over_capacity_trajectories %>%
    group_by(sample) %>%
    
    summarise(date_over = min(date[transitions > capacity_limit]))
  
  if(nrow(over_capacity_trajectories_summary) > 0) {
    plot_ecdf <- tibble(date = forecast_days,
                        y = ecdf(over_capacity_trajectories_summary$date_over)(date)) %>%
      mutate(y_adj = y * nrow(over_capacity_trajectories_summary) / n_trajs_total)
    
  }
  else{
    plot_ecdf <- tibble(date = forecast_days,
                        y_adj = 0)
  }
  
  p2 <- ggplot() +
    geom_hline(yintercept = 0,
               linetype = 'dotted') +
    
    geom_line(aes(x = date, y = y_adj * 100),
              plot_ecdf) +
    
    geom_vline(xintercept = state_forecast_start,
               lty = 2, colour = "grey60") +
    
    scale_y_continuous("Percent",
                       breaks = scales::breaks_extended(),
                       position = 'right') +
    
    coord_cartesian(xlim = c(state_forecast_start - ddays(4),
                             forecast_dates$forecast_horizon),
                    
                    ylim = c(0, 100)) +
    
    scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)) +
    
    ggtitle(NULL, "Percentage of simulated trajectories reaching admissions threshold") +
    
    plots_common
  
  
  p_joint <- cowplot::plot_grid(p1, p2, ncol = 1,
                     align = 'v',
                     axis = 'lr')
  
  
  list(
    plot = p_joint,
    table = plot_ecdf %>% select(date, y_adj)
  )
}

