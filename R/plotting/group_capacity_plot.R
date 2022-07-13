
group_capacity_plot <- function(
  capacity_group, capacity_limit,
  sim_results, p_title, ribbon_col,
  forecast_dates, state_forecast_start
) {
  
  over_capacity_trajectories <- sim_results$trajectories %>% 
    filter(group == capacity_group,
           date >= state_forecast_start + ddays(7)) %>%
    group_by(sample) %>%
    
    filter(any(count > capacity_limit))
  
  n_trajs_total <- length(unique(sim_results$trajectories$sample))
  
  over_capacity_trajectories_summary <- over_capacity_trajectories %>%
    group_by(sample) %>%
    
    summarise(date_over = min(date[count > capacity_limit]))
  
  
  forecast_days <- seq(state_forecast_start + ddays(7),
                       forecast_dates$forecast_horizon - ddays(1),
                       by = "days")
  
  
  forecast_weeks <- seq(state_forecast_start + ddays(7),
                        forecast_dates$forecast_horizon,
                        by = "weeks")
  
  if(nrow(over_capacity_trajectories_summary) > 0) {
    plot_ecdf <- tibble(date = forecast_days,
                        y = ecdf(over_capacity_trajectories_summary$date_over)(date)) %>%
      mutate(y_adj = y * nrow(over_capacity_trajectories_summary) / n_trajs_total)
    
  }
  else{
    plot_ecdf <- tibble(date = forecast_days,
                        y_adj = 0)
  }
  
  plots_common <- list(
    geom_vline(xintercept = state_forecast_start + ddays(7),
               lty = 2, colour = "grey20"),
    scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)),
    cowplot::theme_cowplot(),
    theme(legend.position = "none",
          axis.text.x = element_text(angle=0),
          text=element_text(size=18),
          axis.text=element_text(size=14),
          strip.background =element_rect(fill="white"))
  )
  
  source("R/_situational_awareness_functions.R")
  
  plot_cols <- get_plot_colors(8, FALSE)
  
  p1 <- ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                sim_results$quants_count %>% filter(group == capacity_group)) +
    
    geom_hline(yintercept = capacity_limit,
               linetype = 'longdash', size = 0.6) +
    
    scale_fill_manual(values = plot_cols[[capacity_group]]) +
    
    coord_cartesian(xlim = c(state_forecast_start + ddays(4),
                             forecast_dates$forecast_horizon)) +
    
    scale_y_continuous(NULL, position = "right",
                       breaks = scales::breaks_extended()) +
    
    ggtitle(" ", p_title) +
    plots_common +
    theme(axis.title.x = element_blank())
  
  
  p2 <- ggplot() +
    geom_hline(yintercept = 0,
               linetype = 'dotted') +
    
    geom_line(aes(x = date, y = y_adj * 100),
              plot_ecdf) +
    
    scale_y_continuous(NULL,
                       breaks = scales::breaks_extended(),
                       position = 'right') +
    
    plots_common +
    
    coord_cartesian(xlim = c(state_forecast_start + ddays(4),
                             forecast_dates$forecast_horizon),
                    
                    ylim = c(0, 100)) +
    
    ggtitle(NULL, str_c("Ward", " \u2013 Percentage of simulations reaching prior peak occupancy"))
  
  
  
  p_joint <- cowplot::plot_grid(p1, p2, ncol = 1,
                                align = 'v',
                                axis = 'lr')
  
  
  p_joint
  
  list(
    plot = p_joint,
    table = plot_ecdf %>% select(date, y_adj)
  )
}
