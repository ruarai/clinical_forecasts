

plot_capacity_plots <- function(sim_results, simulation_options) {
  
  capacity_plot <- function(capacity_group, capacity_limit, sim_results, p_title, ribbon_col,
                            do_label) {
    
    over_capacity_trajectories <- sim_results$tbl_count_grouped %>% 
      filter(group == capacity_group,
             date >= simulation_options$dates$last_onset_50) %>%
      group_by(ix) %>%
      
      filter(any(count > capacity_limit))
    
    n_trajs_total <- length(unique(sim_results$tbl_count_grouped$ix))
    
    over_capacity_trajectories_summary <- over_capacity_trajectories %>%
      group_by(ix) %>%
      
      summarise(date_over = min(date[count > capacity_limit]))
    
    
    forecast_days <- seq(simulation_options$dates$last_onset_50,
                         simulation_options$dates$last_onset_50 + 28,
                         by = "days")
    
    
    forecast_weeks <- seq(simulation_options$dates$last_onset_50,
                          simulation_options$dates$last_onset_50 + 28,
                          by = "weeks")
    
    plot_ecdf <- tibble(date = forecast_days,
                        y = ecdf(over_capacity_trajectories_summary$date_over)(date)) %>%
      mutate(y_adj = y * nrow(over_capacity_trajectories_summary) / n_trajs_total)
    
    p1 <- ggplot() +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                  alpha = 0.2, fill = ribbon_col,
                  sim_results$tbl_count_grouped_quants %>% filter(group == capacity_group)) +
      
      geom_hline(yintercept = capacity_limit,
                 linetype = 'longdash', size = 0.6) +
      
      geom_vline(xintercept = simulation_options$dates$last_onset_50,
                 lty = 2, colour = "grey60") +
      
      coord_cartesian(xlim = c(simulation_options$dates$last_onset_50 - ddays(4),
                               simulation_options$dates$last_onset_50 + ddays(28))) +
      
      scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)) +
      
      scale_y_continuous("Number Occupied Beds", position = "right",
                         breaks = scales::breaks_extended()) +
      
      ggtitle(p_title, if_else(do_label,
                               "Forecasted trajectories",
                               ""))+
      
      cowplot::theme_cowplot() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle=0),
            text=element_text(size=18),
            axis.text=element_text(size=14),
            strip.background =element_rect(fill="white")) +
      theme(axis.title.x = element_blank())
    
    
    p2 <- ggplot() +
      geom_hline(yintercept = 0,
                 linetype = 'dotted') +
      
      geom_line(aes(x = date, y = y_adj * 100),
                plot_ecdf) +
      
      geom_vline(xintercept = simulation_options$dates$last_onset_50,
                 lty = 2, colour = "grey60") +
      
      scale_y_continuous("Percent",
                         expand = expansion(mult = c(0.1, 1.1)),
                         breaks = scales::breaks_extended(),
                         position = 'right') +
      
      coord_cartesian(xlim = c(simulation_options$dates$last_onset_50 - ddays(4),
                               simulation_options$dates$last_onset_50 + ddays(28)),
                      
                      ylim = c(0, NA)) +
      
      scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)) +
      
      ggtitle(" ", if_else(do_label,
                           "Percentage of simulated trajectories reaching capacity threshold",
                           ""))+
      
      cowplot::theme_cowplot() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle=0),
            text=element_text(size=18),
            axis.text=element_text(size=14),
            strip.background =element_rect(fill="white")) +
      theme(plot.title = element_blank())
    
    
    
    p_joint <- cowplot::plot_grid(p1, p2, ncol = 1,
                                  align = 'v',
                                  axis = 'lr')
    
    list(
      plot = p_joint,
      table = plot_ecdf %>% select(date, y_adj)
    )
  }
  
  
  results_ward <- capacity_plot("ward", simulation_options$capacities$ward_occupancy,
                                sim_results, "Ward Occupancy", "darkorchid", TRUE)
  results_ICU <- capacity_plot("ICU", simulation_options$capacities$ICU_occupancy, 
                               sim_results, "ICU Occupancy", "green4", TRUE)
  
  cowplot::plot_grid(results_ward$plot, NULL, results_ICU$plot,
                     ncol = 3, rel_widths = c(0.5, 0.01, 0.5))
  
  
  scale <- 7.5
  ggsave(paste0(simulation_options$dirs$plots, "/report_figs_capacity.png"),
         bg = 'white',
         width = 2.3 * scale, height = 1 * scale,
         dpi = 200)
  
}
