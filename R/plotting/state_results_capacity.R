


plot_state_results_capacity <- function(
  sim_results,
  
  state_modelled,
  forecast_dates,
  forecast_name,
  plot_dir
) {
  
  
  capacity_plot <- function(capacity_group, capacity_limit, sim_results, p_title, ribbon_col,
                            do_label) {
    
    over_capacity_trajectories <- sim_results$trajectories %>% 
      filter(group == capacity_group,
             date >= forecast_dates$forecast_start) %>%
      group_by(sample) %>%
      
      filter(any(count > capacity_limit))
    
    n_trajs_total <- length(unique(sim_results$trajectories$sample))
    
    over_capacity_trajectories_summary <- over_capacity_trajectories %>%
      group_by(sample) %>%
      
      summarise(date_over = min(date[count > capacity_limit]))
    
    
    forecast_days <- seq(forecast_dates$forecast_start,
                         forecast_dates$forecast_horizon - ddays(1),
                         by = "days")
    
    
    forecast_weeks <- seq(forecast_dates$forecast_start,
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
    p1 <- ggplot() +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                  alpha = 0.2, fill = ribbon_col,
                  sim_results$quants_count %>% filter(group == capacity_group)) +
      
      geom_hline(yintercept = capacity_limit,
                 linetype = 'longdash', size = 0.6) +
      
      geom_vline(xintercept = forecast_dates$forecast_start,
                 lty = 2, colour = "grey60") +
      
      coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(4),
                               forecast_dates$forecast_horizon)) +
      
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
      
      geom_vline(xintercept = forecast_dates$forecast_start,
                 lty = 2, colour = "grey60") +
      
      scale_y_continuous("Percent",
                         expand = expansion(mult = c(0.1, 1.1)),
                         breaks = scales::breaks_extended(),
                         position = 'right') +
      
      geom_blank(aes(y = 5)) +
      
      coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(4),
                               forecast_dates$forecast_horizon),
                      
                      ylim = c(0, NA)) +
      
      scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0.01)) +
      
      ggtitle(" ", if_else(do_label,
                           "Percentage of simulated trajectories reaching capacity threshold",
                           ""))+
      
      labs(caption = state_modelled) +
      
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
  
  capacity_limits <- list(
    "ACT" = list("ward" = 488,  "ICU" = 42 ),
    "NSW" = list("ward" = 8832, "ICU" = 782),
    "NT"  = list("ward" = 276,  "ICU" = 24 ),
    "QLD" = list("ward" = 5099, "ICU" = 329),
    "SA"  = list("ward" = 1915, "ICU" = 129),
    "TAS" = list("ward" = 557,  "ICU" = 41 ),
    "VIC" = list("ward" = 6158, "ICU" = 380),
    "WA"  = list("ward" = 2471, "ICU" = 121)
  )
  
  
  results_ward <- capacity_plot("ward", capacity_limits[[state_modelled]]$ward,
                                sim_results, "Ward Occupancy", "darkorchid", TRUE)
  
  results_ICU <- capacity_plot("ICU", capacity_limits[[state_modelled]]$ICU, 
                               sim_results, "ICU Occupancy", "green4", TRUE)
  
  cowplot::plot_grid(results_ward$plot, NULL, results_ICU$plot,
                     ncol = 3, rel_widths = c(0.5, 0.01, 0.5))
  
  
  scale <- 7.5
  ggsave(paste0(plot_dir, state_modelled, "_capacity.png"),
         bg = 'white',
         width = 2.3 * scale, height = 1 * scale,
         dpi = 200)
  
  
  results_tbl <- bind_rows(
    results_ward$table %>% mutate(group = "ward"),
    results_ICU$table %>% mutate(group = "ICU")
  ) %>%
    mutate(state = state_modelled)
  
  results_tbl
}

