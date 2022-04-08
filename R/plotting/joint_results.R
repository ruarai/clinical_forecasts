
plot_joint_results <- function(
  all_state_quants,
  known_occupancy_ts,
  forecast_dates,
  date_reporting_line,
  
  forecast_name,
  plot_dir
) {
  
  source("R/capacity_table.R")
  
  capacity_limits_tbl <- capacity_limits %>%
    map_dfr(function(x) tibble_row(capacity_ward = x$ward, capacity_ICU = x$ICU),
            .id = "state") %>%
    pivot_longer(cols = c(capacity_ward, capacity_ICU),
                 names_prefix = "capacity_",
                 names_to = "group", values_to = "capacity")
  
  
  source("R/plotting/get_joint_plot_limits.R")
  
  plot_lims <- get_joint_plot_limits(
    all_state_quants,
    known_occupancy_ts,
    capacity_limits_tbl,
    
    forecast_dates
  )
  
  known_occupancy_ts <- known_occupancy_ts %>%
    
    filter(date >= forecast_dates$forecast_start - ddays(10)) %>%
    
    mutate(do_match = date > forecast_dates$forecast_start & date <= forecast_dates$forecast_start + ddays(7))
  
  
  
  all_state_quants <- all_state_quants %>%
    
    filter(date > forecast_dates$forecast_start - ddays(10)) %>%
    
    left_join(plot_lims) %>%
    
    mutate(upper = if_else(is.na(y_lim), upper, pmin(upper, y_lim)),
           lower = if_else(is.na(y_lim), lower, pmin(lower, y_lim)))
  
  
  forecast_weeks <- seq(forecast_dates$forecast_start - 7,
                        forecast_dates$forecast_start + 28,
                        by = "weeks")
  
  plots_common <- list(
    coord_cartesian(xlim = c(forecast_dates$forecast_start - 7,
                             forecast_dates$forecast_horizon)),
    scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0)),
    
    scale_shape_manual(
      values = c("FALSE" = 1, "TRUE" = 16)
    ),
    cowplot::theme_cowplot(),
    theme(legend.position = "none",
          text=element_text(size=12),
          axis.text=element_text(size=8),
          strip.background =element_rect(fill="white"),
          plot.margin = margin(l = 1, unit = "cm"),
          strip.text = element_text(hjust = 0),
          axis.text.y = element_text(size = 8, margin = margin(l = 5, unit = "cm"))),
    geom_blank(aes(y = 10)),
    facet_wrap(~state, ncol = 1,
               scales = "free_y")
  )
  
  capacity_limits_tbl <- capacity_limits_tbl %>%

    left_join(plot_lims) %>%

    filter(capacity < y_lim)

  
  plot_states <- function(states) {
    adj_ward <- . %>% filter(group == "ward", state %in% states) %>%
      mutate(state = str_c("Ward - ", state))
    
    p_ward <- ggplot(all_state_quants %>% adj_ward()) +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey60") +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                  fill = 'darkorchid', alpha = 0.2) +
      
      geom_vline(xintercept = forecast_dates$forecast_start + 7,
                 colour = "grey60", linetype = 'longdash') +
      
      geom_point(aes(x = date, y = count, pch = do_match),
                 known_occupancy_ts %>% adj_ward(),
                 size = 1, alpha = 0.8) +
      
      scale_y_continuous("Number Occupied Beds", position = "right",
                         expand = c(0,0),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(accuracy = 1)) +
      
      plots_common +
      
      geom_hline(aes(yintercept = capacity),
                 capacity_limits_tbl %>% adj_ward(),
                 linetype = 'dashed', size = 0.3) +
      
      geom_blank(aes(yintercept = y_lim),
                 plot_lims %>% adj_ward()) +
      
      theme(axis.title.y = element_blank())
    
    
    adj_ICU <- . %>% filter(group == "ICU", state %in% states) %>%
      mutate(state = str_c("ICU - ", state))
    
    p_ICU <- ggplot(all_state_quants %>% adj_ICU()) +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey80") +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                  fill = 'green4', alpha = 0.2) +
      
      geom_vline(xintercept = forecast_dates$forecast_start + 7,
                 colour = "grey60", linetype = 'longdash') +
      
      geom_point(aes(x = date, y = count, pch = do_match),
                 known_occupancy_ts %>% adj_ICU(),
                 size = 1, alpha = 0.8)  +
      
      scale_y_continuous("Number Occupied Beds\n ", position = "right",
                         expand = c(0,0),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(accuracy = 1)) +
      
      plots_common +
      
      geom_hline(aes(yintercept = capacity),
                 capacity_limits_tbl %>% adj_ICU(),
                 linetype = 'dashed', size = 0.3) +
      
      geom_blank(aes(yintercept = y_lim),
                 plot_lims %>% adj_ICU())
    
    cowplot::plot_grid(p_ward, p_ICU, ncol = 2)
  }
  
  
  
  states_A <- c("ACT", "NSW", "NT",  "QLD")
  states_B <- c("SA",  "TAS", "VIC", "WA")
  
  plot_states(states_A)
  
  scale <- 7.5
  
  ggsave(paste0(plot_dir, "_national_forecast_join_A.png"),
         bg = 'white',
         width = 1 * scale, height = 1 * scale,
         dpi = 200)
  plot_states(states_B)
  
  ggsave(paste0(plot_dir, "_national_forecast_join_B.png"),
         bg = 'white',
         width = 1 * scale, height = 1 * scale,
         dpi = 200)
  
  
}