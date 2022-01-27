
plot_joint_results <- function(
  all_state_quants,
  forecast_dates,
  date_reporting_line,
  
  forecast_name,
  plot_dir
) {
  dir.create(plot_dir, showWarnings = FALSE)
  
  forecast_weeks <- seq(forecast_dates$forecast_start - 7,
                        forecast_dates$forecast_start + 28,
                        by = "weeks")
  
  plots_common <- list(
    coord_cartesian(xlim = c(forecast_dates$forecast_start - 7,
                             forecast_dates$forecast_horizon)),
    scale_x_date("Date", date_labels = "%e/%m", breaks = forecast_weeks, expand=c(0,0)),
    geom_vline(xintercept = forecast_dates$forecast_start,
               lty = 5, colour = "grey60"),
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
  
  source("R/capacity_table.R")
  
  capacity_limits_tbl <- capacity_limits %>%
    map_dfr(function(x) tibble_row(capacity_ward = x$ward, capacity_ICU = x$ICU),
            .id = "state") %>%
    pivot_longer(cols = c(capacity_ward, capacity_ICU),
                 names_prefix = "capacity_",
                 names_to = "group", values_to = "capacity")
  
  
  plot_states <- function(states) {
    p_ward <- ggplot(all_state_quants %>% filter(group == "ward", state %in% states) %>%
                       mutate(state = str_c("Ward - ", state))) +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey80") +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
      
      scale_fill_manual(values = c("99" = "#e7cff2",
                                   "95" = "#d3a8e7",
                                   "90" = "#c389de",
                                   "75" = "#b770d7",
                                   "50" = "#ad5cd2")) +
      
      scale_y_continuous("Number Occupied Beds", position = "right",
                         expand = c(0,0),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(accuracy = 1)) +
      
      plots_common +
      
      geom_hline(aes(yintercept = capacity),
                 capacity_limits %>% filter(group == "ward", state %in% states) %>%
                   mutate(state = str_c("Ward - ", state)),
                 linetype = 'dashed', size = 0.3) +
      
      theme(axis.title.y = element_blank())
    
    
    p_ICU <- ggplot(all_state_quants %>% filter(group == "ICU", state %in% states) %>%
                      mutate(state = str_c("ICU - ", state))) +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey80") +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant))  +
      
      scale_fill_manual(values = c("99" = "#cfe5cc",
                                   "95" = "#a8d0a3",
                                   "90" = "#89bf82",
                                   "75" = "#70b168",
                                   "50" = "#5ca653")) +
      
      scale_y_continuous("Number Occupied Beds\n ", position = "right",
                         expand = c(0,0),
                         breaks = scales::breaks_extended(),
                         labels = scales::label_number(accuracy = 1)) +
      plots_common +
      
      geom_hline(aes(yintercept = capacity),
                 capacity_limits %>% filter(group == "ICU", state %in% states) %>%
                   mutate(state = str_c("ICU - ", state)),
                 linetype = 'dashed', size = 0.3)
    
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