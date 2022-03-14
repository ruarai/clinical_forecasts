forecast_dates <- tar_read(forecast_dates)
all_state_quants <- tar_read(all_state_quants) %>%
  filter(date >= forecast_dates$forecast_start - ddays(60))

all_state_known_occupancy_ts <- tar_read(all_state_known_occupancy_ts) %>%
  filter(!(state == "NSW" & source == "direct_ll"))


forecast_name <- tar_read(forecast_name)

plot_dir <- tar_read(plot_dir)
dir.create(plot_dir, showWarnings = FALSE)

plots_common <- list(
  coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(60),
                           forecast_dates$forecast_horizon)),
  scale_x_date("Date", date_labels = "%e/%m", breaks = "weeks", expand=c(0,0)),
  geom_vline(xintercept = forecast_dates$forecast_start,
             lty = 2, colour = "grey60"),
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


plot_states <- function(states) {
  apply_ward <- . %>% filter(group == "ward", state %in% states) %>%
    mutate(state = str_c("Ward - ", state))
  
  apply_ICU <- .  %>% filter(group == "ICU", state %in% states) %>%
    mutate(state = str_c("ICU - ", state))
  
  p_ward <- ggplot(all_state_quants %>% apply_ward()) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'darkorchid', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count, linetype = source),
              all_state_known_occupancy_ts %>% apply_ward()) +
    
    scale_y_continuous("Number Occupied Beds", position = "right",
                       expand = c(0,0),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(accuracy = 1)) +
    
    plots_common +
    
    theme(axis.title.y = element_blank())
  
  
  p_ICU <- ggplot(all_state_quants %>% apply_ICU()) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'green4', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count, linetype = source),
              all_state_known_occupancy_ts %>% apply_ICU()) +
    
    scale_y_continuous("Number Occupied Beds\n ", position = "right",
                       expand = c(0,0),
                       breaks = scales::breaks_extended(),
                       labels = scales::label_number(accuracy = 1)) +
    plots_common
  
  cowplot::plot_grid(p_ward, p_ICU, ncol = 2)
}



states_A <- c("ACT", "NSW", "NT",  "QLD")
states_B <- c("SA",  "TAS", "VIC", "WA")

plot_states(states_A)

scale <- 7.5

ggsave(paste0(plot_dir, "_national_backcast_join_A.png"),
       bg = 'white',
       width = 1 * scale, height = 1 * scale,
       dpi = 200)
plot_states(states_B)

ggsave(paste0(plot_dir, "_national_backcast_join_B.png"),
       bg = 'white',
       width = 1 * scale, height = 1 * scale,
       dpi = 200)
