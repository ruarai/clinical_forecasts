
linelist_data_counts <- read_csv(paste0(simulation_options$dirs$input,"/20211125_Hospital_Occupancy.csv")) %>%
  mutate(ward = Beds.Occupied,
         ICU = ICU.Beds.Occupied,
         date = Date) %>%
  select(date, ward, ICU) %>%
  pivot_longer(cols = c(ward, ICU), names_to = "group", values_to = "count")

p_ward <- ggplot(sim_results$tbl_count_grouped_quants %>% filter(group == "ward")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_point(aes(x = date, y = count),
             linelist_data_counts  %>% filter(group == "ward"),
             fill = 'black', size = 2, shape = 21, alpha = 0.7) +
  
  scale_fill_manual(values = c("99" = "#e7cff2",
                               "95" = "#d3a8e7",
                               "90" = "#c389de",
                               "75" = "#b770d7",
                               "50" = "#ad5cd2")) +
  # 
  # coord_cartesian(xlim = c(simulation_options$dates$last_onset_50 - ddays(60),
  #                          simulation_options$dates$forecast_horizon)) +
  
  coord_cartesian(xlim = c(simulation_options$dates$simulation_start,
                           simulation_options$dates$forecast_horizon)) +
  
  scale_y_continuous("Number Occupied Beds", position = "right") +
  scale_x_date("Date", date_labels = "%e/%m", date_breaks = "2 weeks", expand=c(0,0)) +
  
  geom_vline(xintercept = simulation_options$dates$last_onset_50,
             lty = 2, colour = "grey60") +
  
  ggtitle(simulation_options$state_modelled, "Ward Occupancy")+
  
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=0),
        text=element_text(size=18),
        axis.text=element_text(size=14),
        strip.background =element_rect(fill="white"))



p_ICU <- ggplot(sim_results$tbl_count_grouped_quants %>% filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_point(aes(x = date, y = count),
             linelist_data_counts  %>% filter(group == "ICU"),
             fill = 'black', size = 2, shape = 21, alpha = 0.7) +
  
  scale_fill_manual(values = c("99" = "#cfe5cc",
                               "95" = "#a8d0a3",
                               "90" = "#89bf82",
                               "75" = "#70b168",
                               "50" = "#5ca653")) +
  
  geom_vline(xintercept = simulation_options$dates$last_onset_50,
             lty = 2, colour = "grey60") +
  
  # coord_cartesian(xlim = c(simulation_options$dates$last_onset_50 - ddays(60),
  #                          simulation_options$dates$forecast_horizon)) +
  
  coord_cartesian(xlim = c(simulation_options$dates$simulation_start,
                           simulation_options$dates$forecast_horizon)) +
  
  scale_y_continuous("Number Occupied Beds", position = "right") +
  scale_x_date("Date", date_labels = "%e/%m", date_breaks = "2 weeks", expand=c(0,0))+
  
  ggtitle(" ", "ICU Occupancy") +
  
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=0),
        text=element_text(size=18),
        axis.text=element_text(size=14),
        strip.background =element_rect(fill="white"))
cowplot::plot_grid(p_ward, p_ICU, nrow = 1)

scale <- 7.5
ggsave(paste0(simulation_options$dirs$plots, "/report_figs.png"),
       bg = 'white',
       width = 2.5 * scale, height = 1 * scale,
       dpi = 200)