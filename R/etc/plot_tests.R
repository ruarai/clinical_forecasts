

clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)


days <- seq(min(clinical_linelist$dt_hosp_admission, na.rm = TRUE),
            max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE),
            by ='days') %>% as_date()


linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist %>%
           filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                  dt_hosp_admission <= date,
                  
                  is.na(dt_first_icu) | dt_first_icu >= date | dt_last_icu <= date) %>%
           nrow(),
         
         count_ICU = clinical_linelist %>%
           drop_na(dt_first_icu) %>%
           filter(dt_last_icu >= date | is.na(dt_last_icu),
                  dt_first_icu <= date) %>%
           nrow(),
         
         count_died = clinical_linelist %>%
           filter(patient_died) %>%
           filter(dt_hosp_discharge <= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")



fix_quant <- . %>% mutate(quant = factor(quant, levels = unique(quant)))

p_ward <- ggplot(sim_results$tbl_count_grouped_quants %>% filter(group == "ward")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_point(aes(x = date, y = count),
            linelist_data_counts %>% filter(group == "ward"),
            color = 'gray20', size = 0.7) +
  
  scale_fill_manual(values = c("99" = "#e7cff2",
                                "95" = "#d3a8e7",
                                "90" = "#c389de",
                                "75" = "#b770d7",
                                "50" = "#ad5cd2")) +
  
  coord_cartesian(xlim = c(simulation_options$dates$simulation_start,
                           simulation_options$dates$last_onset_50)) +
  
  ylab("Number Occupied Beds") +
  xlab("Date") +
  
  scale_x_date(labels = scales::label_date_short(),
               breaks = scales::date_breaks()) +
  
  scale_y_continuous(position = 'right') +
  
  ggtitle("Ward Occupancy") +
  
  cowplot::theme_cowplot() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 11,
                                  face = 'plain'),
        axis.title = element_text(size = 11,
                                  face = 'plain'))



p_ICU <- ggplot(sim_results$tbl_count_grouped_quants %>% filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_point(aes(x = date, y = count),
            linelist_data_counts  %>% filter(group == "ICU"),
            color = 'gray20', size = 0.7) +
  
  scale_fill_manual(values = c("99" = "#cfe5cc",
                               "95" = "#a8d0a3",
                               "90" = "#89bf82",
                               "75" = "#70b168",
                               "50" = "#5ca653")) +
  
  coord_cartesian(xlim = c(simulation_options$dates$simulation_start,
                           simulation_options$dates$last_onset_50)) +
  
  ylab("Number Occupied Beds") +
  xlab("Date") +
  
  scale_x_date(labels = scales::label_date_short(),
               breaks = scales::date_breaks()) +
  
  scale_y_continuous(position = 'right') +
  
  ggtitle("ICU Occupancy") +
  
  cowplot::theme_cowplot() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 11,
                                  face = 'plain'),
        axis.title = element_text(size = 11,
                                  face = 'plain'))

cowplot::plot_grid(p_ward, p_ICU, nrow = 1)

ggsave(paste0(simulation_options$dirs$plots, "/report_figs.png"),
       bg = 'white',
       width = 10, height = 5,
       dpi = 400)
