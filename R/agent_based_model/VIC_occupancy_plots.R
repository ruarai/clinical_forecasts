ICU_twitter <- read_csv("/usr/local/forecasting/linelist_data/VIC/ICU_twitter.csv") %>%
  mutate(date = dmy(Date))


c19data <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == simulation_options$state_modelled,
         date >= min(clinical_linelist$dt_hosp_admission)) %>% select(-c(state, state_abbrev)) %>%
  
  mutate(ward_cum = hosp_cum - icu_cum) %>%
  select(date, ward = ward_cum, ICU = icu_cum) %>%
  
  pivot_longer(cols = -c(date),
               values_to = "count", names_to = "group")

ggplot() +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = 'ribbon_active'),
              alpha = 0.2,
              sim_results$tbl_count_active_grouped_quants %>% filter(group == "ICU")) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = 'ribbon_total'),
              alpha = 0.2,
              sim_results$tbl_count_grouped_quants %>% filter(group == "ICU")) +
  
  geom_line(aes(x = date, y = count, linetype = 'ICU occupancy - active cases'),
            c19data %>% filter(group == "ICU")) +
  
  geom_line(aes(x = date, y = TotalICU, linetype = 'ICU occupancy - total'),
            ICU_twitter) +
  
  xlim(c(simulation_options$dates$simulation_start,
         simulation_options$dates$last_onset_50)) +
  
  scale_fill_manual("",
                    values = c("ribbon_active" = "blue4",
                               "ribbon_total" = "green4"),
                    labels = c("ribbon_active" = "Active",
                               "ribbon_total" = "Total")) +
  
  xlab("Date") + ylab("Count") +
  
  theme_minimal() +
  theme(legend.position = 'bottom')


ggplot() +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = 'ribbon_active'),
              alpha = 0.2,
              sim_results$tbl_count_active_grouped_quants %>% filter(group == "ward")) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = 'ribbon_total'),
              alpha = 0.2,
              sim_results$tbl_count_grouped_quants %>% filter(group == "ward")) +
  
  geom_line(aes(x = date, y = count, color = 'ward occupancy - active cases'),
            c19data %>% filter(group == "ward")) +
  
  xlim(c(simulation_options$dates$simulation_start,
         simulation_options$dates$last_onset_50)) +
  
  scale_color_manual("", values = c("black")) +
  
  scale_fill_manual("",
                    values = c("ribbon_active" = "blue4",
                               "ribbon_total" = "darkorchid"),
                    labels = c("ribbon_active" = "Active",
                               "ribbon_total" = "Total")) +
  
  xlab("Date") + ylab("Count") +
  
  theme_minimal() +
  theme(legend.position = 'bottom')
