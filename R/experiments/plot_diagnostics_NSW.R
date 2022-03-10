
linelist_data_NSW <- tar_read(linelist_state_NSW)

linelist_raw_NSW <- readxl::read_excel(tar_read(NSW_linelist_path), sheet = 2) %>%
  mutate(date_onset = as_date(admit_date - ddays(covid_to_adm))) %>%
  
  group_by(person_id) %>%
  slice(n())

p_common <- list(
  theme_minimal(),
  scale_x_date(date_breaks = "months", labels = scales::label_date_short()),
  scale_y_continuous(breaks = scales::extended_breaks(), labels = scales::label_comma()),
  coord_cartesian(xlim = c(forecast_dates$simulation_start, forecast_dates$forecast_start))
)


ggplot() +
  
  geom_bar(aes(x = date_onset),# fill = 'blue', alpha = 0.25,
           tar_read(nindss_state_NSW) %>% filter(ever_in_hospital)) +
  geom_bar(aes(x = date_onset),
           fill = 'blue', alpha = 0.5,
           linelist_data_NSW %>%
             filter(date_onset >= forecast_dates$simulation_start)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'black', alpha = 0.2,
              
              data = results_ungrouped_transitions_quants %>%
                filter(compartment == "symptomatic_clinical")) +
  
  p_common

ggplot() +
  # geom_bar(aes(x = date_onset),
  #          fill = 'black', alpha = 0.5,
  #          linelist_data_NSW %>%
  #            filter(date_onset >= forecast_dates$simulation_start, ever_in_ICU)) +
  
  geom_bar(aes(x = date_onset), 
           tar_read(nindss_state_NSW) %>% filter(ever_in_hospital, ever_in_ICU)) +
  
  geom_ribbon(aes(x = date - 7, ymin = lower, ymax = upper, group = quant),
              fill = 'purple', alpha = 0.2,
              
              data = results_transitions_quants %>%
                filter(group == "ICU")) +
  
  p_common


ggplot(
  linelist_data_NSW %>%
    mutate(date_admission = as_date(dt_hosp_admission   )) %>%
    filter(date_admission > forecast_dates$simulation_start)
) +
  geom_bar(aes(x = date_admission)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'green4', alpha = 0.2,
              
              data = results_ungrouped_transitions_quants %>%
                filter(compartment == "ward")) +
  
  p_common +
  
  ggtitle(NULL, "Ward admissions")




ggplot(
  linelist_data_NSW %>%
    mutate(date_ICU_admission = as_date(dt_first_icu)) %>%
    filter(date_ICU_admission > forecast_dates$simulation_start)
) +
  geom_bar(aes(x = date_ICU_admission)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'purple', alpha = 0.2,
              
              data = results_transitions_quants %>%
                filter(group == "ICU")) +
  
  p_common +
  
  ggtitle(NULL, "ICU admissions")
