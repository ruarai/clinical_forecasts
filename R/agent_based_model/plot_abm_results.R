

plot_abm_results <- function(results_all,
                             simulation_options,
                             forecast_dates) {
  
  tbl_count <- results_all$tbl_count
  tbl_count_grouped <- results_all$tbl_count_grouped
  tbl_transitions <- results_all$tbl_transitions
  tbl_count_grouped_quants <- results_all$tbl_count_grouped_quants
  
  forecast_date_lines <- list(
    geom_vline(xintercept = forecast_dates$date_last_infection_50 - 5, linetype = 'dashed'),
    geom_vline(xintercept = forecast_dates$date_last_onset_50, linetype = 'dashed'),
    geom_vline(xintercept = forecast_dates$date_forecast_horizon, linetype = 'dotted')
  )
  
  p1 <- ggplot(tbl_transitions %>%
           filter(str_detect(ix, "-1"))) +
    geom_line(aes(x = date, y = n, group = ix),
              size = 0.1) +
    
    facet_wrap(~new_comp, scales = "free_y", ncol = 1) +
    forecast_date_lines +
    
    theme_minimal() +
    ggtitle("Transitions into compartment") + xlab("")
  
  
  p2 <- ggplot(tbl_count %>%
           filter(str_detect(ix, "-1"),
                  compartment %in% unique(tbl_transitions$new_comp))) +
    geom_line(aes(x = date, y = count, group = ix),
              size = 0.1) +
    
    facet_wrap(~compartment, scales = "free_y", ncol = 1) +
    forecast_date_lines +
    
    theme_minimal() +
    ggtitle("Count in compartment") + xlab("")
  
  plot_p12 <- cowplot::plot_grid(p1, p2, ncol = 2)
  ggsave(paste0(simulation_options$dirs$plots, "/trace_plots.png"),
         height = 12, width = 8, bg = 'white')
  
  
  ggplot(tbl_count_grouped %>%
           filter(str_detect(ix, "-1"),
                  !is.na(group))) +
    geom_line(aes(x = date, y = count, group = ix),
              size = 0.1) +
    
    facet_wrap(~group, scales = "free_y", ncol = 3) +
    forecast_date_lines +
    
    theme_minimal()
  ggsave(paste0(simulation_options$dirs$plots, "/trace_count_grouped.png"),
         height = 6, width = 8, bg = 'white')
  
  
  ggplot(tbl_transitions %>%
           filter(new_comp == "symptomatic_clinical",
                  str_detect(ix, "-1"))) +
    geom_line(aes(x = date, y = n, group = ix),
               size = 0.5) +
    forecast_date_lines +
    
    coord_cartesian(xlim = c(forecast_dates$date_last_onset_50 - 30,
                             forecast_dates$date_last_onset_50 + 28)) +
    
    theme_minimal()
  ggsave(paste0(simulation_options$dirs$plots, "/trace_onset.png"),
         height = 6, width = 8, bg = 'white')
  
  
  plot_group_counts(sim_results, simulation_options,
                    forecast_date_lines)
  
  plot_group_transitions(sim_results, simulation_options,
                         forecast_date_lines)
  
  ggsave(paste0(simulation_options$dirs$plots, "/quants_transitions.png"),
         height = 6, width = 8, bg = 'white')
  
}

plot_group_counts <- function(sim_results, simulation_options,
                              forecast_date_lines) {
  
  
  clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)
  
  
  days <- seq(min(clinical_linelist$dt_hosp_admission, na.rm = TRUE),
              max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE),
              by ='days') %>% as_date()
  
  
  linelist_data_counts <- tibble(date = days) %>%
    rowwise() %>%
    
    mutate(count_ward = clinical_linelist %>%
             filter(dt_hosp_discharge >= date, dt_hosp_admission <= date) %>%
             nrow(),
           
           count_ICU = clinical_linelist %>%
             drop_na(dt_first_icu) %>%
             filter(dt_last_icu >= date, dt_first_icu <= date) %>%
             nrow(),
           
           count_died = clinical_linelist %>%
             filter(patient_died) %>%
             filter(dt_hosp_discharge <= date) %>%
             nrow()) %>%
    
    pivot_longer(cols = starts_with("count_"),
                 names_prefix = "count_",
                 values_to = "count",
                 names_to = "group")
  
  
  c19data <- read_rds("data/covid19data.rds") %>%
    filter(state_abbrev == simulation_options$state_modelled,
           date >= min(clinical_linelist$dt_hosp_admission)) %>% select(-c(state, state_abbrev)) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(date),
                 values_to = "count", names_to = "group")
  
  ggplot(sim_results$tbl_count_grouped_quants) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
    
    geom_line(aes(x = date, y = count, linetype = 'public data'),
              c19data) +
    
    geom_line(aes(x = date, y = count, linetype = 'clinical linelist data'),
              linelist_data_counts) +
    
    facet_wrap(~group, scales = "free_y") +
    forecast_date_lines +
    
    scale_fill_brewer(type = 'seq',
                      palette = 5) +
    
    theme_minimal() +
    theme(legend.position = 'bottom')
  ggsave(paste0(simulation_options$dirs$plots, "/quants_grouped_backcast.png"),
         height = 6, width = 9, bg = 'white')
  
  
  
  ggplot(sim_results$tbl_count_grouped_quants) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
    
    geom_line(aes(x = date, y = count, linetype = 'public data'),
              c19data) +
    
    geom_line(aes(x = date, y = count, linetype = 'clinical linelist data'),
              linelist_data_counts) +
    
    facet_wrap(~group, scales = "free_y") +
    
    scale_fill_brewer(type = 'seq',
                      palette = 5) +
    forecast_date_lines +
    
    coord_cartesian(xlim = c(forecast_dates$date_last_onset_50 - 60,
                             forecast_dates$date_last_onset_50 + 28)) +
    
    theme_minimal() +
    theme(legend.position = 'bottom')
  ggsave(paste0(simulation_options$dirs$plots, "/quants_grouped_forecast.png"),
         height = 6, width = 9, bg = 'white')
  
  filter_grp <- . %>% filter(group %in% c("ICU", "ward", "died"))
  ggplot(sim_results$tbl_count_grouped_quants %>% filter_grp) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
    
    geom_line(aes(x = date, y = count, linetype = 'public data'),
              c19data) +
    
    geom_line(aes(x = date, y = count, linetype = 'clinical linelist data'),
              linelist_data_counts) +
    
    facet_wrap(~group, scales = "free_y") +
    
    scale_fill_brewer(type = 'seq',
                      palette = 5) +
    forecast_date_lines +
    
    coord_cartesian(xlim = c(forecast_dates$date_last_onset_50 - 60,
                             forecast_dates$date_last_onset_50 + 28)) +
    
    theme_minimal() +
    theme(legend.position = 'bottom')
  ggsave(paste0(simulation_options$dirs$plots, "/quants_key_groups.png"),
         height = 6, width = 9, bg = 'white')
  
}


plot_group_transitions <- function(sim_results, simulation_options,
                                   forecast_date_lines) {
  clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)
  
  ward_admission_by_day <- clinical_linelist %>%
    group_by(date = as_date(dt_hosp_admission, 'days')) %>%
    summarise(n_ward = n())
  
  ICU_admission_by_day <- clinical_linelist %>%
    drop_na(dt_first_icu) %>%
    group_by(date = as_date(dt_first_icu, 'days')) %>%
    summarise(n_ICU = n())
  
  discharge_by_day <- clinical_linelist %>%
    filter(dt_hosp_discharge != max(dt_hosp_discharge),
           !patient_died) %>%
    group_by(date = as_date(dt_hosp_discharge, 'days')) %>%
    summarise(n_discharged = n())
  
  death_by_day <- clinical_linelist %>%
    filter(dt_hosp_discharge != max(dt_hosp_discharge),
           patient_died) %>%
    group_by(date = as_date(dt_hosp_discharge, 'days')) %>%
    summarise(n_died = n())
  
  linelist_data <- ward_admission_by_day %>%
    left_join(ICU_admission_by_day) %>%
    left_join(discharge_by_day) %>%
    left_join(death_by_day) %>%
    pivot_longer(cols = starts_with("n_"),
                 names_to = "group",
                 names_prefix = "n_") %>%
    mutate(value = replace_na(value, 0))
  
  transition_data <- sim_results$tbl_transitions_grouped_quants %>%
    filter(group %in% c("ward", "ICU", "discharged", "died"))
  
  
  
  
  ggplot() +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant),
                data = transition_data) +
    
    geom_line(aes(x = date, y = value),
              linelist_data) +
    
    facet_wrap(~group, scales = "free_y") +
    
    forecast_date_lines +
    
    scale_fill_brewer(type = 'seq',
                      palette = 5) +
    
    theme_minimal() +
    
    ggtitle("Transitions into compartment groups")
}








