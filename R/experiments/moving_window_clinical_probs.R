

nindss_state <- tar_read(nindss_state_NSW)

forecast_dates <- tar_read(forecast_dates)

date_start <- ymd("2021-10-01")
date_end <- forecast_dates$NNDSS - ddays(2)

age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


fn_score_hosp <- function(x, A, days_since_onset, delay_shape, delay_scale) {
  prob_already_observed <- pgamma(days_since_onset,
                                  shape = delay_shape,
                                  scale = delay_scale)
  
  
  A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
}


fn_score_ICU <- function(x, A, days_since_onset, 
                         delay_hosp_shape, delay_hosp_scale,
                         delay_ICU_shape, delay_ICU_scale) {
  
  prob_already_observed <- sapply(1:length(days_since_onset), function(i) {
    F_icu_given_case(days_since_onset[i],
                     delay_hosp_shape[i], delay_hosp_scale[i],
                     delay_ICU_shape[i], delay_ICU_scale[i])
  })
  
  
  
  A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
}
clinical_parameters <- tar_read(clinical_parameters)

clinical_parameter_lookup <- clinical_parameters %>%
  select(-age_group) %>%
  as.matrix() %>%
  `rownames<-`(clinical_parameters$age_group)

model_results <- map_dfr(age_groups, function(i_age_class) {
  
  model_data <- nindss_state %>%
    filter(age_group == i_age_class,
           date_onset >= date_start,
           date_onset <= date_end)
  
  
  
  
  window_start <- date_start
  window_end <- window_start + ddays(3)
  
  cases_in_window <- function(start, end) {
    model_data %>% filter(date_onset >= start, date_onset <= end)
  }
  i_window <- 1
  
  window_data <- tibble()
  
  while(window_start < date_end + ddays(3)) {
    
    while(nrow(cases_in_window(window_start, window_end)) < 100 & window_end < date_end + ddays(3)) {
      window_end <- window_end + ddays(1)
    }
    
    window_data <- bind_rows(
      window_data,
      cases_in_window(window_start, window_end) %>%
        mutate(window = i_window,
               date_start = window_start,
               date_end = window_end)
    )
    
    window_start <- window_end + ddays(1)
    window_end <- window_start + ddays(3)
    
    
    i_window <- i_window + 1
    
  }
  
  
  window_data_summ <- window_data %>%
    group_by(window_s = window, date_start, date_end) %>%
    summarise(n_hosp = sum(ever_in_hospital, na.rm = TRUE),
              n_cases = n(), .groups = "drop") %>%
    
    rowwise() %>%
    mutate(nothosp_days_since_onset = window_data %>%
             filter(window_s == window) %>%
             filter(!ever_in_hospital) %>%
             mutate(days_since_onset = forecast_dates$NNDSS - date_onset) %>%
             pull(days_since_onset) %>% as.numeric() %>% list())
  
  
  
  delay_hosp_shape <- clinical_parameter_lookup[i_age_class,
                                                "shape_onset_to_ward"]
  
  delay_hosp_scale <- clinical_parameter_lookup[i_age_class,
                                                "scale_onset_to_ward"]
  
  
  
  window_data_summ_adj <- window_data_summ %>%
  rowwise() %>%
  mutate(
    pr_hosp_adj = tryCatch(pracma::fzero(
      function(x) {
        fn_score_hosp(x, 
                      n_hosp,
                      nothosp_days_since_onset,
                      delay_shape = delay_hosp_shape, 
                      delay_scale = delay_hosp_scale)
      },
      
      x = c(0+.Machine$double.eps,1-.Machine$double.eps),
    )$x,
    error = function(c) { return(-1) }),
    
    pr_hosp = n_hosp / n_cases
  ) %>%
  
  filter(pr_hosp_adj != -1) %>%
    
    mutate(age_group = i_age_class)
  
  window_data_summ_adj
})

model_plot <- model_results %>%
  rowwise() %>%
  mutate(date = mean.Date(c(date_start, date_end)))

ggplot(model_plot) +
  # geom_line(aes(x = date_end, y = pr_hosp_adj, color = age_group)) +
  # geom_point(aes(x = date_end, y = pr_hosp_adj, color = age_group),
  #            size = 0.5) +
  geom_line(aes(x = date, y = pr_hosp, color = age_group)) +
  geom_point(aes(x = date, y = pr_hosp, color = age_group),
             size = 0.5) +
  
  ggokabeito::scale_color_okabe_ito(name = "Age group") +
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_x_date(breaks = "months",
               labels = scales::label_date_short()) +
  
  xlab("Date") + ylab("Probability") +
  
  theme_minimal() +
  
  ggtitle("Observed probabilities of hospitalisation",
          paste0("Estimates produced over windows of at least 100 cases.\n\n",
                 "Without adjustment for right truncation applied."))
