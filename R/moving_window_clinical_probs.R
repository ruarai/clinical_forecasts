
make_clinical_prob_table <- function(
  nindss_state,
  forecast_dates,
  clinical_parameters,
  
  state_modelled,
  
  national_clinical_table,
  
  plot_dir
) {
  
  
  # Return the national-average clinical probabilities for states without good data
  if(!(state_modelled %in% c("VIC", "NSW", "national"))) {
    return(national_clinical_table)
  }
  
  if(state_modelled == "national") {
    # Producing national-average results across VIC and NSW (excluding all other states for data quality)
    nindss_state <- nindss_state %>%
      filter(state %in% c("VIC", "NSW"))
  }
  
  
  date_start <- forecast_dates$simulation_start
  date_end <- forecast_dates$NNDSS - ddays(2)
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  fn_score_hosp <- function(x, A, days_since_onset, delay_shape, delay_scale) {
    prob_already_observed <- pgamma(days_since_onset,
                                    shape = delay_shape,
                                    scale = delay_scale)
    
    
    A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
  }
  
  
  
  F_icu_given_case <- function(x, 
                               delay_hosp_shape, delay_hosp_scale,
                               delay_ICU_shape, delay_ICU_scale) {
    
    numer <- integrate(function(y){
      pgamma(x - y,
             shape = delay_ICU_shape,
             scale = delay_ICU_scale) *
        dgamma(y,
               shape = delay_hosp_shape,
               scale = delay_hosp_scale)
    },
    
    0 + .Machine$double.eps, x)$value
    
    denom <- pgamma(x,
                    shape = delay_hosp_shape,
                    scale = delay_hosp_scale)
    
    return(numer / denom)
  }
  
  fn_score_ICU <- function(x, A, days_since_onset, 
                           delay_hosp_shape, delay_hosp_scale,
                           delay_ICU_shape, delay_ICU_scale) {
    
    prob_already_observed <- sapply(1:length(days_since_onset), function(i) {
      F_icu_given_case(days_since_onset[i],
                       delay_hosp_shape, delay_hosp_scale,
                       delay_ICU_shape, delay_ICU_scale)
    })
    
    
    
    A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
  }
  
  
  
  clinical_parameter_lookup <- clinical_parameters %>%
    
    
    mutate(scale_onset_to_ward = scale_onset_to_ward * 0.7,
           shape_onset_to_ward = shape_onset_to_ward * 0.7) %>%
    
    select(-age_group) %>%
    as.matrix() %>%
    `rownames<-`(clinical_parameters$age_group)
  
  window_min_width_hosp <- ddays(3)
  window_min_width_ICU <- ddays(10)
  window_min_cases <- 100
  
  model_results_hosp <- map_dfr(age_groups, function(i_age_class) {
    
    model_data <- nindss_state %>%
      filter(age_group == i_age_class,
             date_onset >= date_start,
             date_onset <= date_end)
    
    cases_in_window <- function(start, end) {
      model_data %>% filter(date_onset >= start, date_onset <= end)
    }
    
    window_end <- date_end
    window_start <- window_end - window_min_width_hosp
    
    i_window <- 1
    
    window_data <- tibble()
    
    while(window_start > date_start - window_min_width_hosp) {
      
      while(nrow(cases_in_window(window_start, window_end)) < window_min_cases & window_start > date_start - window_min_width_hosp) {
        window_start <- window_start - ddays(1)
      }
      
      window_data <- bind_rows(
        window_data,
        cases_in_window(window_start, window_end) %>%
          mutate(window = i_window,
                 date_start = window_start,
                 date_end = window_end)
      )
      
      window_end <- window_start - ddays(1)
      window_start <- window_start - ddays(5)
      
      
      i_window <- i_window + 1
      
    }
    
    window_data_summ <- window_data %>%
      group_by(window_s = window, date_start, date_end) %>%
      summarise(n_hosp = sum(ever_in_hospital, na.rm = TRUE),
                n_cases = n(), .groups = "drop") %>%
      
      rowwise() %>%
      mutate(nothosp_days_since_onset = window_data %>%
               filter(window_s == window) %>%
               filter(!ever_in_hospital | is.na(ever_in_hospital)) %>%
               mutate(days_since_onset = forecast_dates$NNDSS - date_onset) %>%
               pull(days_since_onset) %>% as.numeric() %>% list())
    
    delay_hosp_shape <- clinical_parameter_lookup[i_age_class, "shape_onset_to_ward"]
    delay_hosp_scale <- clinical_parameter_lookup[i_age_class, "scale_onset_to_ward"]
    
    window_data_summ_adj <- window_data_summ %>%
    rowwise() %>%
    mutate(
      pr_hosp = n_hosp / n_cases,
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
      error = function(c) { return(pr_hosp) }),
    ) %>%
      
      mutate(age_group = i_age_class)
    
    window_data_summ_adj
  }) %>%
    rowwise() %>%
    mutate(date_mid = mean.Date(c(date_start, date_end))) %>%
    ungroup()
  
  
  model_results_ICU <- map_dfr(age_groups, function(i_age_class) {
    
    model_data <- nindss_state %>%
      filter(ever_in_hospital) %>%
      filter(age_group == i_age_class,
             date_onset >= date_start,
             date_onset <= date_end)
    
    hosp_in_window <- function(start, end) {
      model_data %>% filter(date_onset >= start, date_onset <= end)
    }
    
    window_end <- date_end
    window_start <- window_end - window_min_width_ICU
    
    i_window <- 1
    
    window_data <- tibble()
    
    while(window_start > date_start - window_min_width_ICU) {
      
      while(nrow(hosp_in_window(window_start, window_end)) < window_min_cases & window_start > date_start - window_min_width_ICU) {
        window_start <- window_start - ddays(1)
      }
      
      window_data <- bind_rows(
        window_data,
        hosp_in_window(window_start, window_end) %>%
          mutate(window = i_window,
                 date_start = window_start,
                 date_end = window_end)
      )
      
      window_end <- window_start - ddays(1)
      window_start <- window_start - window_min_width_ICU
      
      
      i_window <- i_window + 1
      
    }
    
    
    window_data_summ <- window_data %>%
      group_by(window_s = window, date_start, date_end) %>%
      summarise(n_hosp = n(),
                n_ICU = sum(ever_in_ICU, na.rm = TRUE), .groups = "drop") %>%
      
      rowwise() %>%
      mutate(notICU_days_since_onset = window_data %>%
               filter(window_s == window) %>%
               filter(!ever_in_ICU | is.na(ever_in_ICU)) %>%
               mutate(days_since_onset = forecast_dates$NNDSS - date_onset) %>%
               pull(days_since_onset) %>% as.numeric() %>% list()) 
    
    window_data_summ_adj <- window_data_summ %>%
      rowwise() %>%
      mutate(
        pr_ICU = n_ICU / n_hosp,
        
        pr_ICU_adj = tryCatch(pracma::fzero(
          function(x) {
            fn_score_ICU(x,
                         n_ICU,
                         notICU_days_since_onset,
                         delay_hosp_shape = clinical_parameter_lookup[i_age_class, "shape_onset_to_ward"],
                         delay_hosp_scale = clinical_parameter_lookup[i_age_class, "scale_onset_to_ward"],
                         
                         delay_ICU_shape = clinical_parameter_lookup[i_age_class,  "shape_ward_to_ICU"],
                         delay_ICU_scale = clinical_parameter_lookup[i_age_class, "scale_ward_to_ICU"])
          },
          
          x = c(0+.Machine$double.eps,1-.Machine$double.eps),
        )$x,
        error = function(c) { return(pr_ICU) })
      ) %>%
      
      mutate(age_group = i_age_class)
    
    window_data_summ_adj
  }) %>%
    rowwise() %>%
    mutate(date_mid = mean.Date(c(date_start, date_end))) %>%
    ungroup()
  
  
  p_hosp <- ggplot(model_results_hosp) +
    geom_line(aes(x = date_mid, y = pr_hosp_adj, color = age_group)) +
    geom_point(aes(x = date_mid, y = pr_hosp_adj, color = age_group),
               size = 0.5) +
    
    geom_linerange(aes(xmin = date_start - 1, xmax = date_end, y = pr_hosp_adj, color = age_group),
                   alpha = 0.2) +
  
    
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    scale_x_date(breaks = "months",
                 labels = scales::label_date_short()) +
    
    xlab("Date") + ylab("Probability") +
    
    
    theme_minimal() +
    
    coord_cartesian(xlim = c(date_start, date_end)) +
    
    ggtitle(paste0("Adjusted probabilities of hospitalisation - ", state_modelled),
            paste0("Estimates produced over windows of at least 100 cases/3 days.\n\n"))
  
  
  
  p_ICU <- ggplot(model_results_ICU) +
    geom_line(aes(x = date_mid, y = pr_ICU_adj, color = age_group)) +
    geom_point(aes(x = date_mid, y = pr_ICU_adj, color = age_group),
               size = 0.5) +
    
    geom_linerange(aes(xmin = date_start - 1, xmax = date_end, y = pr_ICU_adj, color = age_group),
                   alpha = 0.2) +
    
    
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    scale_x_date(breaks = "months",
                 labels = scales::label_date_short()) +
    
    xlab("Date") + ylab("Probability") +
    
    coord_cartesian(xlim = c(date_start, date_end)) +
    
    
    theme_minimal() +
    
    ggtitle(paste0("Adjusted probabilities of ICU admission given hospitalisation - ", state_modelled),
            paste0("Estimates produced over windows of at least 100 hospitalisations/10 days.\n\n"))
  
  cowplot::plot_grid(
    p_hosp, p_ICU, ncol = 1
  )
  
  ggsave(
    paste0(plot_dir, "/", "morbidity_", state_modelled, ".png"),
    
    width = 12, height = 8, bg = "white"
  )
  
  
  
  hosp_timeseries <- model_results_hosp %>%
    rowwise() %>%
    mutate(dates = list(seq(date_start, date_end, by = "days"))) %>%
    unnest(dates) %>%
    
    select(date_onset = dates, age_group, pr_hosp_adj) %>%
    
    arrange(age_group, date_onset)
  
  ICU_timeseries <- model_results_ICU %>%
    rowwise() %>%
    mutate(dates = list(seq(date_start, date_end, by = "days"))) %>%
    unnest(dates) %>%
    
    select(date_onset = dates, age_group, pr_ICU_adj) %>%
    
    arrange(age_group, date_onset)
  
  
  full_timeseries <- hosp_timeseries %>%
    left_join(ICU_timeseries) %>%
    filter(date_onset > date_start) %>%
    
    rename(pr_hosp = pr_hosp_adj, pr_ICU = pr_ICU_adj) %>%
    
    right_join(expand_grid(date_onset = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = 'days'),
                           age_group = unique(nindss_state$age_group))) %>%
    arrange(date_onset) %>%
    
    group_by(age_group) %>%
    fill(pr_hosp, pr_ICU, .direction = 'updown') %>%
    ungroup()
  
  
  ggplot(full_timeseries %>%
           pivot_longer(c(pr_hosp, pr_ICU))) +
    geom_step(aes(x = date_onset, y = value, color = name)) +
    
    theme_minimal() +
    
    coord_cartesian(xlim = c(date_start, date_end)) +
    
    xlab(NULL) + ylab(NULL) +
    
    scale_color_brewer("", type = "qual", palette = 2) +
    
    facet_wrap(~age_group, scales = "free_y") +
    theme(legend.position = "bottom")
  
  ggsave(
    paste0(plot_dir, "/", "morbidity_2_", state_modelled, ".png"),
    
    width = 12, height = 8, bg = "white"
  )
  
  full_timeseries
}