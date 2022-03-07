
moving_window_morbidity_plots <- function(
  nindss_state,
  forecast_dates,
  clinical_parameters,
  
  state_modelled,
  
  plot_dir
) {
  
  date_start <- forecast_dates$simulation_start
  date_end <- forecast_dates$NNDSS - ddays(2)
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  
  
  clinical_parameter_lookup <- clinical_parameters %>%
    
    
    mutate(scale_onset_to_ward = scale_onset_to_ward * 0.7,
           shape_onset_to_ward = shape_onset_to_ward * 0.7) %>%
    
    select(-age_group) %>%
    as.matrix() %>%
    `rownames<-`(clinical_parameters$age_group)
  
  window_min_width_hosp <- ddays(3)
  window_min_width_ICU <- ddays(10)
  window_min_cases <- 100
  
  library(furrr)
  library(future.callr)
  plan(callr, workers = 9)
  
  
  model_results_hosp <- future_map_dfr(age_groups, function(i_age_class) {
    
    map_dfr(
      1:10, function(i_sample) {
        
        model_data <- nindss_state %>%
          filter(age_group == i_age_class,
                 date_onset >= date_start,
                 date_onset <= date_end) %>%
          
          sample_n(size = n(), replace = TRUE)
        
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
          window_start <- window_start - ddays(round(runif(1, min = 3, max = 7)))
          
          
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
          
          mutate(age_group = i_age_class,
                 sample = i_sample)
        
        window_data_summ_adj
      }
    )
    
  },
    .options = furrr_options(seed = TRUE)
  ) %>%
    rowwise() %>%
    mutate(date_mid = mean.Date(c(date_start, date_end))) %>%
    ungroup()
  

  model_results_ICU <- map_dfr(age_groups, function(i_age_class) {

    map_dfr(
      1:10, function(i_sample) {

        model_data <- nindss_state %>%
          filter(ever_in_hospital) %>%
          filter(age_group == i_age_class,
                 date_onset >= date_start,
                 date_onset <= date_end) %>%

          sample_n(size = n(), replace = TRUE)

        hosp_in_window <- function(start, end) {
          model_data %>% filter(date_onset >= start, date_onset <= end)
        }

        window_end <- date_end
        window_start <- window_end - window_min_width_ICU

        i_window <- 1

        window_data <- tibble()

        while(window_start > date_start - window_min_width_ICU) {

          while(nrow(hosp_in_window(window_start, window_end)) < window_min_cases * 5 & window_start > date_start - window_min_width_ICU) {
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
          window_start <- window_start - ddays(round(runif(1, 10, 14)))


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

            pr_ICU_adj = if_else(
              as.numeric(forecast_dates$NNDSS - date_end) > 40,
              pr_ICU,
              tryCatch(pracma::fzero(
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
            error = function(c) { return(pr_ICU) }))
          ) %>%

          mutate(age_group = i_age_class,
                 sample = i_sample)
        window_data_summ_adj
      }
    )


  }) %>%
    rowwise() %>%
    mutate(date_mid = mean.Date(c(date_start, date_end))) %>%
    ungroup()

  
  ggplot(model_results_hosp) +
    
    geom_linerange(aes(xmin = date_start - 1, xmax = date_end, y = pr_hosp_adj, color = age_group),
                   alpha = 0.4) +
    
    geom_smooth(aes(x = date_mid, y = pr_hosp_adj, color = age_group, group = age_group),
                fill = 'transparent',
                method = "gam") +
  
    
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    scale_x_date(breaks = "months",
                 labels = scales::label_date_short()) +
    
    xlab("Date") + ylab("Probability") +
    
    
    theme_minimal() +
    theme(legend.position = "none") +
    
    facet_wrap(~age_group, scales = "free_y") +
    
    coord_cartesian(xlim = c(date_start, date_end)) +
    
    ggtitle(paste0("Adjusted probabilities of hospitalisation - ", state_modelled),
            paste0("Estimates produced over windows of at least 100 cases/3 days.\n\n"))
  
  
  
  ggsave(
    paste0(plot_dir, "/", "morbidity_ts_", state_modelled, ".png"),
    
    width = 12, height = 8, bg = "white"
  )
  
  
  ggplot(model_results_ICU) +
    geom_linerange(aes(xmin = date_start - 1, xmax = date_end, y = pr_ICU_adj, color = age_group),
                   alpha = 0.4) +
    
    geom_smooth(aes(x = date_mid, y = pr_ICU_adj, color = age_group, group = age_group),
                fill = 'transparent',
                method = "gam") +
    
    
    ggokabeito::scale_color_okabe_ito(name = "Age group") +
    
    scale_x_date(breaks = "months",
                 labels = scales::label_date_short()) +
    
    xlab("Date") + ylab("Probability") +
    
    
    theme_minimal() +
    theme(legend.position = "none") +
    
    facet_wrap(~age_group, scales = "free_y") +
    
    coord_cartesian(xlim = c(date_start, date_end)) +
    
    ggtitle(paste0("Adjusted probabilities of hospitalisation - ", state_modelled),
            paste0("Estimates produced over windows of at least 100 cases/3 days.\n\n"))
  
  
  ggsave(
    paste0(plot_dir, "/", "morbidity_ICU_ts_", state_modelled, ".png"),
    
    width = 12, height = 8, bg = "white"
  )
  
  return(NULL)
}