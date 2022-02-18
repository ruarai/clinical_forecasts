

make_clinical_prob_table <- function(
  nindss,
  forecast_dates,
  clinical_parameters,
  
  state_modelled,
  national_clinical_prob_table,
  
  plot_dir
) {
  
  require(tidyverse)
  require(lubridate)
  
  # Need to improve this, but for now return the national-average
  # clinical probabilities for states without large epidemics
  if(!(state_modelled %in% c("VIC", "NSW", "national"))) {
    return(national_clinical_prob_table)
  }
  
  
  dir.create(plot_dir, showWarnings = FALSE)
  
  
  print("Starting...")
  
  NNDSS_linelist <- nindss %>%
    filter(date_onset >= forecast_dates$simulation_start)
  
  
  require(data.table)
  setDTthreads(threads = 1)
  
  require(future.callr)
  require(furrr)
  
  dt_linelist <- NNDSS_linelist %>%
    select(age_group, date_onset, status_hospital, status_ICU, age_group) %>%
    
    data.table(key = c("age_group", "date_onset",
                       "status_hospital", "status_ICU"))
    
  
  data_date <- forecast_dates$NNDSS
  
  
  
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
                       delay_hosp_shape[i], delay_hosp_scale[i],
                       delay_ICU_shape[i], delay_ICU_scale[i])
    })
    
    
    
    A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
  }
  
  
  
  
  prob_naive <- function(data, numer_fn, denom_fn = identity) {
    n_numer <- nrow(data %>% numer_fn)
    n_denom <- nrow(data %>% denom_fn)
    
    return(n_numer / n_denom)
  }
  
  fix_prob <- function(p) {
    case_when(is.infinite(p) ~ 0,
              is.na(p) ~ 0,
              TRUE ~ pmin(pmax(p, 0), 1))
  }
  
  pr_hosp_total <- prob_naive(NNDSS_linelist,
                              function(x) x %>% filter(status_hospital == 1))
  
  pr_ICU_total <- prob_naive(NNDSS_linelist,
                             function(x) x %>% filter(status_ICU == 1),
                             function(x) x %>% filter(status_hospital == 1))
  
  clinical_parameter_lookup <- clinical_parameters %>%
    select(-age_group) %>%
    as.matrix() %>%
    `rownames<-`(clinical_parameters$age_group)
  
  
  clinical_probs <- function(date_start) {
    n_days_forward <- 14
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_linelist[.(dates), on = "date_onset",
                                  nomatch = NULL]
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
    
    prob_hosp_naive <- nrow(cases_hospitalised) / nrow(cases_on_dates)
    
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
    
    prob_ICU_naive <- nrow(cases_ICU) / nrow(cases_hospitalised)
    
    # Only perform right-truncation adjustment in the last 50 days as we
    # expect no effect before this time (>99.99% quantile)
    
    if(data_date - date_start <= ddays(50)) {
      
      nothosp_days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
      
      ## TODO: Finish this off when we can make sensible estimates here.
      
      delay_hosp_shape <- clinical_parameter_lookup[cases_not_hospitalised$age_group,
                                                    "shape_onset_to_ward"]
      
      delay_hosp_scale <- clinical_parameter_lookup[cases_not_hospitalised$age_group,
                                                    "scale_onset_to_ward"]
      
      
      prob_hosp_MLE <- tryCatch(pracma::fzero(
        function(x) {
          fn_score_hosp(x, 
                        nrow(cases_hospitalised),
                        nothosp_days_since_onset,
                        delay_shape = delay_hosp_shape, 
                        delay_scale = delay_hosp_scale)
        },
        
        x = c(0+.Machine$double.eps,1-.Machine$double.eps),
      )$x,
      error = function(c) { return(prob_hosp_naive) })
      
      
      notICU_days_since_onset <- as.numeric(data_date - cases_not_ICU$date_onset)
      
      delay_ICU_shape <- clinical_parameter_lookup[cases_not_hospitalised$age_group,
                                                    "shape_ward_to_ICU"]
      
      delay_ICU_scale <- clinical_parameter_lookup[cases_not_hospitalised$age_group,
                                                    "scale_ward_to_ICU"]
      
      
      prob_ICU_MLE <- prob_ICU_naive
      # BROKEN 28/01/2022
      # Adjustment has very little effect in past
      # 
      # prob_ICU_MLE <- tryCatch(pracma::fzero(
      #   function(x) {
      #     fn_score_ICU(x, 
      #                  nrow(cases_ICU),
      #                  notICU_days_since_onset,
      #                  delay_hosp_shape = delay_hosp_shape, 
      #                  delay_hosp_scale = delay_hosp_scale,
      #                  
      #                  delay_ICU_shape = delay_ICU_shape,
      #                  delay_ICU_scale = delay_ICU_scale)
      #   },
      #   
      #   x = c(0+.Machine$double.eps,1-.Machine$double.eps),
      # )$x,
      # error = function(c) { return(prob_ICU_naive) })
      
      
      
    } else{
      prob_hosp_MLE <- prob_hosp_naive
      prob_ICU_MLE <- prob_ICU_naive
    }
    
    
    tibble(
      prob_hosp_MLE = prob_hosp_MLE,
      prob_hosp_naive = prob_hosp_naive,
      
      prob_ICU_MLE = prob_ICU_MLE,
      prob_ICU_naive = prob_ICU_naive,
      
      n_cases_hospitalised = nrow(cases_hospitalised),
      
      n_cases_ICU = nrow(cases_ICU)
    )
  }
  
  
  print("Calculating age-independent probability timeseries...")
  
  
  
  clinical_probs_results <- tibble(date = seq(min(NNDSS_linelist$date_onset),
                                              max(NNDSS_linelist$date_onset) - 14,
                                              by = 'days')) %>%
    group_by(date) %>%
    summarise(clinical_probs(date)) %>%
    
    mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 50),
           weight_use_ICU = pmin(1, n_cases_ICU / 50)) %>%
    
    mutate(pr_hosp = prob_hosp_MLE * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
           pr_hosp_naive = prob_hosp_naive * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
           
           pr_ICU = prob_ICU_MLE * weight_use_ICU + pr_ICU_total * (1 - weight_use_ICU),
           pr_ICU_naive = prob_ICU_naive * weight_use_ICU + pr_hosp_total * (1 - weight_use_ICU)) %>%
    
    select(date, pr_hosp, pr_hosp_naive, pr_ICU, pr_ICU_naive)
  
  clinical_probs_plot <- clinical_probs_results %>%
    pivot_longer(cols = c(pr_hosp, pr_ICU, pr_hosp_naive, pr_ICU_naive)) %>%
    mutate(type = if_else(str_detect(name, "naive"), "naive", "adjusted"),
           name = str_remove(name, "_naive"))
  
  ggplot(clinical_probs_plot) +
    geom_line(aes(x = date, y = value, linetype = type)) +
    facet_grid(rows = vars(name),
               scales = "free_y") +
    
    geom_hline(aes(yintercept = y),
               data = tibble(name = "pr_hosp", y = pr_hosp_total),
               linetype = 'dotted') +
    
    geom_hline(aes(yintercept = y),
               data = tibble(name = "pr_ICU", y = pr_ICU_total),
               linetype = 'dotted') +
    
    coord_cartesian(ylim = c(0, NA)) +
    
    scale_x_date(date_breaks = "months",
                 labels = scales::label_date_short()) +
    
    theme_minimal() +
    theme(legend.position = 'bottom') +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-independent clinical probability timeseries",
            "With right truncation adjustment.")
  
  
  
  ggsave(paste0(plot_dir, state_modelled, "_morbidity_timeseries.png"),
         height = 9, width = 12, bg = 'white')
  
  clinical_probs_by_ageclass <- function(date_start, filter_age_group,
                                         n_days_forward = 14) {
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_linelist[.(dates), on = "date_onset",
                                  nomatch = NULL]
    
    cases_age <- cases_on_dates[.(filter_age_group), on = "age_group", nomatch = NULL]
    
    pr_age_given_case <- nrow(cases_age) / nrow(cases_on_dates)
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_hospitalised_age <- cases_hospitalised[.(filter_age_group), on = "age_group", nomatch = NULL]
    
    pr_age_given_hosp <- nrow(cases_hospitalised_age) / nrow(cases_hospitalised)
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_ICU_age <- cases_ICU[.(filter_age_group), on = "age_group", nomatch = NULL]
    
    pr_age_given_ICU <- nrow(cases_ICU_age) / nrow(cases_ICU)
    
    
    
    
    tibble(
      pr_age_given_case,
      n_cases = nrow(cases_age),
      
      pr_age_given_hosp,
      n_hosps = nrow(cases_hospitalised_age),
      
      pr_age_given_ICU,
      n_ICUs = nrow(cases_ICU_age)
      
    )
  }
  
  print("Calculating timeseries by age...")
  
  results_by_ageclass <- expand_grid(
    date = seq(min(NNDSS_linelist$date_onset),max(NNDSS_linelist$date_onset) - 14, by = 'days'),
    age_group = unique(NNDSS_linelist$age_group)
  ) %>%
    
    group_by(date, age_group) %>%
    summarise(clinical_probs_by_ageclass(date, age_group))
  
  
  
  results_by_ageclass_total <- expand_grid(
    age_group = unique(NNDSS_linelist$age_group)
  ) %>%
    group_by(age_group) %>%
    summarise(clinical_probs_by_ageclass(forecast_dates$simulation_start, 
                                         age_group,
                                         n_days_forward = 1000)) %>%
    
    rename_with(.cols = starts_with("pr_"),
                .fn = ~ str_c(. , "_total"))
  
  
  
  results_combined <- results_by_ageclass %>% 
    left_join(results_by_ageclass_total %>%
                select(-c(n_cases, n_hosps, n_ICUs))) %>%
    
    mutate(weight_use_cases = pmin(1, n_cases / 50),
           weight_use_hosp = pmin(1, n_hosps / 50),
           weight_use_ICU = pmin(1, n_ICUs / 50))  %>%
    
    mutate(pr_age_given_case = pr_age_given_case * weight_use_cases + pr_age_given_case_total * (1 - weight_use_cases),
           
           pr_age_given_hosp = pr_age_given_hosp * weight_use_hosp + pr_age_given_hosp_total * (1 - weight_use_hosp),
           
           pr_age_given_ICU = pr_age_given_ICU * weight_use_ICU + pr_age_given_ICU_total * (1 - weight_use_ICU)) %>%
    
    select(date, age_group, pr_age_given_case, pr_age_given_hosp, pr_age_given_ICU) %>%
    
    
    left_join(clinical_probs_results) %>%
    
    mutate(pr_hosp_given_case_and_age = (pr_age_given_hosp * pr_hosp) / (pr_age_given_case),
           
           pr_ICU_given_hosp_and_age = (pr_age_given_ICU * pr_ICU) / (pr_age_given_hosp)) %>%
    
    select(date, age_group,
           pr_hosp_given_case_and_age,
           pr_ICU_given_hosp_and_age)
  
  
  results_combined_plot <- results_combined %>%
    pivot_longer(cols = c(pr_hosp_given_case_and_age,
                          pr_ICU_given_hosp_and_age)) %>%
    
    mutate(age_group = factor(age_group, levels = unique(clinical_parameters$age_group)))
  
  ggplot(results_combined_plot) +
    geom_line(aes(x = date, y = value, color = age_group)) +
    
    facet_wrap(~name, ncol = 1) +
    
    scale_color_viridis_d() +
    
    scale_x_date(date_breaks = "months",
                 labels = scales::label_date_short()) +
    
    theme_minimal() +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-specific morbidity probability timeseries")
  
  
  ggsave(paste0(plot_dir, state_modelled, "_morbidity_age_timeseries.png"),
         height = 9, width = 12, bg = 'white')
  
  print("Saving results...")
  
  clinical_probabilities <- results_combined %>%
    ungroup() %>%
    mutate(date_onset = date + 7) %>%
    
    select(date_onset, age_group, 
           pr_hosp = pr_hosp_given_case_and_age, 
           pr_ICU = pr_ICU_given_hosp_and_age) %>%
    
    right_join(expand_grid(date_onset = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = 'days'),
                           age_group = unique(NNDSS_linelist$age_group))) %>%
    arrange(date_onset) %>%
    
    group_by(age_group) %>%
    fill(pr_hosp, pr_ICU, .direction = 'updown') %>%
    ungroup()
  
  clinical_probabilities
}
