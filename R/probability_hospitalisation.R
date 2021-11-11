

make_clinical_prob_table <- function(simulation_options,
                                     model_params) {
  
  require(tidyverse)
  require(data.table)
  require(lubridate)
  
  
  
  
  clinical_linelist <- read_rds(simulation_options$files$NNDSS_linelist) %>%
    filter(date_onset >= simulation_options$dates$simulation_start,
           state == simulation_options$state_modelled) %>%
    
    mutate(age_class_factor = factor(age_class, levels = model_params$covariates_age)) %>%
    arrange(date_onset, age_class_factor)
  
  
  dt_clinical_linelist <- data.table(clinical_linelist,
                                     key = c("age_class", "date_onset",
                                             "status_hospital", "status_ICU"))
  
  data_date <- simulation_options$dates$NNDSS
  
  
  fn_score <- function(x, A, days_since_onset, delay_shape, delay_mean) {
    prob_already_observed <- pgamma(days_since_onset, shape = delay_shape, scale = delay_mean / delay_shape)
    
    
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
  
  pr_hosp_total <- prob_naive(clinical_linelist,
                              function(x) x %>% filter(status_hospital == 1))
  
  pr_ICU_total <- prob_naive(clinical_linelist,
                             function(x) x %>% filter(status_ICU == 1),
                             function(x) x %>% filter(status_hospital == 1))
  
  
  pr_total_by_age <- clinical_linelist %>%
    group_by(age_class) %>%
    do(pr_hosp_total_age = prob_naive(., function(x) x %>% filter(status_hospital == 1)),
       pr_ICU_total_age = prob_naive(.,
                                     function(x) x %>% filter(status_ICU == 1),
                                     function(x) x %>% filter(status_hospital == 1))) %>%
    
    unnest(c(pr_hosp_total_age, pr_ICU_total_age)) %>%
    
    mutate(age_class = factor(age_class, levels = model_params$covariates_age))
  
  ggplot() +
    geom_point(aes(x = age_class, y = pr_hosp_total_age, color = 'hospitalisation|case'),
               pr_total_by_age,
               position = position_nudge(x = -0.1)) +
    geom_point(aes(x = age_class, y = pr_ICU_total_age, color = 'ICU|hospitalisation'),
               pr_total_by_age,
               position = position_nudge(x = 0.1)) +
    
    geom_hline(aes(color = 'hospitalisation|case', yintercept = pr_hosp_total),
               linetype = 'dotted') +
    
    geom_hline(aes(color = 'ICU|hospitalisation', yintercept = pr_ICU_total),
               linetype = 'dotted') +
    
    ggtitle("Probabilities over total time period",
            paste0(min(clinical_linelist$date_onset), " to ", max(clinical_linelist$date_onset))) +
    xlab("Age class") + ylab("Probability") +
    
    theme_minimal() +
    theme(legend.position = 'bottom')
  
  
  ggsave(paste0(simulation_options$dirs$plots, "/morbidity_total_probs.png"),
         height = 6, width = 8, bg = 'white')
  
  
  
  clinical_probs_ageless <- function(date_start) {
    n_days_forward <- 14
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_clinical_linelist[.(dates), on = "date_onset",
                                           nomatch = NULL]
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
    
    
    nothosp_days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
    
    # TODO FIXME
    delay_shape <- 2
    delay_mean <- 5
    
    
    prob_hosp_MLE <- tryCatch(pracma::fzero(
      function(x) {fn_score(x, nrow(cases_hospitalised),
                            nothosp_days_since_onset,
                            delay_shape, delay_mean)},
      
      x = c(0+.Machine$double.eps,1-.Machine$double.eps),
    )$x,
    error = function(c) { return(pr_hosp_total) })
    
    
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
    
    
    
    
    
    tibble(
      prob_hosp_MLE = prob_hosp_MLE,
      
      prob_ICU = nrow(cases_ICU) / nrow(cases_not_ICU),
      
      n_cases_hospitalised = nrow(cases_hospitalised),
      
      n_cases_ICU = nrow(cases_ICU)
    )
  }
  
  
  results_ageless <- tibble(date = seq(min(clinical_linelist$date_onset),
                                       max(clinical_linelist$date_onset) - 14,
                                       by = 'days')) %>%
    group_by(date) %>%
    summarise(clinical_probs_ageless(date)) %>%
    
    mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 50),
           weight_use_ICU = pmin(1, n_cases_ICU / 50)) %>%
    
    mutate(pr_hosp = prob_hosp_MLE * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
           
           pr_ICU = prob_ICU * weight_use_ICU + pr_ICU_total * (1 - weight_use_ICU)) %>%
    
    select(date, pr_hosp, pr_ICU)
  
  
  ggplot(results_ageless %>% pivot_longer(cols = c(pr_hosp, pr_ICU))) +
    geom_line(aes(x = date, y = value)) +
    facet_grid(rows = vars(name)) +
    
    geom_hline(aes(yintercept = y),
               data = tibble(name = "pr_hosp", y = pr_hosp_total),
               linetype = 'dotted') +
    
    geom_hline(aes(yintercept = y),
               data = tibble(name = "pr_ICU", y = pr_ICU_total),
               linetype = 'dotted') +
    
    theme_minimal() +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-independent clinical probability timeseries",
            "With right truncation adjustment (for hospitalisation)")
  
  
  
  ggsave(paste0(simulation_options$dirs$plots, "/morbidity_total_timeseries.png"),
         height = 6, width = 8, bg = 'white')
  
  clinical_probs_by_ageclass <- function(date_start, filter_age_class) {
    
    n_days_forward <- 14
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_clinical_linelist[.(filter_age_class, dates), on = c("age_class", "date_onset"),
                                           nomatch = NULL]
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
    
    nothosp_days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
    
    # TODO FIXME
    delay_shape <- 2
    delay_mean <- 5
    
    
    prob_hosp_MLE <- tryCatch(pracma::fzero(
      function(x) {fn_score(x, nrow(cases_hospitalised),
                            nothosp_days_since_onset,
                            delay_shape, delay_mean)},
      
      x = c(0+.Machine$double.eps,1-.Machine$double.eps),
    )$x,
    error = function(c) { return(pr_hosp_total) })
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
    
    tibble(
      prob_hosp = fix_prob(prob_hosp_MLE),
      prob_hosp_naive = fix_prob(nrow(cases_hospitalised) / nrow(cases_on_dates)),
      
      prob_ICU = fix_prob(nrow(cases_ICU) / nrow(cases_not_ICU)),
      
      n_cases_hospitalised = nrow(cases_hospitalised),
      
      n_cases_ICU = nrow(cases_ICU)
    )
  }
  
  
  results_by_ageclass <- expand_grid(
    date = seq(min(clinical_linelist$date_onset),max(clinical_linelist$date_onset) - 14, by = 'days'),
    age_class = unique(clinical_linelist$age_class)
  ) %>%
    
    group_by(date, age_class) %>%
    summarise(clinical_probs_by_ageclass(date, age_class)) %>%
    
    left_join(pr_total_by_age) %>%
    
    mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 25),
           weight_use_ICU = pmin(1, n_cases_ICU / 25)) %>%
    
    mutate(pr_hosp = prob_hosp * weight_use_hosp + pr_hosp_total_age * (1 - weight_use_hosp),
           
           pr_ICU = prob_ICU * weight_use_ICU + pr_ICU_total_age * (1 - weight_use_ICU))
  
  age_prob_timeseries_plot_data <- results_by_ageclass %>%
    mutate(age_class = factor(age_class, levels = model_params$covariates_age))
  
  
  ggplot(age_prob_timeseries_plot_data) +
    geom_line(aes(x = date, y = prob_hosp, color = 'MLE')) +
    geom_line(aes(x = date, y = prob_hosp_naive, color = 'direct/naive')) +
    geom_line(aes(x = date, y = pr_hosp, color = 'corrected MLE')) +
    geom_hline(aes(yintercept = pr_hosp_total_age), linetype = 'dotted',
               pr_total_by_age) +
    
    facet_wrap(~age_class, scales = "free_y") +
    coord_cartesian(xlim = c(max(age_prob_timeseries_plot_data$date) - 30, max(age_prob_timeseries_plot_data$date))) +
    
    theme_minimal() +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-specific morbidity probability timeseries")
  
  
  ggsave(paste0(simulation_options$dirs$plots, "/morbidity_age_timeseries.png"),
         height = 9, width = 12, bg = 'white')
  
  
  clinical_probabilities <- results_by_ageclass %>%
    ungroup() %>%
    rename(date_onset = date) %>%
    
    select(date_onset, age_class, pr_hosp, pr_ICU) %>%
    
    right_join(expand_grid(date_onset = seq(ymd("2021-01-01"), ymd("2022-01-01"), by = 'days'),
                           age_class = unique(clinical_linelist$age_class))) %>%
    arrange(date_onset) %>%
    
    group_by(age_class) %>%
    fill(pr_hosp, pr_ICU, .direction = 'updown') %>%
    ungroup()
  
  
  write_rds(clinical_probabilities,
            simulation_options$files$clinical_prob_table)
  
  
  
}
