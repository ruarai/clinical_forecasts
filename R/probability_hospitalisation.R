

make_clinical_prob_table <- function(simulation_options,
                                     model_parameters) {
  
  require(tidyverse)
  require(data.table)
  require(lubridate)
  
  
  
  NNDSS_linelist <- read_rds(simulation_options$files$NNDSS_linelist) %>%
    filter(date_onset >= simulation_options$dates$simulation_start,
           state == simulation_options$state_modelled)
  
  
  dt_linelist <- data.table(NNDSS_linelist,
                            key = c("age_class", "date_onset",
                                    "status_hospital", "status_ICU"))
  
  data_date <- simulation_options$dates$NNDSS
  
  
  
  fn_score_hosp <- function(x, A, days_since_onset, delay_shape, delay_mean) {
    prob_already_observed <- pgamma(days_since_onset,
                                    shape = delay_shape,
                                    scale = delay_mean / delay_shape)
    
    
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
  
  
  
  
  
  clinical_probs <- function(date_start) {
    n_days_forward <- 14
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_linelist[.(dates), on = "date_onset",
                                  nomatch = NULL]
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_not_hospitalised <- cases_on_dates[!.(1), on = "status_hospital"]
    
    
    nothosp_days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
    
    delay_shape <- model_parameters$delay_params$
      compartment_LoS_shape[cases_not_hospitalised$age_class,"symptomatic_to_ED"]
    delay_mean <- model_parameters$delay_params$
      compartment_LoS_shape[cases_not_hospitalised$age_class,"symptomatic_to_ED"]
    
    
    prob_hosp_MLE <- tryCatch(pracma::fzero(
      function(x) {
        fn_score_hosp(x, 
                      nrow(cases_hospitalised),
                      nothosp_days_since_onset,
                      delay_shape, delay_mean)
      },
      
      x = c(0+.Machine$double.eps,1-.Machine$double.eps),
    )$x,
    error = function(c) { return(pr_hosp_total) })
    
    
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_not_ICU <- cases_hospitalised[!.(1), on = "status_ICU"]
    
    
    
    
    
    tibble(
      prob_hosp_MLE = prob_hosp_MLE,
      prob_hosp_naive = nrow(cases_hospitalised) / nrow(cases_on_dates),
      
      prob_ICU = nrow(cases_ICU) / nrow(cases_hospitalised),
      
      n_cases_hospitalised = nrow(cases_hospitalised),
      
      n_cases_ICU = nrow(cases_ICU)
    )
  }
  
  
  
  
  clinical_probs_results <- tibble(date = seq(min(NNDSS_linelist$date_onset),
                                              max(NNDSS_linelist$date_onset) - 14,
                                              by = 'days')) %>%
    group_by(date) %>%
    summarise(clinical_probs(date)) %>%
    
    mutate(weight_use_hosp = pmin(1, n_cases_hospitalised / 50),
           weight_use_ICU = pmin(1, n_cases_ICU / 50)) %>%
    
    mutate(pr_hosp = prob_hosp_MLE * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
           pr_hosp_naive = prob_hosp_naive * weight_use_hosp + pr_hosp_total * (1 - weight_use_hosp),
           
           pr_ICU = prob_ICU * weight_use_ICU + pr_ICU_total * (1 - weight_use_ICU)) %>%
    
    select(date, pr_hosp, pr_hosp_naive, pr_ICU)
  
  clinical_probs_plot <- clinical_probs_results %>%
    pivot_longer(cols = c(pr_hosp, pr_ICU, pr_hosp_naive)) %>%
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
    
    theme_minimal() +
    theme(legend.position = 'bottom') +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-independent clinical probability timeseries",
            "With right truncation adjustment (for hospitalisation)")
  
  
  
  clinical_probs_by_ageclass <- function(date_start, filter_age_class,
                                         n_days_forward = 14) {
    dates <- seq(date_start, date_start + n_days_forward, by = 'day')
    
    cases_on_dates <- dt_linelist[.(dates), on = "date_onset",
                                  nomatch = NULL]
    
    cases_age <- cases_on_dates[.(filter_age_class), on = "age_class", nomatch = NULL]
    
    pr_age_given_case <- nrow(cases_age) / nrow(cases_on_dates)
    
    cases_hospitalised <- cases_on_dates[.(1), on = "status_hospital", nomatch = NULL]
    cases_hospitalised_age <- cases_hospitalised[.(filter_age_class), on = "age_class", nomatch = NULL]
    
    pr_age_given_hosp <- nrow(cases_hospitalised_age) / nrow(cases_hospitalised)
    
    cases_ICU <- cases_hospitalised[.(1), on = "status_ICU"]
    cases_ICU_age <- cases_ICU[.(filter_age_class), on = "age_class", nomatch = NULL]
    
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
  
  
  results_by_ageclass <- expand_grid(
    date = seq(min(NNDSS_linelist$date_onset),max(NNDSS_linelist$date_onset) - 14, by = 'days'),
    age_class = unique(NNDSS_linelist$age_class)
  ) %>%
    
    group_by(date, age_class) %>%
    summarise(clinical_probs_by_ageclass(date, age_class))
  
  
  
  results_by_ageclass_total <- expand_grid(
    age_class = unique(NNDSS_linelist$age_class)
  ) %>%
    group_by(age_class) %>%
    summarise(clinical_probs_by_ageclass(simulation_options$dates$simulation_start, 
                                         age_class,
                                         n_days_forward = 1000)) %>%
    
    rename_with(.cols = starts_with("pr_"),
                .fn = ~ str_c(. , "_total"))
  
  
  
  results_combined <- results_by_ageclass %>% 
    left_join(results_by_ageclass_total) %>%
    
    mutate(weight_use_cases = pmin(1, n_cases / 50),
           weight_use_hosp = pmin(1, n_hosps / 50),
           weight_use_ICU = pmin(1, n_ICUs / 50))  %>%
    
    mutate(pr_age_given_case = pr_age_given_case * weight_use_cases + pr_age_given_case_total * (1 - weight_use_cases),
           
           pr_age_given_hosp = pr_age_given_hosp * weight_use_hosp + pr_age_given_hosp_total * (1 - weight_use_hosp),
           
           pr_age_given_ICU = pr_age_given_ICU * weight_use_ICU + pr_age_given_ICU_total * (1 - weight_use_ICU)) %>%
    
    select(date, age_class, pr_age_given_case, pr_age_given_hosp, pr_age_given_ICU) %>%
    
    
    left_join(clinical_probs_results) %>%
    
    mutate(pr_hosp_given_case_and_age = (pr_age_given_hosp * pr_hosp) / (pr_age_given_case),
           
           pr_ICU_given_hosp_and_age = (pr_age_given_ICU * pr_ICU) / (pr_age_given_hosp)) %>%
    
    select(date, age_class,
           pr_hosp_given_case_and_age,
           pr_ICU_given_hosp_and_age)
  
  
  results_combined_plot <- results_combined %>%
    pivot_longer(cols = c(pr_hosp_given_case_and_age,
                          pr_ICU_given_hosp_and_age)) %>%
    
    mutate(age_class = factor(age_class, levels = model_parameters$covariates_age))
  
  ggplot(results_combined_plot) +
    geom_line(aes(x = date, y = value, color = age_class)) +
    
    facet_wrap(~name, ncol = 1) +
    
    theme_minimal() +
    xlab("Date") + ylab("Probability") +
    ggtitle("Age-specific morbidity probability timeseries")
  
  
  ggsave(paste0(simulation_options$dirs$plots, "/morbidity_age_timeseries.png"),
         height = 9, width = 12, bg = 'white')
  
  
  
  clinical_probabilities <- results_combined %>%
    ungroup() %>%
    mutate(date_onset = date + 7) %>%
    
    select(date_onset, age_class, 
           pr_hosp = pr_hosp_given_case_and_age, 
           pr_ICU = pr_ICU_given_hosp_and_age) %>%
    
    right_join(expand_grid(date_onset = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = 'days'),
                           age_class = unique(NNDSS_linelist$age_class))) %>%
    arrange(date_onset) %>%
    
    group_by(age_class) %>%
    fill(pr_hosp, pr_ICU, .direction = 'updown') %>%
    ungroup()
  
  
  write_rds(clinical_probabilities,
            simulation_options$files$clinical_prob_table)
  
  
  
}
