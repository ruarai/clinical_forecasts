
make_morbidity_estimates <- function(
  nindss_state,
  
  nindss_date,
  simulation_start_date,
  
  clinical_parameters,
  
  state_modelled,
  
  national_morbidity_estimates
) {
  
  
  
  # Return the national-average clinical probabilities for states without good data
  if(!(state_modelled %in% c("VIC", "NSW", "national"))) {
    return(national_morbidity_estimates)
  }
  
  if(state_modelled == "national") {
    # Producing national-average results across VIC and NSW (excluding all other states for data quality)
    nindss_state <- nindss_state %>%
      filter(state %in% c("VIC", "NSW"))
  }
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  clinical_parameter_lookup <- clinical_parameters %>%
    
    
    mutate(scale_onset_to_ward = scale_onset_to_ward * 0.7,
           shape_onset_to_ward = shape_onset_to_ward * 0.7) %>%
    
    select(-age_group) %>%
    as.matrix() %>%
    `rownames<-`(clinical_parameters$age_group)
  
  
  nindss_state <- nindss_state %>%
    filter(date_onset >= nindss_date - ddays(60)) %>%
    select(date_onset, age_group, ever_in_hospital, ever_in_ICU)
  
  fn_score_hosp <- function(x, A, days_since_onset, delay_shape, delay_scale) {
    prob_already_observed <- pgamma(days_since_onset,
                                    shape = delay_shape,
                                    scale = delay_scale)
    
    
    A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
  }
  
  F_ICU_lookup <- get_ICU_lookup(age_groups, clinical_parameter_lookup)
  
  F_icu_given_case <- function(x, age_group) {
    F_ICU_lookup[min(round(x, digits = 2) * 10 + 1, 301), age_group]
  }
  
  fn_score_ICU <- function(x, A, days_since_onset, age_group) {
    
    prob_already_observed <- sapply(1:length(days_since_onset), function(i) {
      F_icu_given_case(days_since_onset[i], age_group)
    })
    
    A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
  }
  
  
  cases_in_window <- function(model_data, start, end) {
    model_data %>% filter(date_onset >= start, date_onset <= end)
  }
  
  window_min_sample <- 1000
  
  
  
  library(furrr)
  library(future.callr)
  plan(callr, workers = 9)
  
  hosp_ests <- future_map_dfr(
    age_groups,
    function(i_age_group) {
      require(lubridate, warn.conflicts = F, quietly = TRUE)
      require(dplyr, warn.conflicts = F, quietly = TRUE)
      require(tidyr, warn.conflicts = F, quietly = TRUE)
      require(purrr, warn.conflicts = F, quietly = TRUE)
      
      
      hosp_window_end <- nindss_date
      hosp_window_start <- nindss_date - ddays(7)
      
      
      model_data <- nindss_state %>%
        filter(age_group == i_age_group) %>%
        mutate(days_since_onset = as.numeric(nindss_date - date_onset),
               ever_in_hospital = replace_na(ever_in_hospital, FALSE))
      
      
      hosp_window_cases <- cases_in_window(model_data, hosp_window_start, hosp_window_end)
      
      while(nrow(hosp_window_cases) < window_min_sample &
            hosp_window_start >= simulation_start_date) {
        hosp_window_start <- hosp_window_start - ddays(1)
        hosp_window_cases <- cases_in_window(model_data, hosp_window_start, hosp_window_end)
      }
      
      delay_hosp_shape <- clinical_parameter_lookup[i_age_group, "shape_onset_to_ward"]
      delay_hosp_scale <- clinical_parameter_lookup[i_age_group, "scale_onset_to_ward"]
      n_cases <- nrow(hosp_window_cases)
      
      pr_hosp_adj <- map_dbl(
        1:100, function(i) {
          
          hosp_sampled_cases <- hosp_window_cases %>%
            sample_n(size = n(), replace = TRUE)
          
          n_hosp <- sum(hosp_sampled_cases$ever_in_hospital, na.rm = TRUE)
          
          nothosp_days_since_onset = hosp_sampled_cases %>%
            filter(!ever_in_hospital) %>%
            pull(days_since_onset)
          
          pr_hosp_adj <- tryCatch(pracma::fzero(
            function(x) {
              fn_score_hosp(x, n_hosp, nothosp_days_since_onset, delay_shape = delay_hosp_shape, delay_scale = delay_hosp_scale)
            },
            
            x = c(0 + .Machine$double.eps, 1 - .Machine$double.eps),
            tol = 0.0001
          )$x,
          error = function(c) { return(-1) })
          
          pr_hosp_adj
        }
      )
      
      tibble(
        pr_hosp_adj = pr_hosp_adj,
        age_group = i_age_group
      )
    },
    .options = furrr_options(
      seed = TRUE,

      globals = c("fn_score_hosp", "clinical_parameter_lookup", "cases_in_window", "nindss_state", "nindss_date", "simulation_start_date",
                  "window_min_sample")
    )
  )
  
  ICU_ests <- future_map_dfr(
    age_groups,
    function(i_age_group) {
      require(lubridate, warn.conflicts = F, quietly = TRUE)
      require(dplyr, warn.conflicts = F, quietly = TRUE)
      require(tidyr, warn.conflicts = F, quietly = TRUE)
      require(purrr, warn.conflicts = F, quietly = TRUE)
      
      ICU_window_end <- nindss_date
      ICU_window_start <- nindss_date - ddays(14)
      
      
      model_data <- nindss_state %>%
        filter(age_group == i_age_group, ever_in_hospital) %>%
        drop_na(ever_in_hospital) %>%
        mutate(days_since_onset = as.numeric(nindss_date - date_onset),
               ever_in_ICU = replace_na(ever_in_ICU, FALSE))
      
      
      ICU_window_cases <- cases_in_window(model_data, ICU_window_start, ICU_window_end)
      
      while(nrow(ICU_window_cases) < window_min_sample &
            ICU_window_start >= simulation_start_date) {
        ICU_window_start <- ICU_window_start - ddays(3)
        ICU_window_cases <- cases_in_window(model_data, ICU_window_start, ICU_window_end)
      }
      
      
      
      n_hosp <- nrow(ICU_window_cases)
      
      pr_ICU_adj <- map_dbl(
        1:100, function(i) {
          
          ICU_sampled_cases <- ICU_window_cases %>%
            sample_n(size = n(), replace = TRUE)
          
          n_ICU <- sum(ICU_sampled_cases$ever_in_ICU, na.rm = TRUE)
          
          notICU_days_since_onset = ICU_sampled_cases %>%
            filter(!ever_in_ICU) %>%
            pull(days_since_onset)
          
          pr_ICU_adj <- tryCatch(pracma::fzero(
            function(x) {
              fn_score_ICU(x,
                           n_ICU,
                           notICU_days_since_onset,
                           i_age_group)
            },
            
            x = c(0+.Machine$double.eps,1-.Machine$double.eps),
          )$x,
          error = function(c) { return(-1) })
          
          pr_ICU_adj
        }
      )
      
      tibble(
        pr_ICU_adj = pr_ICU_adj,
        age_group = i_age_group
      )
    },

    .options = furrr_options(
      seed = TRUE,

      globals = c("fn_score_ICU", "F_icu_given_case", "clinical_parameter_lookup", "cases_in_window",
                  "nindss_state", "nindss_date", "simulation_start_date", "window_min_sample", "F_ICU_lookup")
    )
  )
  
  
  all_morbidity_ests <- bind_rows(
    hosp_ests %>% rename(value = pr_hosp_adj) %>% mutate(name = "pr_hosp") %>%
      slice(rep(1:nrow(hosp_ests), each = 9000 / nrow(hosp_ests))),
    ICU_ests %>% rename(value = pr_ICU_adj) %>% mutate(name = "pr_ICU") %>%
      slice(rep(1:nrow(ICU_ests), each = 9000 / nrow(ICU_ests))) # Repeat samples here
  )
  
  if(any(all_morbidity_ests$value < 0)) {
    print("Invalid morbidity estimate produced")
  }
  
  
  morbidity_samples <- all_morbidity_ests %>% 
    group_by(age_group, name) %>%
    mutate(sample = row_number()) %>% 
    pivot_wider()

  morbidity_samples
}



get_ICU_lookup <- function(age_groups, clinical_parameter_lookup) {
  
  tbl_vars <- expand_grid(
    x = seq(0, 30, by = 0.1),
    age_group = age_groups
  )
  
  
  F_icu_given_case_exact <- function(x, 
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
  
  y_vals <- pmap_dbl(tbl_vars, function(x, age_group) {
    
    
    F_icu_given_case_exact(
      x, 
      delay_hosp_shape = clinical_parameter_lookup[age_group, "shape_onset_to_ward"],
      delay_hosp_scale = clinical_parameter_lookup[age_group, "scale_onset_to_ward"],
      
      delay_ICU_shape = clinical_parameter_lookup[age_group,  "shape_ward_to_ICU"],
      delay_ICU_scale = clinical_parameter_lookup[age_group, "scale_ward_to_ICU"]
    )
  })
  
  
  tbl_output <- tbl_vars %>%
    mutate(y = y_vals,
           
           y = if_else(is.nan(y), 0, y))
  
  
  F_ICU_lookup <- tbl_output %>%
    pivot_wider(names_from = age_group,
                values_from = y) %>%
    select(-x) %>%
    as.matrix()
  
}



