get_time_varying_age_distribution <- function(
  nindss_state, forecast_dates
) {
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  fit_data <- nindss_state %>%
    mutate(t = as.numeric(date_onset - forecast_dates$simulation_start)) %>%
    
    count(t, age_group) %>%
    complete(t = 0:as.numeric(forecast_dates$NNDSS - forecast_dates$simulation_start),
             age_group = age_groups,
             
             fill = list(n = 0)) %>%
    group_by(t) %>%
    mutate(total_n = sum(n)) %>%
    ungroup() %>%
    mutate(age_group = factor(age_group, levels = age_groups),
           dow = wday(forecast_dates$simulation_start + t))
  
  
  require(mgcv)
  
  
  fit <- bam(
    cbind(n, total_n - n) ~ s(t, by = age_group, k = 40) + s(dow, by = age_group, bs = "cc", k = 4),
    
    data = fit_data,
    family = "binomial",
    discrete = TRUE
  )
  
  
  
  pred_data <- expand_grid(
    date_onset = seq(forecast_dates$simulation_start, forecast_dates$NNDSS - ddays(7), "days"),
    age_group = age_groups
  ) %>%
    mutate(t = as.numeric(date_onset - forecast_dates$simulation_start),
           dow = 4) %>%
    
    mutate(pr_age_given_case = predict(fit, newdata = ., type = "response")) %>%
    select(-c(t, dow)) %>%
    
    group_by(date_onset) %>%
    mutate(pr_age_given_case = pr_age_given_case / sum(pr_age_given_case)) %>%
    ungroup()
}
