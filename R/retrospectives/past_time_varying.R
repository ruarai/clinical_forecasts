

get_time_varying_as_known <- function(
    date_forecasting,
    forecast_dates
) {
  read_rds("~/source/clinical_forecasting_NSW/data/time_varying_estimates_2022-10-28.rds") %>%
    
    filter(date >= forecast_dates$simulation_start,
           date < date_forecasting - days(7)) %>%
    
    complete(
      bootstrap,
      age_group,
      date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, "days")
    ) %>%
    
    arrange(date) %>%
    
    group_by(bootstrap, age_group) %>%
    fill(pr_age_given_case, pr_hosp, pr_ICU, .direction = "down") %>%
    
    ungroup()
}


get_time_varying_with_future <- function(
    date_forecasting,
    forecast_dates
) {
  read_rds("~/source/clinical_forecasting_NSW/data/time_varying_estimates_2022-10-28.rds") %>%
    
    filter(date >= forecast_dates$simulation_start,
           date <= forecast_dates$forecast_horizon) %>%
    
    ungroup()
}

