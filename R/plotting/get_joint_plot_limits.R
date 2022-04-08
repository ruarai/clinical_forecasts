

get_joint_plot_limits <- function(
  all_state_quants,
  known_occupancy_ts,
  capacity_limits_tbl,
  
  forecast_dates
) {
  
  all_state_quants %>%
    
    filter(date > forecast_dates$forecast_start - ddays(10)) %>%
    filter(group %in% c("ward", "ICU")) %>%
    group_by(state, group)  %>%
    
    summarise(peak_forecast = max(upper), .groups = "drop") %>%
    
    left_join(
      known_occupancy_ts %>%
        
        filter(date > forecast_dates$forecast_start - ddays(10)) %>%
        group_by(state, group)  %>%
        
        summarise(peak_observed = max(count), .groups = "drop")
    ) %>%
    
    left_join(
      capacity_limits_tbl
    ) %>%
    
    mutate(
      peak_near_capacity = peak_forecast > capacity * 0.9,
      
      y_lim = if_else(
        peak_near_capacity,
        pmax(peak_forecast, capacity),
        pmax(peak_forecast, peak_observed)
      ) + 10
    ) %>%
    select(state, group, y_lim)
}



