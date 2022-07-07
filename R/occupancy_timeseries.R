make_occupancy_timeseries <- function(
  c19data,
  anzics_data = NULL,
  state_modelled = NULL
) {
  
  ts <- c19data %>%
    select(-state) %>%
    rename(state = state_abbrev) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(state, date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(state, date),
                 values_to = "count", names_to = "group") %>%
    
    mutate(source = "c19")
  
  if(!is.null(state_modelled)) {
    ts <- ts %>%
      filter(state == state_modelled)
  }
  
  
  return(ts)
}