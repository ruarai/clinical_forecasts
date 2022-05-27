make_occupancy_timeseries <- function(
  c19data,
  anzics_data,
  state_modelled
) {
  
  use_anzics <- FALSE
  
  
  ts <- c19data %>%
    select(-state) %>%
    rename(state = state_abbrev) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(state, date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(state, date),
                 values_to = "count", names_to = "group") %>%
    
    filter(state == state_modelled) %>%
    
    mutate(source = "c19")
  
  if(use_anzics) {
    ts <- ts %>%
      filter(group != "ICU") %>%
      
      bind_rows(
        anzics_data %>% 
          filter(name == "ICU_HDU_patients_COVID") %>% 
          select(state, date, count = value) %>%
          filter(state == state_modelled) %>%
          mutate(group = "ICU", source = "anzics")
      )
  }
  
  
  
  return(ts)
}