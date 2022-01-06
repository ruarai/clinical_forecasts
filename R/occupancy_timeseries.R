make_occupancy_timeseries <- function(
  direct_linelist_state,
  c19data,
  state_modelled
) {
  
  ts <- c19data %>%
    select(-state) %>%
    rename(state = state_abbrev) %>%
    
    mutate(ward_cum = hosp_cum - icu_cum) %>%
    select(state, date, ward = ward_cum, ICU = icu_cum) %>%
    
    pivot_longer(cols = -c(state, date),
                 values_to = "count", names_to = "group") %>%
    
    filter(state == state_modelled) %>%
    
    mutate(source = "c19")
  
  if(state_modelled == "NSW") {
    
    days <- seq(ymd("2021-06-01"),
                as_date(max(direct_linelist_state$dt_hosp_discharge, na.rm = TRUE)),
                by ='days')
    
    ts_linelist <- tibble(date = days) %>%
      rowwise() %>%
      
      mutate(count_ward = direct_linelist_state %>%
               filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                      dt_hosp_admission < date,
                      
                      is.na(dt_first_icu) | dt_first_icu > date | dt_last_icu < date) %>%
               nrow(),
             
             count_ICU = direct_linelist_state %>%
               drop_na(dt_first_icu) %>%
               filter(dt_last_icu >= date | is.na(dt_last_icu),
                      dt_first_icu <= date,) %>%
               nrow()) %>%
      
      pivot_longer(cols = starts_with("count_"),
                   names_prefix = "count_",
                   values_to = "count",
                   names_to = "group") %>%
      
      mutate(source = "direct_ll",
             state = state_modelled)
    
    ts <- bind_rows(ts, ts_linelist)
  }
  
  return(ts)
}