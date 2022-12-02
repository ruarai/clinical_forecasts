make_occupancy_timeseries <- function(
  c19data,
  anzics_data = NULL,
  state_modelled = NULL
) {
  
  ts <- c19data
  
  
  if(!is.null(state_modelled)) {
    ts <- ts %>%
      filter(state == state_modelled)
  }
  
  if(state_modelled == "ACT"){
    ts <- ts %>%
      mutate(
        count = if_else(date >= ymd("2022-11-13") & date <= ymd("2022-11-24"), NA_real_, count)
      )
  }
  
  
  
  return(ts)
}