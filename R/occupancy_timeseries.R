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
  
  
  
  return(ts)
}