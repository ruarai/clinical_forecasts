

get_c19data <- function(){
  covid19data_url <- "https://github.com/M3IT/COVID-19_Data/raw/master/Data/COVID_AU_state.csv"
  read_csv(covid19data_url,
           show_col_types = FALSE)
}


get_state_linelist <- function(
  state_modelled,
  nindss_state,
  NSW_linelist_path
  ) {
  if(state_modelled == "NSW") {
    source("R/state_data/NSW.R")
    
    return(process_NSW_linelist(NSW_linelist_path))
    
  } else {
    return(nindss_state)
  }
}

get_state_linelist_date <- function(
  state_modelled,
  NSW_linelist_path,
  
  NINDSS_date
) {
  if(state_modelled == "NSW") {
    
    return(lubridate::ymd(str_extract(NSW_linelist_path, "\\d{4}_\\d{2}_\\d{2}")))
    
  } else {
    return(NINDSS_date)
  }
}
