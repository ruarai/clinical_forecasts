
read_linelist <- function(simulation_options) {
  if(simulation_options$state_modelled == "NSW") {
    source("R/linelist_processing/read_NSW_linelist.R")
    
    process_NSW_linelist(simulation_options)
  } else if(simulation_options$state_modelled == "VIC") {
    source("R/linelist_processing/read_VIC_linelist.R")
    
    process_VIC_linelist(simulation_options)
  }
}