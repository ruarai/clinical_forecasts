process_NSW_linelist <- function(simulation_options) {
  
  source("../covid19_los_estimations/R/read_NSW_linelist.R")
  source("R/data_processing/fn_age_classes.R")
  
  print(paste0("Using NSW linelist ", simulation_options$files$clinical_linelist_source))
  
  linelist_raw <- readxl::read_xlsx(simulation_options$files$clinical_linelist_source, sheet = 2)
  
  
  NSW_linelist <- read_NSW_linelist(linelist_raw) %>%
    mutate(age_class = assign_age_class(age))
  
  write_rds(NSW_linelist, simulation_options$files$clinical_linelist)
  
}