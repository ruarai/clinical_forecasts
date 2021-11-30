


make_timeseries_from_occupancy <- function(simulation_options) {
  clinical_linelist <- read_rds(simulation_options$files$clinical_linelist)
  
  
  days <- seq(ymd("2021-07-01"),
              as_date(max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE)),
              by ='days')
  
  
  linelist_data_counts <- tibble(date = days) %>%
    rowwise() %>%
    
    mutate(occupied_ward_beds = clinical_linelist %>%
             filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                    dt_hosp_admission <= date,
                    
                    is.na(dt_first_icu) | dt_first_icu >= date | dt_last_icu <= date) %>%
             nrow(),
           
           occupied_ICU_beds = clinical_linelist %>%
             drop_na(dt_first_icu) %>%
             filter(dt_last_icu >= date | is.na(dt_last_icu),
                    dt_first_icu <= date) %>%
             nrow())
  
  
  out_file <- paste0("/usr/local/forecasting/clinical_forecasts/timeseries_occupancy/",
                     "reported_occupancy_",
                     simulation_options$state_modelled,
                     "_",
                     clinical_linelist_date,
                     ".csv")
  
  write_csv(linelist_data_counts,
            out_file)
}