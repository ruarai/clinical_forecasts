

get_anzics_data <- function(forecast_date) {
  print(str_c("Downloading ANZICS data from mediaflux for forecast ", forecast_date))
  
  
  mediaflux_call <- 
    paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-download ",
           "--mf.config /home/forecast/auth_keys/mflux_unimelb.cfg ",
           "--out /home/forecast/data_private/ ",
           "/projects/proj-6200_covid19_internal_sit_assessment-1128.4.505/anzics-data ")
  
  system(mediaflux_call)
  
  file_meta <- tibble(
    path = list.files("/home/forecast/data_private/anzics-data",
                      recursive = TRUE,
                      full.names = TRUE)
  ) %>%
    mutate(date = lubridate::dmy(str_extract(path, "\\d{2}[A-Z][a-z]{2}\\d{4}")))
  
  
  all_data <- pmap_dfr(
    file_meta,
    function(path, date) {
      
      excel_raw <- suppressMessages(readxl::read_excel(
        path,
        sheet = 1
      ))
      
      drop_rows <- c(1, 2, 3, 9, 10, 11, 12, 18, 19, 20, 21, 24) - 1
      
      excel_rows <- setdiff(1:24, drop_rows)
      excel_cols <- c(2, 4, 6, 8, 10, 12, 14, 16)
      
      excel_row_names <- c(
        "ICU_nursing_staff_active",
        "HDU_nursing_staff_active",
        "nursing_staff_exposed",
        "nursing_staff_extra_available",
        "ICU_beds_open_staffed_equipped",
        
        "ICU_patients",
        "HDU_patients",
        "ICU_HDU_patients_COVID",
        "ICU_capable_beds_expansion",
        "ICU_capable_beds_total",
        
        "ventilators_occupied",
        "ventilators_occupied_COVID",
        "ventilators_total"
      )
      
      excel_data <- excel_raw[excel_rows, excel_cols] %>%
        `colnames<-`((excel_raw[2, excel_cols])) %>%
        
        mutate(name = excel_row_names) %>%
        
        pivot_longer(cols = -name,
                     names_to = "state") %>%
        
        mutate(val = as.numeric(word(value)),
               date = date) %>%
        select(-value) %>% rename(value = val)
      
      excel_data
    }
  )
}
