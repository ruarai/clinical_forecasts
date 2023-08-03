
read_occupancy_data <- function(occupancy_path, save_full = FALSE) {
  
  
  national_data <- readxl::read_excel(
    occupancy_path,
    sheet = 2
  ) %>%
    `colnames<-`(c("date", "hospitalised", "ICU", "state")) %>%
    mutate(date = as_date(date),
           ward = hospitalised - ICU) %>%
    
    select(date, state, ward, ICU)  %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group")
  
  old_ts <- read_csv("/home/forecast/source/clinical_forecasting/data/occupancy/compiled/compiled_2022-10-20.csv", show_col_types = FALSE)
  
  
  full_occ <- bind_rows(
    old_ts %>% filter(date < min(national_data$date)),
    national_data
  ) %>%
    select(state, group, date, count) %>%
    
    filter(!(state == "QLD" & date >= ymd("2022-12-24") & date <= ymd("2023-01-02"))) %>%
    
    mutate(
      count = if_else(state == "ACT" & date >= ymd("2022-11-13") & date <= ymd("2022-11-24"), NA_real_, count)
    )
  
  if(save_full) {
    write_csv(full_occ, str_c("/home/forecast/source/clinical_forecasting/data/occupancy/compiled/occupancy_compiled_", today(), ".csv"))
  }
  
  
  full_occ
}

