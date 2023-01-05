
read_occupancy_data <- function(occupancy_path) {
  
  
  national_data <- readxl::read_excel(
    occupancy_path,
    sheet = 2
  ) %>%
    `colnames<-`(c("date", "hospitalised", "ICU", "state")) %>%
    mutate(date = as_date(date),
           ward = hospitalised - ICU) %>%
    
    select(date, state, ward, ICU)  %>%
    pivot_longer(c(ward, ICU), values_to = "count", names_to = "group")
  
  old_ts <- read_csv("data/occupancy/compiled/compiled_2022-10-20.csv")
  
  
  full_occ <- bind_rows(
    old_ts %>% filter(date < min(national_data$date)),
    national_data
  ) %>%
    select(state, group, date, count) %>%
    
    filter(!(state == "QLD" & date >= ymd("2022-12-24") & date <= ymd("2023-01-02")))
  
  
  write_csv(full_occ, str_c("data/occupancy/compiled/occupancy_compiled_", today(), ".csv"))
  
  full_occ
}

