
get_public_occupancy <- function(date_forecasting) {
  
  
  national_data <- readxl::read_excel(
    "data/occupancy/NAT_2022-04-21_Data for Uni of Melbourne.xlsx",
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
    select(state, group, date, count)
  
  
  p <- full_occ %>%
    filter(date >= today() - days(180), group == "ward") %>%
    ggplot() +
    geom_point(aes(x = date, y = count)) +
    
    facet_wrap(~state, scales = "free_y")
  
  
  write_csv(full_occ, str_c("data/occupancy/compiled/occupancy_compiled_", today(), ".csv"))
  
  full_occ
}

