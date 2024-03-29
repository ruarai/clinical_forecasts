



library(tidyverse)
library(targets)
library(lubridate)

source("R/read_occupancy_data.R")


occ_data <- read_occupancy_data("data/occupancy/NAT_2023-01-05_Data for Uni of Melbourne.xlsx")


occ_data %>%
  filter(date >= today() - days(30),
         group == "ward") %>%
  
  ggplot() +
  
  geom_point(aes(x = date, y = count)) +
  
  facet_wrap(~state, scales = "free_y")




occ_data %>%
  filter(date >= today() - days(30),
         group == "ICU") %>%
  
  ggplot() +
  
  geom_point(aes(x = date, y = count)) +
  
  geom_point(aes(x = date, y = count),
             colour = "red", alpha = 0.5,
             anzics_data %>% filter(date >= today() - days(30))) +
  
  facet_wrap(~state, scales = "free_y")
