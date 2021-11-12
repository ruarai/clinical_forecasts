
require(tidyverse)
require(lubridate)



setwd("/usr/local/forecasting/source/covid19_los_estimations")

data <- read_csv("data/20211101_Individual_Stay_Data.csv")

data$sepdate_cens <- pmin(data$sepdate, data$Onset + 9, na.rm = TRUE)
data$admdate_cens <- pmin(data$admdate, data$Onset + 9, na.rm = TRUE)

days <- tibble(date = seq(min(data$admdate, na.rm=TRUE), max(data$sepdate, na.rm=TRUE), by = 'days'))


vic_data <- days %>%
  pmap_dfr(function(date){
    tibble(date = !!date, count = data %>%
             filter(date >= admdate, date <= sepdate | is.na(sepdate)) %>%
             distinct(RecordID, .keep_all = TRUE) %>%
             nrow())
  })
vic_data_cens <- days %>%
  pmap_dfr(function(date){
    tibble(date = !!date, count = data %>% 
             filter(date >= admdate_cens, date <= sepdate_cens | is.na(sepdate_cens)) %>%
             distinct(RecordID, .keep_all = TRUE) %>%
             nrow())
  })

c19_data <- read_rds("../covid19_aus_clinical_forecasting/data/covid19data.rds")


ggplot() +
  geom_line(aes(x=date,y=count, color = 'occupancy linelist'),
            vic_data) +
  geom_line(aes(x=date,y=count, color = 'occupancy linelist (with censoring applied)'),
            vic_data_cens) +
  
  geom_line(aes(x = date, y = hosp_cum, color = 'daily announced figure'),
            c19_data %>% filter(state == "Victoria")) +
  
  coord_cartesian(xlim = c(today() - 90, today())) +
  
  
  scale_color_brewer(type = 'qual', palette = 6) +
  theme_minimal() +
  theme(legend.position = 'bottom')

