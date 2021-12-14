

linelist_raw <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211210_Individual_Stay_Data.csv")


load_date <- ymd("2021-12-10")

guess_age <- function(age_band) {
  age_mat <- age_band %>%
    str_split_fixed("-", n = 2) %>%
    apply(1, as.numeric)
  
  lower_age <- age_mat[1,]
  upper_age <- age_mat[2,]
  
  runif(length(age_band), lower_age, upper_age) %>% 
    floor()
}



single_episodes <- linelist_raw %>%
  group_by(RecordID) %>%
  filter(n() == 1)


print("Single episode types:")
table(single_episodes$TYPE)


single_episodes_flattened <- single_episodes %>%
  mutate(date_admit = admdate,
         date_discharge = sepdate,
         
         date_first_icu = if_else(TYPE %in% c("Vent", "ICU"), date_admit, NA_Date_),
         date_last_icu = if_else(TYPE %in% c("Vent", "ICU"), date_discharge, NA_Date_)) %>%
  
  select(-c(TYPE, admdate, sepdate))


multiple_episodes <- linelist_raw %>%
  arrange(RecordID, admdate) %>%
  
  group_by(RecordID) %>%
  filter(n() > 1) %>%
  
  mutate(TYPE = case_when(TYPE == "Vent" ~ "ICU",
                          TRUE ~ TYPE))



multiple_episodes_flattened <- multiple_episodes %>%
  mutate(date_admit = min(admdate),
         date_discharge = max(sepdate),
         
         date_first_icu = min(admdate[TYPE == "ICU"]),
         date_last_icu = max(sepdate[TYPE == "ICU"])) %>%
  
  select(-c(TYPE, admdate, sepdate)) %>%
  
  slice(n())



clinical_linelist_collapsed <- bind_rows(
  single_episodes_flattened,
  multiple_episodes_flattened
) %>%
  ungroup()





c19data <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == "VIC",
         date >= ymd("2021-07-01")) %>% select(-c(state, state_abbrev)) %>%
  
  mutate(ward_cum = hosp_cum - icu_cum) %>%
  select(date, ward = ward_cum, ICU = icu_cum) %>%
  
  pivot_longer(cols = -c(date),
               values_to = "count", names_to = "group")

ICU_twitter <- read_csv("/usr/local/forecasting/linelist_data/VIC/ICU_twitter.csv") %>%
  mutate(date = dmy(Date))


days <- seq(ymd("2021-07-01"),
            max(clinical_linelist_collapsed$date_discharge, na.rm = TRUE),
            by ='days')


linelist_data_counts_censored <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist_collapsed %>%
           filter(date_discharge >= date | is.na(date_discharge),
                  date_admit < date,
                  DiagnosisDate + ddays(14) > date,
                  
                  is.na(date_first_icu) | date_first_icu>date | date_last_icu<date) %>%
           nrow(),
         
         count_ICU = clinical_linelist_collapsed %>%
           drop_na(date_first_icu) %>%
           filter(date_last_icu >= date | is.na(date_last_icu),
                  date_first_icu <= date,
                  DiagnosisDate + ddays(21) >= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")


linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist_collapsed %>%
           filter(date_discharge >= date | is.na(date_discharge),
                  date_admit < date,
                  
                  is.na(date_first_icu) | date_first_icu>date | date_last_icu<date) %>%
           nrow(),
         
         count_ICU = clinical_linelist_collapsed %>%
           drop_na(date_first_icu) %>%
           filter(date_last_icu >= date | is.na(date_last_icu),
                  date_first_icu <= date,) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")


ts_vic <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211210_Hospital_Occupancy.csv") %>%
  mutate(n_ICU = ICU.Beds.Occupied,
         n_ward = Beds.Occupied - n_ICU,
         date = Date) %>%
  select(date, n_ward, n_ICU) %>%
  
  pivot_longer(cols = starts_with("n_"),
               names_prefix = "n_",
               values_to = "count",
               names_to = "group")



ggplot() +
  
  #geom_line(aes(x = date, y = count, color = 'VicDoH linelist (with 14 day censor)'),
  #          linelist_data_counts_censored %>%
  #            filter(group == "ward")) +
  
  geom_line(aes(x = date, y = count, color = 'VicDoH linelist (raw)'),
            linelist_data_counts %>%
              filter(group == "ward")) +
  
  geom_line(aes(x = date, y = count, color = 'VicDoH timeseries'),
            ts_vic %>%
              filter(group == "ward")) +
  
  #geom_line(aes(x = date, y = count, color = 'Public (VicDoH Twitter)'),
  #          c19data %>%
  #            filter(group == "ward")) +
  
  coord_cartesian(xlim = c(ymd("2021-Oct-01"), NA)) +
  
  ggokabeito::scale_color_okabe_ito(name= "Data source",
                                    order = c(1,2,3,5)) +
  ylab("Occupancy (count)") + xlab("Date") +
  
  ggtitle("Victorian ward occupancy",
          "Comparison of sources") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')

ggsave("vic_data_sources_ward.png",
       bg = 'white',
       width = 8, height = 6)


ggplot() +
  
  geom_line(aes(x = date, y = count, color = 'VicDoH linelist (raw)'),
            linelist_data_counts %>%
              filter(group == "ICU")) +
  
  geom_line(aes(x = date, y = count, color = 'VicDoH linelist (with 21 day censor)'),
            linelist_data_counts_censored %>%
              filter(group == "ICU")) +
  
  geom_line(aes(x = date, y = count, color = 'VicDoH timeseries'),
            ts_vic %>%
              filter(group == "ICU")) +
  
  geom_line(aes(x = date, y = TotalICU, color = 'Total (VicDoH Twitter)'),
            ICU_twitter) +
  
  geom_line(aes(x = date, y = ActiveICU, color = 'Active (VicDoH Twitter)'),
            ICU_twitter) +
  
  coord_cartesian(xlim = c(ymd("2021-Oct-01"), NA)) +
  
  ggokabeito::scale_color_okabe_ito(name= "Data source",
                                    order = c(1,2,3,5,6)) +
  ylab("Occupancy (count)") + xlab("Date") +
  
  ggtitle("Victorian ICU occupancy",
          "Comparison of sources") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')







