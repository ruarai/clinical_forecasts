test <- clinical_linelist %>%
  group_by(person_id) %>%
  filter(n() > 1) %>%
  
  arrange(person_id, admit_date_dt) %>%
  mutate(discharge_to_next_admit = time_diff_to_days(lead(admit_date_dt) - discharge_date_dt),
         discharge_to_next_admit = replace_na(discharge_to_next_admit, 0)) %>%
  
  filter(discharge_to_next_admit >= 2 |
        discharge_to_next_admit < -1) %>%
  
  filter(any(subward == "Infectious Diseases"))


incidental_multiple_episodes <- clinical_linelist %>%
  group_by(person_id) %>%
  filter(n() > 1) %>%
  
  arrange(person_id, admit_date_dt) %>%
  mutate(discharge_to_next_admit = time_diff_to_days(lead(admit_date_dt) - discharge_date_dt),
         discharge_to_next_admit = replace_na(discharge_to_next_admit, 0)) %>%
  
  filter(any(discharge_to_next_admit >= 2) |
         any(discharge_to_next_admit < -1)) %>%
  
  mutate(total_ward_LoS = sum(time_diff_to_days(discharge_date_dt - admit_date_dt)),
         
         admit_date_dt = min(admit_date_dt)) %>%
  
  slice(n()) %>%
  ungroup() %>%
  mutate(discharge_date_dt = admit_date_dt + ddays(total_ward_LoS))

unlikely_admission_delay <- single_episodes %>%
  filter(days_onset_to_adm < -14) %>%
  
  mutate(admit_date_dt = pmin(admit_date_dt,
                              admit_date_dt - ddays(days_onset_to_adm)))



incidental_linelist <- bind_rows(
  incidental_multiple_episodes,
  unlikely_admission_delay
)

days <- seq(ymd("2021-06-01"),
            as_date(max(incidental_linelist$discharge_date_dt, na.rm = TRUE)),
            by ='days')

linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward_incidental = incidental_linelist %>%
           filter(discharge_date_dt >= date | is.na(discharge_date_dt),
                  admit_date_dt <= date) %>%
           nrow(),
         
         count_ward_clinical = filtered_clinical_linelist %>%
           filter(discharge_date_dt >= date | is.na(discharge_date_dt),
                  admit_date_dt <= date) %>%
           nrow(),
         
         
         count_total = count_ward_incidental + count_ward_clinical)


c19data <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == "NSW")

ggplot() +
  geom_line(aes(x = date, y = count_ward_incidental, color = 'secondary'),
            linelist_data_counts) +
  geom_line(aes(x = date, y = count_ward_clinical, color = 'clinical'),
            linelist_data_counts) +
  geom_line(aes(x = date, y = count_total, color = 'total (clinical + secondary)'),
            linelist_data_counts) +
  geom_line(aes(x = date, y = hosp_cum, color = '. public figure'),
            c19data) +
  
  scale_x_date(breaks = scales::breaks_width("month"),
               labels = scales::label_date_short()) +
  
  coord_cartesian(xlim = c(ymd("2021-06-01"), NA)) +
  
  theme_minimal() +
  theme(legend.position = 'bottom')



ggplot(linelist_data_counts %>%
         filter(date >= ymd("2021-09-01"))) +
  
  geom_smooth(aes(x = count_ward_clinical, y = count_ward_incidental),
              method = 'lm') +
  
  geom_point(aes(x = count_ward_clinical, y = count_ward_incidental)) +
  
  
  theme_minimal()





