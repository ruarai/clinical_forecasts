
clinical_linelist <- read_rds("results/VIC-test-2021-11-15/input/clinical_linelist.rds")







days <- seq(min(clinical_linelist$dt_hosp_admission, na.rm = TRUE),
            max(clinical_linelist$dt_hosp_discharge, na.rm = TRUE),
            by ='days') %>% as_date()


linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = clinical_linelist %>%
           filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                  dt_hosp_admission <= date) %>%
           nrow(),
         
         count_ICU = clinical_linelist %>%
           drop_na(dt_first_icu) %>%
           filter(dt_last_icu >= date | is.na(dt_last_icu),
                  dt_first_icu <= date) %>%
           nrow(),
         
         count_died = clinical_linelist %>%
           filter(patient_died) %>%
           filter(dt_hosp_discharge <= date) %>%
           nrow()) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")



ggplot() +
  geom_line(aes(x = date, y = count, linetype = 'clinical linelist data'),
            linelist_data_counts) +
  
  facet_grid(rows = vars(group), scales = "free_y") +
  
  geom_line(aes(x = date, y = count, linetype = 'public data'),
            c19data) +
  
  
  coord_cartesian(xlim = c(max(c19data$date) - 120, max(c19data$date)))



raw_vic <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211115_Individual_Stay_Data.csv")



linelist_data_counts_raw <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(
    count_ICU = raw_vic %>%
      drop_na(AdmissionDateICU) %>%
      filter(DischargeDateICU >= date | is.na(DischargeDateICU),
             AdmissionDateICU <= date) %>%
      distinct(RecordID) %>%
      nrow(),
    
    count_ward = raw_vic %>%
      filter(sepdate >= date | is.na(sepdate),
             admdate <= date) %>%
      distinct(RecordID) %>%
      nrow() - count_ICU) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")

linelist_data_counts_raw_bad <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(
    count_ICU = raw_vic %>%
      drop_na(AdmissionDateICU) %>%
      filter(DischargeDateICU >= date,
             AdmissionDateICU <= date) %>%
      distinct(RecordID) %>%
      nrow(),
    
    count_ward = raw_vic %>%
      filter(sepdate >= date,
             admdate <= date) %>%
      distinct(RecordID) %>%
      nrow() - count_ICU) %>%
  
  pivot_longer(cols = starts_with("count_"),
               names_prefix = "count_",
               values_to = "count",
               names_to = "group")



c19data <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == "VIC",
         date >= min(clinical_linelist$dt_hosp_admission)) %>% select(-c(state, state_abbrev)) %>%
  
  mutate(ward_cum = hosp_cum - icu_cum,
         deaths_cum = deaths_cum - min(deaths_cum)) %>%
  select(date, ward = ward_cum, ICU = icu_cum, died = deaths_cum) %>%
  
  pivot_longer(cols = -c(date),
               values_to = "count", names_to = "group")


ts_vic <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211115_Hospital_Occupancy.csv") %>%
  mutate(n_ICU = ICU.Beds.Occupied + ICU.Vent.Beds.Occupied,
         n_ward = Beds.Occupied,
         date = Date) %>%
  select(date, n_ward, n_ICU) %>%
  
  pivot_longer(cols = starts_with("n_"),
               names_prefix = "n_",
               values_to = "count",
               names_to = "group")
  

ggplot() +
  geom_line(aes(x = date, y = count, color = 'clinical linelist data (Individual_Stay_Data.csv)'),
            linelist_data_counts_raw) +
  
  geom_line(aes(x = date, y = count, color = 'timeseries occupancy (Hospital_Occupancy.csv)'),
            ts_vic) +
  
  geom_line(aes(x = date, y = count, color = 'publicly announced figures'),
            c19data %>% filter(group != "died")) +
  
  facet_grid(rows = vars(group), scales = "free_y") +
  
  
  coord_cartesian(xlim = c(max(c19data$date) - 120, max(c19data$date))) +
  theme(legend.position = 'bottom')


raw_vic %>%
  distinct(RecordID, .keep_all = TRUE)

## ICU date present
raw_vic %>%
  drop_na(AdmissionDateICU) %>%
  distinct(RecordID, .keep_all = TRUE)

## Ventilator dates with no ICU dates

raw_vic %>%
  drop_na(AdmissionDateVentilator) %>%
  filter(is.na(AdmissionDateICU)) %>%
  distinct(RecordID, .keep_all = TRUE)


## ICU date present, discharged with no ICU discharge date
raw_vic %>%
  drop_na(AdmissionDateICU) %>%
  filter(Status == "Discharged") %>%
  filter(is.na(DischargeDateICU)) %>%
  distinct(RecordID, .keep_all = TRUE) 
