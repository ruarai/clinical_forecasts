

source("R/linelist_processing/read_VIC_linelist.R")


clinical_linelist_dir <- "/usr/local/forecasting/linelist_data/VIC/"
clinical_linelist_date <- ymd("2021-11-28")


clinical_linelist_source <- paste0(clinical_linelist_dir,
                                   format(clinical_linelist_date, "%Y%m%d"),
                                   "_Individual_Stay_Data.csv")

linelist_raw_path <- clinical_linelist_source
load_date <- clinical_linelist_date


VIC_linelist <- read_VIC_linelist(clinical_linelist_source,
                                  clinical_linelist_date)


linelist_raw <- read_csv(linelist_raw_path,
                         col_types = linelist_spec)


vent_no_ICU <- linelist_raw %>%
  drop_na(AdmissionDateVentilator) %>%
  filter(is.na(AdmissionDateICU))

nrow(vent_no_ICU)



raw_incorrect_intervals <- linelist_raw %>%
  filter(RecordID %in% incorrect_intervals$person_id)





occupancy_timeseries <- read_csv("/usr/local/forecasting/linelist_data/VIC/20211128_Hospital_Occupancy.csv") %>%
  mutate(SumICU = ICU.Beds.Occupied + ICU.Vent.Beds.Occupied)

ICU_twitter <- read_csv("/usr/local/forecasting/linelist_data/VIC/ICU_twitter.csv") %>%
  mutate(date = dmy(Date))

ggplot() +
  geom_line(aes(x = date, y = TotalICU, color = 'VicDoH Twitter - ICU occupied - total'),
            ICU_twitter) +
  
  geom_line(aes(x = date, y = ActiveICU, color = 'VicDoH Twitter - ICU occupied - active'),
            ICU_twitter) +
  
  geom_line(aes(x = Date, y = ICU.Beds.Occupied, color = 'Hospital_Occupancy - ICU.Beds.Occupied'),
            occupancy_timeseries) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  xlab("Date") + ylab("Count") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')


days <- seq(min(linelist_raw$DiagnosisDate, na.rm = TRUE),
            max(linelist_raw$DiagnosisDate, na.rm = TRUE), by = 'days')


NNDSS_linelist <- read_rds("results/VIC-prod-2021-11-28/input/linelist_NNDSS.rds") %>%
  filter(state == "VIC")

by_day <- tibble(date = days) %>%
  rowwise() %>%
  mutate(hosp_by_diagnosis = VIC_linelist %>% filter(diagnosis_date == date) %>%
           nrow(),
         hosp_by_diagnosis_NNDSS = NNDSS_linelist %>%
           filter(date_diagnosis == date,
                  status_hospital == 1) %>%
           nrow(),
         
         ICU_by_diagnosis = VIC_linelist %>% drop_na(dt_first_icu) %>%
           filter(diagnosis_date == date) %>%
           nrow(),
         
         ICU_by_diagnosis_NNDSS = NNDSS_linelist %>%
           filter(date_diagnosis == date,
                  status_hospital == 1,
                  status_ICU == 1) %>%
           nrow())

ggplot(by_day) +
  geom_line(aes(x = date, y = hosp_by_diagnosis, color = 'Individual_Stay_Data')) +
  
  geom_line(aes(x = date, y = hosp_by_diagnosis_NNDSS, color = 'NNDSS')) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  coord_cartesian(xlim = c(max(by_day$date) - 120, NA)) +
  
  xlab("Date of Notification/Diagnosis") + ylab("Count") +
  ggtitle("Hospitalised cases by diagnosis date",
          "Comparison of NNDSS and Individual_Stay_Data") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')

ggplot(by_day) +
  geom_line(aes(x = date, y = ICU_by_diagnosis, color = 'Individual_Stay_Data')) +
  
  geom_line(aes(x = date, y = ICU_by_diagnosis_NNDSS, color = 'NNDSS')) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  coord_cartesian(xlim = c(max(by_day$date) - 120, NA)) +
  
  xlab("Date of Notification/Diagnosis") + ylab("Count") +
  ggtitle("ICU cases by diagnosis date",
          "Comparison of NNDSS and Individual_Stay_Data") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')






linelist_data_counts <- tibble(date = days) %>%
  rowwise() %>%
  
  mutate(count_ward = VIC_linelist %>%
           filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                  dt_hosp_admission <= date,
                  
                  is.na(dt_first_icu) | dt_first_icu >= date | dt_last_icu <= date) %>%
           nrow(),
         
         count_ICU = VIC_linelist %>%
           drop_na(dt_first_icu) %>%
           filter(dt_last_icu >= date | is.na(dt_last_icu),
                  dt_first_icu <= date) %>%
           nrow(),
         
         count_died = VIC_linelist %>%
           filter(patient_died) %>%
           filter(dt_hosp_discharge <= date) %>%
           nrow())

ggplot() +
  geom_line(aes(x = date, y = count_ward, color = 'Individual_Stay_Data - ward beds occupied'),
            linelist_data_counts) +
  
  geom_line(aes(x = Date, y = Beds.Occupied, color = 'Hospital_Occupancy - Beds.Occupied'),
            occupancy_timeseries)  +
  
  ggokabeito::scale_color_okabe_ito() +
  
  coord_cartesian(xlim = c(max(by_day$date) - 60, NA)) +
  
  xlab("Date") + ylab("Count") +
  ggtitle("Ward occupancy comparison") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')




ggplot() +
  geom_line(aes(x = date, y = count_ICU, color = 'Individual_Stay_Data - ICU beds occupied'),
            linelist_data_counts) +
  
  geom_line(aes(x = Date, y = ICU.Beds.Occupied, color = 'Hospital_Occupancy - ICU.Beds.Occupied'),
            occupancy_timeseries)  +
  
  ggokabeito::scale_color_okabe_ito() +
  
  coord_cartesian(xlim = c(max(by_day$date) - 60, NA)) +
  
  xlab("Date") + ylab("Count") +
  ggtitle("ICU occupancy comparison") +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')



ggplot() +
  geom_line(aes(x = date, y = ActiveICU, color = 'VicDoH Twitter - ICU occupied - active'),
            ICU_twitter) +
  
  geom_line(aes(x = date, y = ClearedICU, color = 'VicDoH Twitter - ICU occupied - cleared'),
            ICU_twitter) 




ggplot(incorrect_intervals %>%
         arrange(pmin(admit_date, first_icu_date, last_icu_date, discharge_date, na.rm = TRUE)) %>%
         mutate(person_num = row_number()) %>%
         slice_max(person_num, n = 50)) +
  
  geom_linerange(aes(y = person_num,
                     xmin = pmin(admit_date, first_icu_date, last_icu_date, discharge_date, na.rm = TRUE),
                     xmax = pmax(admit_date, first_icu_date, last_icu_date, discharge_date, na.rm = TRUE))) +
  
  geom_point(aes(y = person_num, x = admit_date, color = 'Hospital admit')) +
  
  geom_point(aes(y = person_num, x = first_icu_date, color = 'ICU admit')) +
  
  geom_point(aes(y = person_num, x = last_icu_date, color = 'ICU discharge')) +
  
  geom_point(aes(y = person_num, x = discharge_date, color = 'Hospital discharge')) +
  
  ggokabeito::scale_color_okabe_ito() +
  
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')
  



incorrect_intervals %>%
  mutate(type = case_when(last_icu_date == admit_date ~ "discharge ICU = hospital admit",
                          first_icu_date == discharge_date ~ "admit ICU = hospital discharge",
                          TRUE ~ "other")) %>% pull(type) %>% table()


  