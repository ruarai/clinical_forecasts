
process_NINDSS_linelist <- function(nindss_raw,
                                   date_simulation_start) {
  source("R/age_groups.R")
  
  
  # Taken from the Curtin model
  nindss_col_types <- c(
    STATE = "c",
    POSTCODE = "c",
    CONFIRMATION_STATUS = "c",
    TRUE_ONSET_DATE = "c",
    SPECIMEN_DATE = "c",
    NOTIFICATION_DATE = "c",
    NOTIFICATION_RECEIVE_DATE = "c",
    AGE_AT_ONSET = "numeric",
    SEX = "numeric",
    DIED = "numeric",
    PLACE_OF_ACQUISITION = "c",
    HOSPITALISED = "numeric",
    CV_ICU = "numeric",
    CV_VENTILATED = "numeric",
    OUTBREAK_REF = "c",
    CASE_FOUND_BY = "numeric",
    CV_SYMPTOMS = "c",
    CV_OTHER_SYMPTOMS = "c",
    CV_COMORBIDITIES = "c",
    CV_OTHER_COMORBIDITIES = "c",
    CV_GESTATION = "numeric",
    CV_EXPOSURE_SETTING = "numeric",
    CV_SOURCE_INFECTION = "numeric",
    
    CV_SYMPTOMS_REPORTED = "c",
    CV_QUARANTINE_STATUS = "c",
    CV_DATE_ENTERED_QUARANTINE = "date",
    
    .default = "c"
  )
  
  print("Reading CSV...")
  
  nindss_data <- read_csv(
    nindss_raw,
    col_types = nindss_col_types
  )
  
  print("Processing...")
  
  nindss_date_parse <- function(raw_date) {
    case_when(
      raw_date == "NULL" ~ lubridate::date(NA),
      TRUE ~ lubridate::dmy(raw_date)
    )
  }
  
  date_valid <- function(raw_date) {
    !(is.na(raw_date) | raw_date == "NULL")
  }
  
  
  linelist_data <- nindss_data %>%
    
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    mutate(date_onset = case_when(date_valid(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                  date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% nindss_date_parse(),
           
           date_diagnosis = case_when(date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                      date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% nindss_date_parse(),
           
           status_ICU = case_when(is.na(CV_ICU) ~ 0,
                                  TRUE          ~ CV_ICU),
           
           status_hospital = case_when(CV_ICU == 1 ~ 1,
                                       TRUE        ~ HOSPITALISED),
           
           age_group = assign_10yr_age_group(AGE_AT_ONSET),
           
           ever_in_hospital = status_hospital == 1,
           ever_in_ICU = status_ICU == 1) %>%
    
    filter(date_onset >= date_simulation_start) %>%
    
    select(state = STATE,
           date_onset,
           date_diagnosis,
           age = AGE_AT_ONSET,
           age_group,
           status_hospital,
           status_ICU,
           status_DIED = DIED,
           ever_in_hospital,
           ever_in_ICU)
  
  print("Saving...")
  
  linelist_data
}
