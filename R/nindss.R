
process_NINDSS_linelist <- function(nindss_raw,
                                   date_simulation_start) {
  source("R/age_groups.R")
  
  
  # Taken from the Curtin model
  nindss_col_types <- c(
    STATE = "text",
    POSTCODE = "numeric",
    CONFIRMATION_STATUS = "text",
    TRUE_ONSET_DATE = "date",
    SPECIMEN_DATE = "date",
    NOTIFICATION_DATE = "date",
    NOTIFICATION_RECEIVE_DATE = "date",
    Diagnosis_Date = "date",
    AGE_AT_ONSET = "numeric",
    SEX = "numeric",
    DIED = "numeric",
    PLACE_OF_ACQUISITION = "text",
    HOSPITALISED = "numeric",
    CV_ICU = "numeric",
    CV_VENTILATED = "numeric",
    OUTBREAK_REF = "text",
    CASE_FOUND_BY = "numeric",
    CV_SYMPTOMS = "text",
    CV_OTHER_SYMPTOMS = "text",
    CV_COMORBIDITIES = "text",
    CV_OTHER_COMORBIDITIES = "text",
    CV_GESTATION = "numeric",
    CV_EXPOSURE_SETTING = "numeric",
    CV_SOURCE_INFECTION = "numeric",
    
    CV_SYMPTOMS_REPORTED = "text",
    CV_QUARANTINE_STATUS = "text",
    CV_DATE_ENTERED_QUARANTINE = "date"
  )
  
  print("Reading Excel...")
  
  nindss_data <- readxl::read_xlsx(nindss_raw,
                                  
                                  col_types = nindss_col_types,
                                  range = cellranger::cell_cols(c("A","AA")))
  
  print("Processing...")
  
  
  linelist_data <- nindss_data %>%
    
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    mutate(date_onset = case_when(!is.na(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  !is.na(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                  !is.na(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% lubridate::date(),
           
           date_diagnosis = case_when(!is.na(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                      !is.na(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% lubridate::date(),
           
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
