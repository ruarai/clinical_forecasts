
process_NNDSS_linelist <- function(simulation_options) {
  source("R/data_processing/fn_age_classes.R")
  
  # Taken from the Curtin model
  nndss_col_types <- c(
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
  
  nndss_data <- readxl::read_xlsx(simulation_options$files$NNDSS_raw, 
                                  col_types = nndss_col_types)
  
  
  
  linelist_data <- nndss_data %>%
    
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    mutate(date_onset = case_when(!is.na(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  !is.na(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                  !is.na(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% lubridate::date(),
           
           status_ICU = case_when(is.na(CV_ICU) ~ 0,
                                  TRUE          ~ CV_ICU),
           
           status_hospital = case_when(CV_ICU == 1 ~ 1,
                                       TRUE        ~ HOSPITALISED),
           
           age_class = assign_age_class(AGE_AT_ONSET)) %>%
    
    filter(date_onset >= simulation_options$dates$simulation_start) %>%
    
    select(state = STATE,
           date_onset,
           age = AGE_AT_ONSET,
           age_class,
           status_hospital,
           status_ICU,
           status_DIED = DIED)
  
  
  
  write_rds(linelist_data,
            simulation_options$files$NNDSS_linelist)
}
