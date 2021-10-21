
process_NNDSS_linelist <- function(minimum_case_date = ymd("2021-06-15")) {
  
  
  
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
    CV_SOURCE_INFECTION = "numeric"
  )
  
  nndss_data <- readxl::read_xlsx("data/input/NNDSS.xlsx", col_types = nndss_col_types)
  
  
  
  linelist_data <- nndss_data %>%
    
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    mutate(date_onset = case_when(!is.na(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  !is.na(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                  !is.na(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE) %>% date(),
           
           status_ICU = case_when(is.na(CV_ICU) ~ 0,
                                  TRUE          ~ CV_ICU),
           
           status_hospital = case_when(CV_ICU == 1 ~ 1,
                                       TRUE        ~ HOSPITALISED)) %>%
    
    select(state = STATE,
           date_onset,
           age = AGE_AT_ONSET,
           status_hospital,
           status_ICU,
           status_DIED = DIED) %>%
    
    filter(date_onset >= minimum_case_date)
  
  
  
  write_rds(linelist_data, "data/processed/clinical_linelist.rds")
  
  
  
}
