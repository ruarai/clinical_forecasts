
process_NINDSS_linelist <- function(
  raw_nindss,
  date_simulation_start
) {
  source("R/age_groups.R")
  
  
  
  nindss_data <- read_csv(raw_nindss)
  
  
  print("Reading CSV data")
  
  parse_csv_date <- function(raw_date) {
    suppressWarnings(case_when(
      raw_date == "NULL" ~ NA_Date_,
      TRUE ~ dmy(raw_date)
    ))
  }
  date_valid <- function(x) !is.na(x)
  
  print("Processing...")
  
  nindss_data %>%
    mutate(TRUE_ONSET_DATE = parse_csv_date(TRUE_ONSET_DATE),
           NOTIFICATION_DATE = parse_csv_date(NOTIFICATION_DATE),
           NOTIFICATION_RECEIVE_DATE = parse_csv_date(NOTIFICATION_RECEIVE_DATE),
           
           CV_ICU = as.numeric(CV_ICU),
           HOSPITALISED = as.numeric(HOSPITALISED),
           AGE_AT_ONSET = as.numeric(AGE_AT_ONSET)) %>%
    
    # Going to lose some cases due to NA AGE_AT_ONSET here,
    # but accept it for now as less likely if HOSPITALISED
    # and we don't care about non-HOSPITALISED
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    
    
    
    mutate(date_onset = case_when(date_valid(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE - 1,
                                  date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE - 2),
           
           date_diagnosis = case_when(date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                      date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE - 1),
           
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
}