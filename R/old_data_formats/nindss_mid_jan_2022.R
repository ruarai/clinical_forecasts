
process_NINDSS_linelist <- function(
  nindss_raw,
  date_simulation_start
) {
  source("R/age_groups.R")
  
  
  
  linelist_data <- bind_rows(
    parse_xl_sheet(nindss_raw, sheet = 1, date_simulation_start)
  )
  
  
  print("Saving...")
  
  linelist_data
}

parse_xl_sheet <- function(nindss_raw, sheet_n, date_simulation_start) {
  
  print(paste0("Reading Excel sheet ", sheet_n, "..."))
  
  nindss_data <- openxlsx::read.xlsx(nindss_raw, sheet = sheet_n)
  
  parse_xl_date <- function(raw_date) {
    suppressWarnings(case_when(
      raw_date == "NULL" ~ NA_Date_,
      is.na(as.numeric(raw_date)) ~ NA_Date_,
      TRUE ~ as.Date(as.numeric(raw_date), origin = "1899-12-30")
    ))
  }
  date_valid <- function(x) !is.na(x)
  
  print("Processing...")
  
  nindss_data %>%
    mutate(TRUE_ONSET_DATE = parse_xl_date(TRUE_ONSET_DATE),
           NOTIFICATION_DATE = parse_xl_date(NOTIFICATION_DATE),
           NOTIFICATION_RECEIVE_DATE = parse_xl_date(NOTIFICATION_RECEIVE_DATE),
           
           CV_ICU = as.numeric(CV_ICU),
           HOSPITALISED = as.numeric(HOSPITALISED),
           AGE_AT_ONSET = as.numeric(AGE_AT_ONSET)) %>%
    
    # Going to lose some cases due to NA AGE_AT_ONSET here,
    # but accept it for now as less likely if HOSPITALISED
    # and we don't care about non-HOSPITALISED
    drop_na(STATE, AGE_AT_ONSET) %>%
    filter(AGE_AT_ONSET >= 0) %>%
    
    
    
    
    mutate(date_onset = case_when(date_valid(TRUE_ONSET_DATE)           ~ TRUE_ONSET_DATE,
                                  date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                  date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE),
           
           date_diagnosis = case_when(date_valid(NOTIFICATION_DATE)         ~ NOTIFICATION_DATE,
                                      date_valid(NOTIFICATION_RECEIVE_DATE) ~ NOTIFICATION_RECEIVE_DATE),
           
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