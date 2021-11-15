process_VIC_linelist <- function(simulation_options) {
  
  
  print(paste0("Using VIC linelist ", simulation_options$files$clinical_linelist_source))
  
  VIC_linelist <- read_VIC_linelist(simulation_options$files$clinical_linelist_source)
  
  write_rds(VIC_linelist, simulation_options$files$clinical_linelist)
  
}

read_VIC_linelist <- function(linelist_raw_path) {
  require(tidyverse)
  require(lubridate)
  source("R/data_processing/fn_age_classes.R")
  
  
  linelist_spec <- cols(
    RecordID = col_character(),
    admdate = col_date(format = ""),
    sepdate = col_date(format = ""),
    LGA = col_character(),
    #LOS = col_double(),
    #TYPE = col_character(),
    Status = col_character(),
    Symptom_Onset = col_date(format = ""),
    DiagnosisDate = col_date(format = ""),
    AdmissionDateICU = col_date(format = ""),
    DischargeDateICU = col_date(format = ""),
    AdmissionDateVentilator = col_date(format = ""),
    DischargeDateVentilator = col_logical(),
    DateOfDeath = col_date(format = ""),
    Vaccine_Status = col_logical(),
    AgeGroup_5yr = col_character()
    #AgeGroup = col_character()
    #Age = col_double(),
    #HaveYouHadACovidVaccine = col_logical()
  )
  #wrong, but whatev
  load_date <- simulation_options$dates$NNDSS
  
  file_path <- linelist_raw_path
  
  
  guess_age <- function(age_band) {
    age_mat <- age_band %>%
      str_split_fixed("-", n = 2) %>%
      apply(1, as.numeric)
    
    lower_age <- age_mat[1,]
    upper_age <- age_mat[2,]
    
    runif(length(age_band), lower_age, upper_age) %>% 
      floor()
  }
  
  clinical_linelist <- read_csv(file_path,
                                col_types = linelist_spec) %>%
    
    select(person_id = RecordID,
           admit_date = admdate,
           discharge_date = sepdate,
           status = Status,
           onset_date = Symptom_Onset,
           confirmation_date = DiagnosisDate,
           
           first_icu_date = AdmissionDateICU,
           last_icu_date = DischargeDateICU,
           
           death_date = DateOfDeath,
           age_class_vic = AgeGroup_5yr,
           #age = Age,
           vaccinated = Vaccine_Status
           ) %>%
    
    drop_na(age_class_vic) %>%
    
    mutate(age = guess_age(age_class_vic)) %>% select(-age_class_vic)
  
  
  single_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() == 1)
  
  collapsed_multiple_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() > 1) %>%
    arrange(person_id, admit_date) %>%
    mutate(discharge_to_next_admit = as.numeric(lead(admit_date) - discharge_date),
           discharge_to_next_admit = replace_na(discharge_to_next_admit, 0),
           
           discharge_to_nextICU = as.numeric(lead(first_icu_date) - discharge_date),
           discharge_to_nextICU = replace_na(discharge_to_nextICU, 0)) %>%
    
    filter(all(discharge_to_next_admit <= 2 & discharge_to_nextICU <= 2)) %>%
    
    mutate(admit_date = min(admit_date, na.rm = TRUE)) %>%
    
    slice(n())
  
  
  clinical_linelist_collapsed <- bind_rows(
    collapsed_multiple_episodes,
    single_episodes
  ) %>%
    ungroup() %>%
    arrange(person_id) %>%
    
    # Make discharge date NA for current ICU cases
    mutate(discharge_date = case_when(!is.na(first_icu_date) & status == "Current" ~ NA_Date_,
                                      TRUE ~ discharge_date))
  
  
  is_date_too_early <- function(dates) {
    replace_na(dates <= ymd("2020-06-01"), FALSE)
  }
  
  is_date_too_late <- function(dates) {
    replace_na(dates > load_date + days(2), FALSE)
  }
  
  is_indicator_incorrect <- function(discharge_date, status) {
    replace_na(!(status == "Current" & discharge_date >= load_date - days(1) |
                   status != "Current" & discharge_date <= load_date), FALSE)
  }
  
  is_duration_too_long <- function(start, end) {
    days <- end - start
    replace_na(days > 120, FALSE)
  }
  
  
  
  
  early_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date"), ~ is_date_too_early(.)))
  
  late_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date"), ~ is_date_too_late(.)))
  
  incorrect_indicator_dates <- clinical_linelist_collapsed %>%
    filter(is_indicator_incorrect(discharge_date, status))
  
  long_duration_entries <-  clinical_linelist_collapsed %>%
    filter(is_duration_too_long(admit_date, discharge_date) |
             is_duration_too_long(first_icu_date, last_icu_date))
  
  
  print("Rows with dates too early:")
  print(early_dated_entries)
  
  print("Rows with dates too late:")
  print(late_dated_entries)
  
  print("Rows with incorrect indicator dates:")
  print(incorrect_indicator_dates)
  
  print("Rows with excessive duration lengths:")
  print(long_duration_entries)
  
  
  filtered_clinical_linelist <- clinical_linelist_collapsed %>%
    filter(if_all(ends_with("_date"), ~ !is_date_too_early(.))) %>%
    filter(if_all(ends_with("_date"), ~ !is_date_too_late(.))) %>%
    filter(!is_indicator_incorrect(discharge_date, status)) %>%
    filter(!is_duration_too_long(admit_date, discharge_date) &
             !is_duration_too_long(first_icu_date, last_icu_date))
  
  
  
  cleaned_clinical_linelist <- filtered_clinical_linelist %>%
    mutate(is_still_in_hosp = status == "Current",
           ever_in_icu = !is.na(first_icu_date),
           is_still_in_icu = ever_in_icu & is.na(last_icu_date),
           patient_died = status == "Deceased",
           age_class = assign_age_class(age),
           
           admit_date_dt = as_datetime(admit_date),
           discharge_date_dt = as_datetime(discharge_date),
           
           first_icu_date_dt = as_datetime(first_icu_date),
           last_icu_date_dt = as_datetime(last_icu_date),
           dt_onset = as_datetime(onset_date)) %>%
    
    drop_na(age) %>%
    
    
    select(case_id = person_id,
           age,
           age_class,
           dt_hosp_admission = admit_date_dt,
           dt_hosp_discharge = discharge_date_dt,
           dt_first_icu = first_icu_date_dt,
           dt_last_icu = last_icu_date_dt,
           dt_onset,
           
           is_still_in_hosp,
           is_still_in_icu,
           ever_in_icu,
           patient_died,
           
           vaccinated
          )
  
  
  cleaned_clinical_linelist
}
