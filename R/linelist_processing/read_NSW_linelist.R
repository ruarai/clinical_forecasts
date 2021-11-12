process_NSW_linelist <- function(simulation_options) {
  
  source("R/data_processing/fn_age_classes.R")
  
  print(paste0("Using NSW linelist ", simulation_options$files$clinical_linelist_source))
  
  linelist_raw <- readxl::read_xlsx(simulation_options$files$clinical_linelist_source, sheet = 2)
  
  
  NSW_linelist <- read_NSW_linelist(linelist_raw) %>%
    mutate(age_class = assign_age_class(age))
  
  write_rds(NSW_linelist, simulation_options$files$clinical_linelist)
  
}


read_NSW_linelist <- function(linelist_raw) {
  clinical_linelist <- linelist_raw %>%
    select(person_id, age, load_date,
           admit_date_dt, discharge_date_dt, first_icu_date_dt, last_icu_date_dt,
           still_in_hosp, still_in_icu, any_icu_flag, discharge_desc = AP_DISCHARGE_DISPOSITION_DESC,
           subward = SUB_WARD_TYPE)
  
  load_date <- first(clinical_linelist$load_date)
  
  time_diff_to_days <- function(time_diff){
    as.numeric(time_diff / ddays(1))
  }
  
  is_date_too_early <- function(dates) {
    replace_na(dates <= ymd("2020-06-01"), FALSE)
  }
  
  is_date_too_late <- function(dates) {
    replace_na(dates > load_date + days(2), FALSE)
  }
  
  is_indicator_incorrect <- function(discharge, still_in) {
    replace_na(!(still_in == 1 & discharge >= load_date - days(1) |
                   still_in == 0 & discharge <= load_date), FALSE)
  }
  
  is_duration_too_long <- function(start, end) {
    days <- time_diff_to_days(end - start)
    replace_na(days > 120, FALSE)
  }
  
  
  
  
  single_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() == 1)
  
  collapsed_multiple_episodes <- clinical_linelist %>%
    group_by(person_id) %>%
    filter(n() > 1) %>%
    arrange(person_id, admit_date_dt) %>%
    mutate(discharge_to_next_admit = time_diff_to_days(lead(admit_date_dt) - discharge_date_dt),
           discharge_to_next_admit = replace_na(discharge_to_next_admit, 0)) %>%
    
    filter(all(discharge_to_next_admit < 2),
           all(discharge_to_next_admit > -1)) %>%
    
    mutate(admit_date_dt = min(admit_date_dt),
           discharge_date_dt = max(discharge_date_dt),
           
           first_icu_date_dt = min(first_icu_date_dt, na.rm = TRUE),
           last_icu_date_dt = max(last_icu_date_dt, na.rm = TRUE),
           
           any_icu_flag = any(any_icu_flag, na.rm = TRUE),
           still_in_icu = any(still_in_icu, na.rm = TRUE),
           
           still_in_hosp = if_else(any(still_in_hosp == 1), 1, 0),
           
           first_icu_date_dt = if_else(any_icu_flag, first_icu_date_dt, NA_POSIXct_),
           last_icu_date_dt = if_else(any_icu_flag, last_icu_date_dt, NA_POSIXct_)) %>%
    
    slice(1)
  
  
  
  clinical_linelist_collapsed <- bind_rows(
    collapsed_multiple_episodes,
    single_episodes
  ) %>%
    ungroup() %>%
    arrange(person_id)
  
  
  early_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date_dt"), ~ is_date_too_early(.)))
  
  late_dated_entries <- clinical_linelist_collapsed %>%
    filter(if_any(ends_with("_date_dt"), ~ is_date_too_late(.)))
  
  incorrect_indicator_dates <- clinical_linelist_collapsed %>%
    filter(is_indicator_incorrect(last_icu_date_dt, still_in_icu) |
             is_indicator_incorrect(discharge_date_dt, still_in_hosp))
  
  long_duration_entries <-  clinical_linelist_collapsed %>%
    filter(is_duration_too_long(first_icu_date_dt, last_icu_date_dt) |
             is_duration_too_long(admit_date_dt, discharge_date_dt))
  
  
  
  
  print("Rows with dates too early:")
  print(early_dated_entries)
  
  print("Rows with dates too late:")
  print(late_dated_entries)
  
  print("Rows with incorrect indicator dates:")
  print(incorrect_indicator_dates)
  
  print("Rows with excessive duration lengths:")
  print(long_duration_entries)
  
  
  filtered_clinical_linelist <- clinical_linelist_collapsed %>%
    filter(if_all(ends_with("_date_dt"), ~ !is_date_too_early(.))) %>%
    filter(if_all(ends_with("_date_dt"), ~ !is_date_too_late(.))) %>%
    filter(!is_indicator_incorrect(last_icu_date_dt, still_in_icu) &
             !is_indicator_incorrect(discharge_date_dt, still_in_hosp)) %>%
    filter(!is_duration_too_long(first_icu_date_dt, last_icu_date_dt) &
             !is_duration_too_long(admit_date_dt, discharge_date_dt))
  
  
  did_patient_die <- function(discharge_description) {
    death_descriptors <- c("deceased", "death", "died", "dead")
    regex_match <- str_c("(?i)(", str_c(death_descriptors, collapse = "|"), ")")
    
    str_detect(discharge_description, regex_match)
  }
  
  cleaned_clinical_linelist <- filtered_clinical_linelist %>%
    mutate(is_still_in_hosp = still_in_hosp == 1,
           is_still_in_icu = still_in_icu == 1,
           ever_in_icu = any_icu_flag == 1,
           patient_died = did_patient_die(discharge_desc),
           
           dt_onset = admit_date_dt - ddays(6)) %>%
    
    
    select(case_id = person_id,
           age,
           dt_hosp_admission = admit_date_dt,
           dt_hosp_discharge = discharge_date_dt,
           dt_first_icu = first_icu_date_dt,
           dt_last_icu = last_icu_date_dt,
           dt_onset,
           
           is_still_in_hosp,
           is_still_in_icu,
           ever_in_icu,
           patient_died)
  
  
  
  ggplot(cleaned_clinical_linelist) +
    
    geom_histogram(aes(x = age),
                   binwidth = 1) +
    theme_minimal()
  
  
  ggplot(cleaned_clinical_linelist %>% filter(!is_still_in_hosp)) +
    geom_histogram(aes(x = dt_hosp_admission),
                   alpha = 1, fill = 'black',
                   binwidth = ddays(1))  +
    
    geom_histogram(aes(x = dt_hosp_discharge),
                   alpha = 0.5, fill = 'orangered',
                   binwidth = ddays(1)) +
    theme_minimal() +
    ggtitle("Ward admission and discharge dates")
  
  
  ggplot(cleaned_clinical_linelist %>% filter(!is_still_in_icu)) +
    geom_histogram(aes(x = dt_first_icu),
                   alpha = 1, fill = 'black',
                   binwidth = ddays(1))  +
    
    geom_histogram(aes(x = dt_last_icu),
                   alpha = 0.5, fill = 'orangered',
                   binwidth = ddays(1)) +
    theme_minimal() +
    ggtitle("ICU admission and discharge dates")
  
  cleaned_clinical_linelist
}
