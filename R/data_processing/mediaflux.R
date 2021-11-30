

get_latest_file_path <- function(mediaflux_dir,
                                 file_pattern,
                                 date_pattern,
                                 str_to_date_fn,
                                 
                                 date_limit = NULL) {
  mediaflux_dir <- paste0("/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/", mediaflux_dir)
  
  require(tidyverse)
  require(lubridate)
  
  
  file_listing_csv <- "data/mediaflux_syncing/diff_mediaflux.csv"
  
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-check",
                           " --mf.config ~/auth_keys/mflux.cfg",
                           " --output ", file_listing_csv,
                           " --direction down",
                           " data/mediaflux_syncing/empty_dir",
                           " '", mediaflux_dir, "'")
  
  
  suppressWarnings(file.remove(file_listing_csv))
  system(mediaflux_call, ignore.stdout = TRUE)
  
  file_listing <- read_csv(file_listing_csv) %>%
    select(source = SRC_PATH) %>%
    mutate(source = basename(source)) %>%
    mutate(date = str_extract(source, date_pattern) %>% str_to_date_fn) %>%
    
    arrange(desc(date))
  
  if(!is.null(date_limit)) {
    file_listing <- file_listing %>%
      filter(date <= date_limit)
  }
  
  
  likely_files <- file_listing %>%
    slice(1:10) %>%
    arrange(date) %>%
    pull(source)
  
  print("Recent files:")
  print(likely_files)
  
  
  
  likely_file <- file_listing %>%
    filter(str_detect(source, file_pattern)) %>%
    slice_max(date, n = 1)
  
  print("")
  print(paste0("Using the most likely file '", likely_file$source, "'"))
  print(paste0("Which is dated ", likely_file$date, 
               " (", today() - ymd(likely_file$date), " days ago)"))
  print("Is this correct?")
  
  readline(prompt="Press [enter] to continue")
  
  tibble(file = likely_file$source, date = likely_file$date)
}




download_mediaflux_files <- function(mf_files) {
  for(i in 1:nrow(mf_files)) {
    mf_file <- mf_files[i,]
    
    mf_dir <- "/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/"
    mf_path_remote <- paste0(mf_dir, mf_file$remote_file)
    
    mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-download ",
                             "--mf.config /usr/local/forecasting/auth_info/mflux.cfg ",
                             "--out data/mediaflux_syncing/download_files/ ",
                             "'", mf_path_remote, "'")
    
    system(mediaflux_call)
    
    file.copy(paste0("data/mediaflux_syncing/download_files/", basename(mf_path_remote)),
              mf_file$local_file,
              overwrite = TRUE)
  }
}

download_latest_mediaflux_files <- function(simulation_options,
                                            
                                            date_limit = NULL) {
  
  latest_ensemble_file <- get_latest_file_path(
    "forecast-outputs",
    "combined_samples", "\\d{4}-\\d{2}-\\d{2}", ymd,
    
    date_limit) %>%
    mutate(file = paste0("forecast-outputs/", file),
           type = "ensemble")
  
  latest_nndss_file <- get_latest_file_path(
    "Health Uploads",
    "COVID-19 UoM", "\\ \\d{1,2}\\D{3}\\d{4}", dmy,
    
    date_limit) %>%
    mutate(file = paste0("Health Uploads/", file),
           type = "NNDSS")
  
  latest_vacc_file <- get_latest_file_path(
    "vaccine_allocation/vaccine_cumulative_medicare/tabular",
    "effective_dose_data", "\\d{4}-\\d{2}-\\d{2}", ymd,
    
    date_limit) %>%
    mutate(file = paste0("vaccine_allocation/vaccine_cumulative_medicare/tabular/", file),
           type = "effective_dose_data")
  
  
  latest_local_cases_file <- get_latest_file_path(
    "past_local_cases",
    "local_cases", "\\d{8}", ymd,
    
    date_limit) %>%
    mutate(file = paste0("past_local_cases/", file),
           type = "local_cases")
  
  mf_files <- tribble(
    ~remote_file, ~local_file,
    latest_ensemble_file$file,        simulation_options$files$ensemble_samples,
    latest_nndss_file$file,           simulation_options$files$NNDSS_raw,
    latest_vacc_file$file,            simulation_options$files$vacc_raw,
    latest_local_cases_file$file,     simulation_options$files$local_cases,
  )
  download_mediaflux_files(mf_files)
  
  bind_rows(latest_ensemble_file,
            latest_nndss_file,
            latest_vacc_file,
            latest_local_cases_file)
}



upload_mflux_sharing <- function() {
  
  
  mediaflux_call <- paste0(
    "~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-upload",
    " --mf.config ~/auth_keys/mflux.cfg",
    " --namespace /projects/proj-6200_nndss_covid19_data_repository-1128.4.270/",
    " /usr/local/forecasting/clinical_forecasts/")
  
  system(mediaflux_call)
  
}


