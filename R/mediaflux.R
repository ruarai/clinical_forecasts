
get_latest_file_path <- function(mediaflux_dir,
                                 file_pattern,
                                 date_pattern,
                                 str_to_date_fn,
                                 
                                 date_limit = NULL) {
  mediaflux_dir <- paste0("/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/", mediaflux_dir)
  
  require(tidyverse)
  require(lubridate)
  
  file_listing_csv <- "data/mflux/mediaflux_diff.csv"
  empty_dir <- "data/mflux/empty_dir"
  file.create(file_listing_csv)
  
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-check",
                           " --mf.config ~/auth_keys/mflux.cfg",
                           " --output ", file_listing_csv,
                           " --direction down",
                           " ", empty_dir,
                           " '", mediaflux_dir, "'")
  
  
  suppressWarnings(file.remove(file_listing_csv))
  system(mediaflux_call, ignore.stdout = TRUE)
  
  file_listing <- read_csv(file_listing_csv,
                           show_col_types = FALSE) %>%
    select(source = SRC_PATH) %>%
    mutate(source = basename(source)) %>%
    mutate(date = str_extract(source, date_pattern) %>% str_to_date_fn) %>%
    
    arrange(desc(date))
  
  if(!is.null(date_limit)) {
    file_listing <- file_listing %>%
      filter(date <= date_limit)
  }
  
  
  likely_file <- file_listing %>%
    filter(str_detect(source, file_pattern)) %>%
    slice_max(date, n = 1) %>%
    slice(1)
  
  print(paste0("Using the most likely file '", likely_file$source, "'"))
  print(paste0("Which is dated ", likely_file$date, 
               " (", today() - ymd(likely_file$date), " days ago)"))
  
  tibble(file = likely_file$source, date = likely_file$date)
}


get_latest_mflux_files <- function(date_limit) {
  latest_ensemble_file <- get_latest_file_path(
    "forecast-outputs",
    "combined_samples", "\\d{4}-\\d{2}-\\d{2}", ymd,
    
    date_limit - ddays(7)) %>%
    mutate(file = paste0("forecast-outputs/", file),
           type = "ensemble")
  
  latest_nindss_file <- get_latest_file_path(
    "Health Uploads",
    "COVID-19 UoM", "\\ \\d{1,2}\\D{3}\\d{4}", dmy,
    
    date_limit) %>%
    mutate(file = paste0("Health Uploads/", file),
           type = "NNDSS")
  
  latest_local_cases_file <- get_latest_file_path(
    "past_local_cases",
    "local_cases", "\\d{8}", ymd,
    
    date_limit) %>%
    mutate(file = paste0("past_local_cases/", file),
           type = "local_cases")
  
  list(
    ensemble = latest_ensemble_file,
    nindss = latest_nindss_file,
    local_cases = latest_local_cases_file
  )
}


download_mediaflux_files <- function(mf_files) {
  for(i in 1:nrow(mf_files)) {
    mf_file <- mf_files[i,]
    
    mf_dir <- "/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/"
    mf_path_remote <- paste0(mf_dir, mf_file$remote_file)
    
    mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-download ",
                             "--out data/mflux/downloads_raw/ ",
                             "--mf.config '/home/forecast/auth_keys/mflux.cfg' ",
                             "'", mf_path_remote, "'")
    
    system(mediaflux_call)
    
    file.copy(paste0("data/mflux/downloads_raw/", basename(mf_path_remote)),
              mf_file$local_file,
              overwrite = TRUE)
  }
}

download_mflux_file <- function(remote_file, local_file) {
  download_mediaflux_files(
    tibble(remote_file = remote_file, local_file = local_file)
  )
  
  return(local_file)
}