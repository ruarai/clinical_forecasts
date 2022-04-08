
mediaflux_project_shared <- "/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/"
mediaflux_project_unimelb <- "/projects/proj-6200_covid19_internal_sit_assessment-1128.4.505/"


get_latest_file_path <- function(mediaflux_project,
                                 mediaflux_dir,
                                 file_pattern,
                                 date_pattern,
                                 str_to_date_fn,
                                 
                                 date_limit = NULL,
                                 config_file = "~/auth_keys/mflux.cfg") {
  mediaflux_dir <- paste0(mediaflux_project, mediaflux_dir)
  
  require(tidyverse)
  require(lubridate)
  
  file_listing_csv <- "data/mflux/mediaflux_diff.csv"
  empty_dir <- "data/mflux/empty_dir"
  file.create(file_listing_csv)
  
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-check",
                           " --mf.config ", config_file,
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
  
  tibble(file = likely_file$source,
         date = likely_file$date,
         project = mediaflux_project,
         config_file = config_file)
}


get_latest_mflux_files <- function(date_limit) {
  latest_ensemble_file <- get_latest_file_path(
    mediaflux_project_shared,
    "forecast-outputs",
    "combined_samples", "\\d{4}-\\d{2}-\\d{2}", ymd,
    
    date_limit - ddays(7)) %>%
    mutate(file = paste0("forecast-outputs/", file),
           type = "ensemble")
  
  latest_nindss_file <- get_latest_file_path(
    mediaflux_project_shared,
    "Health Uploads",
    "COVID-19 UoM", "\\ \\d{1,2}\\D{3}\\d{4}", dmy,
    
    date_limit) %>%
    mutate(file = paste0("Health Uploads/", file),
           type = "NNDSS")
  
  latest_local_cases_file <- get_latest_file_path(
    mediaflux_project_unimelb,
    "local_cases_input",
    "local_cases_input", "\\d{4}-\\d{2}-\\d{2}", ymd,
    
    config_file = "~/auth_keys/mflux_unimelb.cfg",
    date_limit) %>%
    mutate(file = paste0("local_cases_input/", file),
           type = "local_cases")
  
  list(
    ensemble = latest_ensemble_file,
    nindss = latest_nindss_file,
    local_cases = latest_local_cases_file
  )
}


download_mediaflux_file <- function(mf_files) {
  mf_file <- mf_files[1,]
  
  mf_dir <- mf_file$project
  mf_path_remote <- paste0(mf_dir, mf_file$remote_file)
  
  config_file <- str_replace(mf_file$config_file, "~", "/home/forecast")
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-download ",
                           "--out data/mflux/downloads_raw/ ",
                           "--mf.config '", config_file ,"' ",
                           "'", mf_path_remote, "'")
  
  system(mediaflux_call)
  
  file.copy(paste0("data/mflux/downloads_raw/", basename(mf_path_remote)),
            mf_file$local_file,
            overwrite = TRUE)
  
  return(mf_file$local_file)
}


upload_mediaflux_trajectories <- function() {
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-upload ",
                           "--mf.config '", "/home/forecast/auth_keys/mflux_unimelb.cfg", "' ",  
                           "--namespace /projects/proj-6200_covid19_internal_sit_assessment-1128.4.505/clinical_forecasts ",
                           "/home/forecast/source/clinical_forecasting/results/trajectories/")
  
  system(mediaflux_call)
  
  
  
}