

get_latest_ensemble_file <- function() {
  require(tidyverse)
  require(lubridate)
  
  
  file_listing_csv <- "data/mediaflux_syncing/diff_ensemble.csv"
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-check",
                           " --mf.config ~/auth_keys/mflux.cfg",
                           " --output ", file_listing_csv,
                           " --direction down",
                           " data/mediaflux_syncing/empty_dir",
                           " /projects/proj-6200_nndss_covid19_data_repository-1128.4.270/forecast-outputs")
  
  
  suppressWarnings(file.remove(file_listing_csv))
  system(mediaflux_call)
  
  
  file_listing <- read_csv(file_listing_csv) %>%
    select(source = SRC_PATH) %>%
    mutate(source = basename(source)) %>%
    mutate(date = str_extract(source, "\\d{4}-\\d{2}-\\d{2}")) %>%
    
    arrange(desc(date))
  
  
  likely_files <- file_listing %>%
    slice(1:20) %>%
    arrange(date) %>%
    pull(source)
  
  print("Recent CSV files:")
  print(likely_files)
  
  
  
  likely_ensemble_file <- file_listing %>%
    filter(str_detect(source, "combined_samples")) %>%
    slice_max(date, n = 1)
  
  print("")
  print(paste0("Using the most likely ensemble CSV '", likely_ensemble_file$source, "'"))
  print(paste0("Which is dated ", likely_ensemble_file$date, 
               " (", today() - ymd(likely_ensemble_file$date), " days ago)"))
  
  likely_ensemble_file$source
}



get_latest_NNDSS_file <- function() {
  require(tidyverse)
  require(lubridate)
  
  
  file_listing_csv <- "data/mediaflux_syncing/diff_NNDSS.csv"
  
  mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-check",
                           " --mf.config ~/auth_keys/mflux.cfg",
                           " --output ", file_listing_csv,
                           " --direction down",
                           " data/mediaflux_syncing/empty_dir",
                           " '/projects/proj-6200_nndss_covid19_data_repository-1128.4.270/Health Uploads'")
  
  
  suppressWarnings(file.remove(file_listing_csv))
  system(mediaflux_call)
  
  
  file_listing <- read_csv(file_listing_csv) %>%
    select(source = SRC_PATH) %>%
    mutate(source = basename(source)) %>%
    mutate(date = str_extract(source, "\\ \\d{1,2}\\D{3}\\d{4}") %>% dmy()) %>%
    
    arrange(desc(date))
  
  
  likely_files <- file_listing %>%
    slice(1:20) %>%
    arrange(date) %>%
    pull(source)
  
  print("Recent files:")
  print(likely_files)
  
  
  
  likely_NNDSS_file <- file_listing %>%
    filter(str_detect(source, "COVID-19 UoM")) %>%
    slice_max(date, n = 1)
  
  print("")
  print(paste0("Using the most likely NNDSS file '", likely_NNDSS_file$source, "'"))
  print(paste0("Which is dated ", likely_NNDSS_file$date, 
               " (", today() - ymd(likely_NNDSS_file$date), " days ago)"))
  
  likely_NNDSS_file$source
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
              mf_file$local_file)
  }
  
  
}


sync_latest_mediaflux_vacc <- function() {
  mf_namespace <- "/projects/proj-6200_nndss_covid19_data_repository-1128.4.270"
  mf_dirs <- str_c(mf_namespace, c("/vaccine_allocation/vaccine_cumulative_medicare/tabular/"))
  
  mf_dirs_local <- str_c("data/mediaflux_syncing/vaccination", c("/tabular"))
  
  
  for(mf_dir in mf_dirs){
    mediaflux_call <- paste0("~/unimelb-mf-clients-0.6.3/bin/unix/unimelb-mf-download ",
                             "--mf.config /usr/local/forecasting/auth_info/mflux.cfg ",
                             "--out data/mediaflux_syncing/vaccination/ ",
                             mf_dir)
    
    system(mediaflux_call)
  }
  
  for(mf_dir_local in mf_dirs_local) {
    local_files <- list.files(mf_dir_local, full.names = TRUE) %>%
      tibble(file = .) %>%
      mutate(date = str_extract(file, "\\d{4}-\\d{2}-\\d{2}") %>% ymd()) %>%
      
      mutate(file_undated = str_remove(file, "_\\d{4}.*(?=\\.csv)") %>% basename()) %>%
      mutate(file_dest = str_c("data/input/", file_undated))
    
    latest_file <- local_files %>%
      slice_max(date, n = 1)
    
    print(paste0("Using file dated ", latest_file$date, " (", latest_file$file, ") as ", latest_file$file_undated))
    
    file.copy(latest_file$file, latest_file$file_dest)
    
  }
}



