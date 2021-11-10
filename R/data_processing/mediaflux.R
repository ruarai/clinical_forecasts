

get_latest_file_path <- function(mediaflux_dir,
                                 file_pattern,
                                 date_pattern,
                                 str_to_date_fn) {
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
  system(mediaflux_call)
  
  file_listing <- read_csv(file_listing_csv) %>%
    select(source = SRC_PATH) %>%
    mutate(source = basename(source)) %>%
    mutate(date = str_extract(source, date_pattern) %>% str_to_date_fn) %>%
    
    arrange(desc(date))
  
  
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
  print(paste0("Using the most likely ensemble CSV '", likely_file$source, "'"))
  print(paste0("Which is dated ", likely_file$date, 
               " (", today() - ymd(likely_file$date), " days ago)"))
  
  likely_file$source
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
