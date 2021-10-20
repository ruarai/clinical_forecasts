
get_latest_ensemble_file <- function() {
  require(tidyverse)
  require(lubridate)
  
  
  file_listing_csv <- "data/mediaflux_syncing/diff.csv"
  
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
