



download_files <- function(dbx_files) {
  require(rdrop2)
  drop_auth(rdstoken = "/usr/local/forecasting/auth_info/dbx_token.rds")
  
  
  for(i in 1:nrow(dbx_files)){
    dbx_file <- dbx_files[i,]
    
    file_exists <- drop_exists(dbx_file$remote_file)
    
    if(file_exists) {
      drop_download(dbx_file$remote_file,
                    local_path = dbx_file$local_file,
                    overwrite = TRUE)
    }
    else {
      print(paste0("Could not find ", dbx_file$remote_file))
    }
    
  }
}