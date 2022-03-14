
t_backup_inputs <- list(
  
  tar_target(
    backup_nindss,
    {
      file_path <- paste0(backup_dir, "/nindss.fst")
      fst::write_fst(nindss, path = file_path, compress = 100)
      return(file_path)
    },
    
    format = "file"
  ),
  
  tar_target(
    backup_local_cases,
    {
      file_path <- paste0(backup_dir, "/local_cases.csv")
      file.copy(tar_read(raw_local_cases), file_path, overwrite = TRUE)
      return(file_path)
    },
    
    format = "file"
  ),
  
  tar_target(
    backup_ensemble,
    {
      file_path <- paste0(backup_dir, "/ensemble.csv")
      file.copy(tar_read(raw_ensemble), file_path, overwrite = TRUE)
      return(file_path)
    },
    
    format = "file"
  )
)