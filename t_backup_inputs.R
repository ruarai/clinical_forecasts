
t_backup_inputs <- list(

  tar_target(
    backup_nindss,
    {
      file_path <- paste0(archive_dir, "/nindss.fst")
      if(file.exists(file_path))
        return(file_path)

      fst::write_fst(nindss, path = file_path, compress = 100)
      return(file_path)
    },
    
    cue = tar_cue_skip(is_retro),
    format = "file"
  ),

  tar_target(
    backup_local_cases,
    {
      file_path <- paste0(archive_dir, "/local_cases.csv")
      if(file.exists(file_path))
        return(file_path)

      file.copy(tar_read(raw_local_cases), file_path, overwrite = TRUE)
      return(file_path)
    },

    cue = tar_cue_skip(is_retro),
    format = "file"
  ),

  tar_target(
    backup_ensemble,
    {
      file_path <- paste0(archive_dir, "/ensemble.parquet")
      if(file.exists(file_path))
        return(file_path)

      file.copy(tar_read(raw_ensemble), file_path, overwrite = TRUE)
      return(file_path)
    },
    
    cue = tar_cue_skip(is_retro),
    format = "file"
  )
)