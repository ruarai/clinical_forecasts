
file.copy(
  from = "_targets/",
  to = tar_read(plot_dir),
  
  recursive = TRUE,
  overwrite = TRUE
)