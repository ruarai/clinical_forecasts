


for(i in 1:length(past_forecast_trajs)){
  forecast_name <- names(past_forecast_trajs)[i]
  
  trajs <- fst::read_fst(past_forecast_trajs[i])
  
  label <- if_else(str_detect(forecast_name, "ABC_null"), "null", "ABC")
  forecast_date <- max(trajs$date) - ddays(20)
  
  wide_trajs <- trajs %>%
    select(state, group, date, sample, count) %>%
    
    pivot_wider(names_from = sample,
                names_prefix = "sim_",
                values_from = count)
  
  
  fst::write_fst(
    wide_trajs,
    paste0("results/retrospectives/export_trajs/", str_c("forecast_retro_", label, "_", forecast_date), ".fst")
  )
}

a <- read_csv(file = "results/retrospectives/export_trajs/fc_2022-01-05_retro.csv")


a <- fst::read_fst("results/retrospectives/export_trajs/fc_2022-01-05_retro.fst")
