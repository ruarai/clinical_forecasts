


for(i in 1:length(past_forecast_trajs)){
  forecast_name <- names(past_forecast_trajs)[i]
  
  trajs <- fst::read_fst(past_forecast_trajs[i])
  
  wide_trajs <- trajs %>%
    select(state, group, date, sample, count) %>%
    
    pivot_wider(names_from = sample,
                names_prefix = "sim_",
                values_from = count)
  
  
  write_csv(
    wide_trajs,
    paste0("results/retrospectives/export_trajs/", forecast_name, ".csv")
  )
}

a <- read_csv(file = "results/retrospectives/export_trajs/fc_2022-01-05_retro.csv")


a <- fst::read_fst("results/retrospectives/export_trajs/fc_2022-01-05_retro.fst")
