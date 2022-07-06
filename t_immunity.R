
source("R/immunity/adjust_morbidity_trajectories.R")

if(longterm) {
  
  source("R/immunity/age_data.R")
  source("R/immunity/ascertainment_scenario_timeseries.R")
  source("R/immunity/get_neuts_change.R")
  source("R/immunity/vaccination_data.R")
  source("R/immunity/get_params.R")
  source("R/immunity/preprepare_vaccination_tables.R")
  source("R/immunity/immunity_projections.R")
  
  source("R/plotting/plot_immunity_forecast.R")
  
  
  
  reff_env <- new.env()
  suppressPackageStartupMessages(source("R/immunity/functions_new.R", local = reff_env))
  params <- get_params()
  
  
  t_preforecasting_immunity <- list(
    tar_target(
      prediction_dates, 
      seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = "4 days")
    ),
    
    tar_target(vaccination_scenario, 254),
    tar_target(vaccination_data, 
               read_rds("data/vaccination/vaccine_state_20220627.rds") %>% 
                 filter(scenario == vaccination_scenario),
               format = "fst_tbl"),
    tar_target(
      neuts_change, 
      get_neuts_change(prediction_dates)
    )
  )
  
  
  t_state_results_immunity <- list(
    tar_target(
      vaccination_data_state, vaccination_data %>% filter(state == state_modelled), 
      format = "fst_tbl"
    ),
    
    tar_target(
      vaccination_coverage_state, 
      process_vaccination_data(vaccination_data_state, prediction_dates),
      format = "qs"
    ),
    
    tar_target(
      vaccination_tables_state, 
      preprepare_vaccination_tables(vaccination_coverage_state, date_simulation_start),
      format = "qs",
      
      memory = "transient",
      garbage_collection = TRUE
    ),
    
    tar_target(
      vaccination_effects_state,
      get_vaccination_effects(vaccination_coverage_state, neuts_change)
    ),
    
    tar_target(
      population_state,
      get_state_population(vaccination_data_state)
    ),
    
    tar_target(
      immune_predictions_state,
      
      get_immune_predictions(
        prediction_dates,
        nindss_state,
        neuts_change,
        case_trajectories,
        population_state,
        vaccination_tables_state,
        vaccination_effects_state,
        forecast_dates
      ),
      format = "fst_tbl"
      
    ),
    
    tar_target(
      immunity_plots,
      plot_immunity_forecast(
        immune_predictions_state,
        forecast_dates,
        plot_dir,
        state_modelled
      )
    )
  )
} else{
  t_preforecasting_immunity <- list()
  t_state_results_immunity <- list(tar_target(immune_predictions_state, tibble()))
  
}

