

read_ensemble_state <- function(raw_ensemble,
                                state_modelled,
                                models_included) {
  
  if (tools::file_ext(raw_ensemble) == "parquet") {
    ensemble_par_data <- arrow::read_parquet(raw_ensemble)
    
    
    ensemble_wide <- ensemble_par_data %>% 
      filter(state == state_modelled) %>%
      rowwise() %>% 
      mutate(id = list(1:length(sample))) %>% 
      ungroup() %>%
      unnest(c(sample, id)) %>% 
      pivot_wider(values_from = sample, names_from = id, names_prefix = "sim") %>%
      select(-.model)
    
    return(ensemble_wide)
  } else {
    
    ensemble_spec <- cols(
      state = col_character(), .model = col_character(),
      date = col_date(), forecast_origin = col_date(),
      .default = col_double()
    )
    
    ensemble_wide <- vroom::vroom(raw_ensemble,
                 col_types = ensemble_spec) %>%
      filter(state == state_modelled)
    
    return(ensemble_wide)
  }
}


read_ensemble_all_states <- function(raw_ensemble, models_included) {
  
  if (tools::file_ext(raw_ensemble) == "parquet") {
    ensemble_par_data <- arrow::read_parquet(raw_ensemble)
    
    
    ensemble_wide <- ensemble_par_data %>% 
      rowwise() %>% 
      mutate(id = list(1:length(sample))) %>% 
      ungroup() %>%
      unnest(c(sample, id)) %>% 
      pivot_wider(values_from = sample, names_from = id, names_prefix = "sim") %>%
      select(-.model)
    
    return(ensemble_wide)
  } else {
    
    ensemble_spec <- cols(
      state = col_character(), .model = col_character(),
      date = col_date(), forecast_origin = col_date(),
      .default = col_double()
    )
    
    ensemble_wide <- vroom::vroom(raw_ensemble,
                                  col_types = ensemble_spec)
    
    return(ensemble_wide)
  }
}