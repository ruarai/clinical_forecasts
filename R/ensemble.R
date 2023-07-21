

read_ensemble_state <- function(raw_ensemble,
                                state_modelled,
                                models_included) {
  
  ensemble_spec <- cols(
    state = col_character(), .model = col_character(),
    date = col_date(), forecast_origin = col_date(),
    .default = col_double()
  )
  
  vroom::vroom(raw_ensemble,
               col_types = ensemble_spec) %>%
    filter(state == state_modelled,
           .model %in% models_included) %>%
    
    mutate(
      across(num_range("sim", 1001:2000), ~ if_else(.model == "moss_varasc_unsmoothed" | .model == "moss_varasc_unsmoothed", NA_real_, .))
    ) %>%
    
    mutate(
      across(num_range("sim", 1001:2000), ~ if_else(str_starts(.model, "dst"), NA_real_, .))
    )
}


read_ensemble_all_states <- function(raw_ensemble, models_included) {
  
  ensemble_spec <- cols(
    state = col_character(), .model = col_character(),
    date = col_date(), forecast_origin = col_date(),
    .default = col_double()
  )
  
  vroom::vroom(raw_ensemble,
               col_types = ensemble_spec) %>%
    filter(.model %in% models_included) %>%
    
    mutate(
      across(num_range("sim", 1001:2000), ~ if_else(.model == "moss_varasc_unsmoothed" | .model == "moss_varasc_unsmoothed", NA_real_, .))
    ) %>%
  
    mutate(
      across(num_range("sim", 1001:2000), ~ if_else(str_starts(.model, "dst"), NA_real_, .))
    )
}
