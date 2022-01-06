

read_ensemble_state <- function(raw_ensemble,
                                state_modelled) {
  
  ensemble_spec <- cols(
    state = col_character(), .model = col_character(),
    date = col_date(), forecast_origin = col_date(),
    .default = col_double()
  )
  
  vroom::vroom(raw_ensemble,
               col_types = ensemble_spec) %>%
    filter(state == state_modelled)
}