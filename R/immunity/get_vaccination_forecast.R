get_vaccination_forecast <- function(
  quantium_zip_path
) {
  scenarios <- readr::read_csv(unzip(quantium_zip_path, "dim_scenario.csv"), show_col_types = FALSE)
  
  
  realistic_scenario_code <- scenarios %>%
    filter(str_detect(booster_uptake, "Realistic")) %>%
    pull(scenario)
  
  
  vaccination_data <- readr::read_csv(unzip(quantium_zip_path, "vaccines.csv"), show_col_types = FALSE)
  
  vaccination_data %>%
    filter(scenario == realistic_scenario_code) 
}
