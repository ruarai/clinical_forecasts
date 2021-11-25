

get_model_parameters <- function(parameters_source) {
  
  
  covariates_age <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                      "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
  
  covariates_vaccine_status <- c("none", "az1", "az2", "pf1", "pf2")

  model_params_base <- read_rds("data/parameters/imperial_updated.rds")
  
  verify_age_order <- function(tbl) {
    ages <- tbl$age_class %>%
      str_extract("^\\d{1,3}") %>%
      as.numeric()
    
    stopifnot(all(ages == ages[order(ages)]))
    
    return(tbl)
  }
  
  
  
  LoS_params <- read_csv(paste0(parameters_source, "/parameter_estimates.csv"),
                         show_col_types = FALSE)
  
  
  params_mean_mat <- LoS_params %>%
    select(age_class, compartment, value = mean) %>%
    pivot_wider(names_from = compartment, values_from = value) %>%
    verify_age_order() %>%
    slice(1:17) %>%
    select(-age_class) %>%
    as.matrix() %>%
    `rownames<-`(covariates_age)
  
  params_shape_mat <- LoS_params %>%
    select(age_class, compartment, value = shape) %>%
    pivot_wider(names_from = compartment, values_from = value) %>%
    verify_age_order() %>%
    slice(1:17) %>%
    select(-age_class) %>%
    as.matrix() %>%
    `rownames<-`(covariates_age)
  
  model_params_base$delay_params$compartment_LoS_mean <- params_mean_mat
  model_params_base$delay_params$compartment_LoS_shape <- params_shape_mat
  
  
  prob_estimates <- read_csv(paste0(parameters_source, "/probability_estimates.csv"),
                         show_col_types = FALSE)
  
  prob_table_matrix <- prob_estimates %>%
    slice(1:17) %>%
    select(-age_class) %>%
    as.matrix() %>%
    `rownames<-`(covariates_age)
  
  model_params_base$morbidity_params$prob_table <- prob_table_matrix
  
  return(model_params_base)
  
  
  
}


