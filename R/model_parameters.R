

get_model_parameters <- function(source = "NSW") {
  
  
  covariates_age <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                      "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
  
  covariates_vaccine_status <- c("none", "az1", "az2", "pf1", "pf2")
  
  if(source == "imperial"){
    return(read_rds("data/parameters/imperial_updated.rds"))
  } else if(source == "NSW") {
    
    model_params_base <- read_rds("data/parameters/imperial_updated.rds")
    
    
    LoS_params <- read_csv("../covid19_los_estimations/output/NSW/2021-11-08/parameter_estimates.csv")
    
    
    # THIS IS ALL VERY FRAGILE, needs improving!
    model_params_base$delay_params$compartment_LoS_mean <- LoS_params %>%
      select(age_class, compartment, value = mean) %>%
      pivot_wider(names_from = compartment, values_from = value) %>%
      slice(1:17) %>%
      select(-age_class) %>%
      as.matrix() %>%
      `rownames<-`(covariates_age)
    
    model_params_base$delay_params$compartment_LoS_shape <- LoS_params %>%
      select(age_class, compartment, value = shape) %>%
      pivot_wider(names_from = compartment, values_from = value) %>%
      slice(1:17) %>%
      select(-age_class) %>%
      as.matrix() %>%
      `rownames<-`(covariates_age)
    
    
    prob_estimates <- read_csv("../covid19_los_estimations/output/NSW/2021-11-08/probability_estimates.csv")
    prob_table_matrix <- prob_estimates %>%
      slice(1:17) %>%
      select(-age_class) %>%
      as.matrix() %>%
      `rownames<-`(covariates_age)
    
    model_params_base$morbidity_params$prob_table <- prob_table_matrix
    
    return(model_params_base)
    
    
    
  }
  
  
}


