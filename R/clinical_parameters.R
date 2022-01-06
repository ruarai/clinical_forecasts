


get_clinical_parameters <- function(dir = "data/clinical_parameters/") {
  
  age_class_5yr_to_10yr <- function(age_class_5yr) {
    age_first_piece <- str_extract(age_class_5yr, "\\d{1,3}")
    
    age_group_min <- pmin(as.numeric(age_first_piece) %/% 10, 80 / 10) * 10
    age_group_max <- age_group_min + (10 - 1)
    
    labels <- str_c(age_group_min, "-", age_group_max) %>%
      str_replace("80-89", "80+")
    
    return(labels)
  }
  
  
  probability_estimates <- read_csv(str_c(dir, "/probability_estimates.csv"),
                                    show_col_types = FALSE)
  los_estimates <- read_csv(str_c(dir, "/parameter_estimates.csv"),
                            show_col_types = FALSE)
  
  los_estimates_model <- los_estimates %>%
    mutate(scale = 1 / rate,
           age_group = age_class_5yr_to_10yr(age_class)) %>%
    
    select(age_group, compartment, shape, scale)
  
  # Are all estimates within age groups equal?
  stopifnot(all(los_estimates_model %>% group_by(compartment, age_group) %>% summarise(var = var(shape), .groups = "drop") %>% pull(var) == 0))
  stopifnot(all(los_estimates_model %>% group_by(compartment, age_group) %>% summarise(var = var(scale), .groups = "drop") %>% pull(var) == 0))
  
  los_estimates_model <- los_estimates_model %>%
    group_by(compartment, age_group) %>%
    slice(1)
  
  
  probability_estimates_model <- probability_estimates %>%
    mutate(age_group = age_class_5yr_to_10yr(age_class)) %>%
    select(age_group, ward_to_discharge, ward_to_ICU, ICU_to_discharge, ICU_to_postICU, postICU_to_death)
  
  
  
  stopifnot(probability_estimates_model %>% group_by(age_group) %>%
              summarise(across(everything(), ~ var(.))) %>%
              select(-age_group) %>% as.matrix() %>% as.vector() %>% var() == 0)
  
  
  probability_estimates_model <- probability_estimates_model %>%
    group_by(age_group) %>%
    slice(1)
  
  scale_cols <- los_estimates_model %>%
    select(-shape) %>% 
    pivot_wider(names_from = "compartment", values_from = "scale") %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("scale_", .))
  
  
  
  shape_cols <- los_estimates_model %>%
    select(-scale) %>% 
    pivot_wider(names_from = "compartment", values_from = "shape") %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("shape_", .))
  
  all_parameters <- probability_estimates_model %>%
    ungroup() %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("pr_", .)) %>%
    
    left_join(scale_cols, by = "age_group") %>%
    
    left_join(shape_cols, by = "age_group")
  
  
  return(all_parameters)
}
