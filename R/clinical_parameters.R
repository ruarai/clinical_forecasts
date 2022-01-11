


get_clinical_parameters <- function(
  dir = "~/source/los_analysis_competing_risks/results/NSW_omi_mix_2022-01-04/"
) {
  
  
  
  probability_estimates <- read_csv(
    paste0(dir, "/modelled_pathway_probs.csv"),
    show_col_types = FALSE
  )
  
  
  
  codings <- c(
    "ward_to_discharge", "ward_to_ICU", "ward_to_death",
    "ICU_to_postICU", "ICU_to_discharge", "ICU_to_death",
    "postICU_to_death", "postICU_to_discharge"
  )
  
  age_classes <- c("0-39", "40-69", "70+")
  
  split_age_class <- function(age_class) {
    case_when(age_class == "0-69" ~ list(c("0-39", "40-69")),
              TRUE ~ list(age_class))
  }
  
  tbl_split_age_class <- .   %>%
    rowwise() %>%
    mutate(age_class = split_age_class(age_class)) %>%
    unnest(age_class) %>%
    
    ungroup()
  
  age_class_to_10yr <- function(age_class) {
    case_when(
      age_class == "0-39" ~ list(c("0-9", "10-19", "20-29", "30-39")),
      age_class == "40-69" ~ list(c("40-49", "50-59", "60-69")),
      age_class == "70+" ~ list(c("70-79", "80+")),
      age_class == "0-69" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69"))
    )
  }
  
  tbl_split_age_class_10yr <- .   %>%
    rowwise() %>%
    mutate(age_class = age_class_to_10yr(age_class)) %>%
    unnest(age_class) %>%
    
    ungroup()
  
  pr_table <- probability_estimates %>%
    select(age_class, coding, prop_pathway) %>%
    tbl_split_age_class() %>%
    
    complete(coding = codings, age_class = age_classes, fill = list(prop_pathway = 0)) %>%
    
    tbl_split_age_class_10yr() %>%
    
    pivot_wider(names_from = "coding",
                values_from = "prop_pathway") %>%
    
    select(age_group = age_class, ward_to_discharge, ward_to_ICU, ICU_to_discharge, ICU_to_postICU, postICU_to_death)
  
  
  los_estimates <- read_csv(
    paste0(dir, "/los_estimates.csv"),
    show_col_types = FALSE
  )
  
  los_params <- los_estimates  %>%
    select(coding, age_class, rate = rate_est, shape = shape_est) %>%
    
    mutate(scale = 1 / rate) %>%
    select(-rate) %>%
    
    tbl_split_age_class_10yr() %>%
    rename(age_group = age_class,
           compartment = coding)
  
  scale_cols <- los_params %>%
    select(-shape) %>% 
    pivot_wider(names_from = "compartment", values_from = "scale") %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("scale_", .))
  
  
  
  shape_cols <- los_params %>%
    select(-scale) %>% 
    pivot_wider(names_from = "compartment", values_from = "shape") %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("shape_", .))
  
  
  
  
  all_parameters <- pr_table %>%
    ungroup() %>%
    rename_with(.cols = -age_group, .fn = ~ str_c("pr_", .)) %>%
    
    left_join(scale_cols, by = "age_group") %>%
    
    left_join(shape_cols, by = "age_group")
  
  return(all_parameters)
}
