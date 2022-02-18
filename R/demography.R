

get_age_distribution_by_state <- function() {
  
  group_age_class_10yr <- function(age_class) {
    age_mat <- suppressWarnings(age_class %>% str_split("-") %>% do.call(rbind, .) %>% apply(1, FUN = as.numeric))
    
    
    age_tbl <- tibble(
      lower = floor(age_mat[1,] / 10) * 10
    ) %>%
      mutate(upper = lower + 9,
             
             group = str_c(lower, "-", upper),
             group = replace_na(group, "80+")) 
    
    age_tbl$group
  }
  
  read_csv("data/demography/age_distribution_by_state.csv",
           show_col_types = FALSE)  %>%
    mutate(age_group = group_age_class_10yr(age_class)) %>%
    group_by(state, age_group) %>%
    summarise(population = sum(population), .groups = "drop") %>%
    drop_na(state) %>%
    arrange(age_group)
  
}

get_contact_matrix <- function() {
  
  group_age_class_10yr <- function(age_class) {
    age_class_clean <- age_class %>%
      str_remove_all("\\[") %>%
      str_remove_all("\\)")
    
    age_class_clean[age_class_clean == "80,Inf"] <- NA
    
    age_mat <- suppressWarnings(
      age_class_clean %>% 
        str_split(",") %>% 
        do.call(rbind, .) %>% 
        apply(1, FUN = as.numeric)
    )
    
    
    age_tbl <- tibble(
      lower = floor(age_mat[1,] / 10) * 10
    ) %>%
      mutate(upper = lower + 9,
             
             group = str_c(lower, "-", upper),
             group = replace_na(group, "80+")) 
    
    age_tbl$group
  }
  
  
  aus_ngm_unscaled <- readRDS("data/demography/aus_ngm_unscaled.RDS")
  
  conmat::matrix_to_predictions(aus_ngm_unscaled) %>%
    
    mutate(age_group_to = group_age_class_10yr(age_group_to)) %>%
    group_by(age_group_from, age_group_to) %>%
    summarise(contacts = mean(contacts), .groups = 'drop') %>%
    
    mutate(age_group_from = group_age_class_10yr(age_group_from)) %>%
    group_by(age_group_from, age_group_to) %>%
    summarise(contacts = sum(contacts)) %>%
    
    conmat::predictions_to_matrix()
}
