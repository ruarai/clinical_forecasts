

breakdown_age_classes <- function(age_classes, width, max_age) {
  classes_between_ages <- function(lower, upper, width) {
    if(is.infinite(upper)) {
      return(tibble(age_class = str_c(lower, "+")))
    }
    
    n_classes <- (upper - lower) %/% width
    
    lowers <- lower + 0:n_classes * width
    uppers <- lowers + width - 1
    
    tibble(age_class = str_c(lowers, uppers, sep = "-"))
  }
  
  
  age_group_table <- tibble(age_class = age_classes) %>%
    mutate(lower_age = as.numeric(str_extract(age_class, "^\\d{1,2}")),
           upper_age = as.numeric(str_extract(age_class, "\\d{1,2}$")),
           
           upper_age = replace_na(upper_age, 100)) %>%
    
    rowwise() %>%
    summarise(result_rows = list(classes_between_ages(lower_age, upper_age, width)))
  
  result_age_groups <- bind_rows(age_group_table$result_rows)
}