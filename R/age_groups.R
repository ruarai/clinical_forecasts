

assign_10yr_age_group <- function(age) {
  age_bottom <- (age %/% 10) * 10
  age_top <- age_bottom + (10 - 1)
  
  case_when(
    age < 0 ~ "invalid",
    age >= 80 ~ "80+",
    TRUE ~ str_c(age_bottom, "-", age_top)
  )
}
