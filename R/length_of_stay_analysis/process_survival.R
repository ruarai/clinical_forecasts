


ward_age_groups <- c("0-39", "40-69", "70+")
ward_age_breaks <- c(0, 40, 70, Inf) - 1

death_age_groups <- c("0-69", "70+")
death_age_breaks <- c(0, 70, Inf) - 1

icu_age_groups <- c("0-39", "40-69", "70+")
icu_age_breaks <- c(0, 40, 70, Inf) - 1


time_diff_to_days <- function(time_diff){ as.numeric(time_diff / ddays(1)) }


code_ward_compartment <- function(is_still_in_hosp, ever_in_icu, patient_died) {
  case_when(
    ever_in_icu ~ "ward_to_ICU",
    is_still_in_hosp  ~ "censored",
    patient_died ~ "ward_to_death",
    TRUE ~ "ward_to_discharge"
  )
}




ward_modelling <- clinical_linelist %>%
  mutate(ward_coding = code_ward_compartment(is_still_in_hosp,
                                             ever_in_icu,
                                             patient_died),
         ward_censor_code = if_else(ward_coding == "censored", 0, 1),
         
         ward_LoS = case_when(
           ward_coding == "ward_to_ICU" ~ time_diff_to_days(dt_first_icu - dt_hosp_admission),
           TRUE ~ time_diff_to_days(dt_hosp_discharge - dt_hosp_admission)
         ),
         
         ward_LoS = if_else(ward_LoS == 0, 0.01, ward_LoS))

ward_modelling$ward_coding %>% table()

ggplot(ward_modelling) +
  geom_histogram(aes(x = age, fill = ward_coding),
                 binwidth = 5) +
  
  geom_vline(aes(xintercept = age),
             data = tibble(age = ward_age_breaks)) +
  
  theme_minimal() +
  
  scale_fill_brewer(type = 'qual',
                     palette = 2) +
  
  theme(legend.position = 'bottom') +
  ggtitle("Ward data w/ age breaks")

make_prob_table <- function(full_data, coding_column, age_breaks, age_groups) {
  with_age <- full_data %>%
    mutate(age_class = cut(age, breaks = age_breaks, labels = age_groups))
  
  with_age %>% 
    
    filter(!!sym(coding_column) != "censored") %>%
    group_by(!!sym(coding_column), age_class) %>%
    summarise(n_down_path = n()) %>% 
    
    group_by(age_class) %>% 
    mutate(n_denom = sum(n_down_path),
           prob = n_down_path / n_denom)
}

get_compartment_data <- function(full_data, compartment_name, age_breaks, age_groups, coding_column) {
    
  prob_table <- make_prob_table(full_data, coding_column, age_breaks, age_groups) %>%
    filter(!!sym(coding_column)  == compartment_name) %>% 
    select(-c(!!sym(coding_column), n_down_path, n_denom))
  
  with_age <- full_data %>%
    mutate(age_class = cut(age, breaks = age_breaks, labels = age_groups))
  
  censored_data <- with_age %>%
    filter(!!sym(coding_column)  == "censored") %>%
    left_join(prob_table) %>%
    
    filter(runif(nrow(.)) <= prob) %>%
    select(-prob)
  
  with_age %>% 
    filter(!!sym(coding_column)  == compartment_name) %>%
    bind_rows(censored_data)
}
  

ward_discharge_data <- get_compartment_data(ward_modelling, "ward_to_discharge",
                                            ward_age_breaks, ward_age_groups,
                                            "ward_coding")


ward_died_data <- get_compartment_data(ward_modelling, "ward_to_death",
                                       death_age_breaks, death_age_groups,
                                       "ward_coding")

ward_ICU_data <- get_compartment_data(ward_modelling, "ward_to_ICU",
                                      icu_age_breaks, icu_age_groups,
                                      "ward_coding")

ward_LoS_discharged <- flexsurvreg(Surv(time = ward_LoS, event = ward_censor_code) ~ 1,
                                   anc = list(shape = ~age_class),
                                   dist = "gamma",
                                   data = ward_discharge_data)

ward_LoS_died <- flexsurvreg(Surv(time = ward_LoS, event = ward_censor_code) ~ 1,
                             anc = list(shape = ~age_class),
                             dist = "gamma",
                             data = ward_died_data)

ward_LoS_ICU <- flexsurvreg(Surv(time = ward_LoS, event = ward_censor_code) ~ 1,
                            anc = list(shape = ~age_class),
                            dist = "gamma",
                            data = ward_ICU_data)

code_ICU_compartment <- function(is_still_in_icu, received_postICU_care, patient_died) {
  case_when(is_still_in_icu ~ "censored",
            
            received_postICU_care ~ "ICU_to_postICU",
            
            !patient_died ~ "ICU_to_discharge",
            
            patient_died ~ "ICU_to_death",
            
            TRUE ~ "unknown")
}

ICU_modelling <- clinical_linelist %>%
  filter(ever_in_icu) %>%
  mutate(ICU_LoS = time_diff_to_days(dt_last_icu - dt_first_icu),
         postICU_LoS = time_diff_to_days(dt_hosp_discharge - dt_last_icu),
         
         received_postICU_care = postICU_LoS > 0.01,
         
         ICU_coding = code_ICU_compartment(is_still_in_icu, received_postICU_care, patient_died),
         
         ICU_censor_code = if_else(ICU_coding == "censored", 0, 1)) %>%
  
  mutate(ICU_LoS = if_else(ICU_LoS == 0, 0.01, ICU_LoS),
         postICU_LoS = if_else(postICU_LoS == 0, 0.01, postICU_LoS))


ggplot(ICU_modelling) +
  geom_histogram(aes(x = age, fill = ICU_coding),
                 binwidth = 5) +
  
  geom_vline(aes(xintercept = age),
             data = tibble(age = icu_age_breaks)) +
  
  theme_minimal() +
  
  scale_fill_brewer(type = 'qual',
                    palette = 1) +
  
  theme(legend.position = 'bottom') +
  ggtitle("ICU data w/ age breaks")

ICU_postICU_data <- get_compartment_data(ICU_modelling, "ICU_to_postICU",
                                         icu_age_breaks, icu_age_groups,
                                         "ICU_coding")

ICU_discharge_data <- get_compartment_data(ICU_modelling, "ICU_to_discharge",
                                           icu_age_breaks, icu_age_groups,
                                           "ICU_coding")

ICU_death_data <-get_compartment_data(ICU_modelling, "ICU_to_death",
                                      death_age_breaks, death_age_groups,
                                      "ICU_coding")

ICU_LoS_postICU <- flexsurvreg(Surv(time = ICU_LoS, event = ICU_censor_code) ~ 1,
                                         anc = list(shape = ~age_class),
                                         dist = "gamma",
                                         data = ICU_postICU_data)

ICU_LoS_discharge <- flexsurvreg(Surv(time = ICU_LoS, event = ICU_censor_code) ~ 1,
                                     anc = list(shape = ~age_class),
                                     dist = "gamma",
                                     data = ICU_discharge_data)

ICU_LoS_death<- flexsurvreg(Surv(time = ICU_LoS, event = ICU_censor_code) ~ 1,
                            anc = list(shape = ~age_class),
                            dist = "gamma",
                            data = ICU_death_data)

code_postICU_compartment <- function(is_still_in_hosp, patient_died) {
  case_when(is_still_in_hosp ~ "censored",
            patient_died ~ "postICU_to_death",
            TRUE ~ "postICU_to_discharge")
}

postICU_modelling <- ICU_modelling %>%
  filter(received_postICU_care) %>%
  
  mutate(postICU_coding = code_postICU_compartment(is_still_in_hosp, patient_died),
         postICU_censor_code = if_else(postICU_coding == "censored", 0, 1))

postICU_death_data <- get_compartment_data(postICU_modelling, "postICU_to_death",
                                           death_age_breaks, death_age_groups,
                                           "postICU_coding")

postICU_discharge_data <- get_compartment_data(postICU_modelling, "postICU_to_discharge",
                                               icu_age_breaks, icu_age_groups,
                                               "postICU_coding")

postICU_LoS_death <- flexsurvreg(Surv(time = postICU_LoS, event = postICU_censor_code) ~ 1,
                                 anc = list(shape = ~age_class),
                                 dist = "gamma",
                                 data = postICU_death_data)

postICU_LoS_discharge <- flexsurvreg(Surv(time = postICU_LoS, event = postICU_censor_code) ~ 1,
                                     anc = list(shape = ~age_class),
                                     dist = "gamma",
                                     data = postICU_discharge_data)


create_parameter_table <- function(flexsurvresult, age_groups) {
  estimate_table <- flexsurvresult$res
  
  rate_row <- rownames(estimate_table) == "rate"
  
  rate <- estimate_table[rate_row, "est"]
  shape_vals <- estimate_table[!rate_row, "est"]
  shape_vals[2:length(shape_vals)] <- shape_vals[1] * exp(shape_vals[2:length(shape_vals)])
  
  names(shape_vals) <- age_groups
  
  mean_vals <- shape_vals / rate
  
  tibble(age_class = age_groups,
         mean = mean_vals,
         rate = rate,
         shape = shape_vals)
}

results_wide_age <- bind_rows(
  create_parameter_table(ward_LoS_discharged, ward_age_groups) %>% mutate(compartment = "ward_to_discharge"),
  create_parameter_table(ward_LoS_died, death_age_groups) %>% mutate(compartment = "ward_to_death"),
  create_parameter_table(ward_LoS_ICU, icu_age_groups) %>% mutate(compartment = "ward_to_ICU"),
  
  create_parameter_table(ICU_LoS_postICU, icu_age_groups) %>% mutate(compartment = "ICU_to_postICU"),
  create_parameter_table(ICU_LoS_discharge, icu_age_groups) %>% mutate(compartment = "ICU_to_discharge"),
  create_parameter_table(ICU_LoS_death, death_age_groups) %>% mutate(compartment = "ICU_to_death"),
  
  create_parameter_table(postICU_LoS_death, death_age_groups) %>% mutate(compartment = "postICU_to_death"),
  create_parameter_table(postICU_LoS_discharge, icu_age_groups) %>% mutate(compartment = "postICU_to_discharge"),
)


source("R/length_of_stay_analysis/fn_age_class.R")
age_class_expansion_table <- tibble(wide_age_class = c(ward_age_groups,
                                                       death_age_groups,
                                                       icu_age_groups) %>% unique()) %>%
  rowwise() %>%
  mutate(narrow_age_class = list(breakdown_age_classes(wide_age_class, 5, 80))) %>% 
  unnest(narrow_age_class) %>%
  rename(narrow_age_class = age_class)

results <- results_wide_age %>%
  right_join(age_class_expansion_table, by = c("age_class" = "wide_age_class")) %>%
  select(age_class = narrow_age_class, compartment, mean, shape, rate)




results %>%
  write_csv(paste0(output_dir, "/parameter_estimates.csv"))







### Probabilities

wide_prob_table <-  bind_rows(
  make_prob_table(ward_modelling,
                  "ward_coding", ward_age_breaks, ward_age_groups) %>%
    filter(ward_coding != "ward_to_death") %>% rename(compartment = ward_coding),
  
  
  make_prob_table(ICU_modelling,
                  "ICU_coding", icu_age_breaks, icu_age_groups) %>%
    filter(ICU_coding == "ICU_to_discharge") %>% rename(compartment = ICU_coding),
  
  make_prob_table(ICU_modelling,
                  "ICU_coding", icu_age_breaks, icu_age_groups) %>%
    filter(ICU_coding == "ICU_to_postICU") %>% rename(compartment = ICU_coding),
  
  make_prob_table(postICU_modelling,
                  "postICU_coding", death_age_breaks, death_age_groups) %>%
    filter(postICU_coding == "postICU_to_death") %>% rename(compartment = postICU_coding),
) %>%
  ungroup()

full_prob_table <- wide_prob_table%>%
  right_join(age_class_expansion_table, by = c("age_class" = "wide_age_class")) %>%
  select(-age_class) %>% rename(age_class = narrow_age_class) %>%
  
  select(age_class, compartment, prob) %>%
  pivot_wider(names_from = compartment, values_from = prob)

full_prob_table  %>%
  write_csv(paste0(output_dir, "/probability_estimates.csv"))






