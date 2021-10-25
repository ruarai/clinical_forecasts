get_compartment_model_definition <- function(model_params) {
  
  ## Defining the compartments and compartment covariates (age, vaccine status)
  
  
  # 'silent' compartments do not have a delay and exist solely to
  # allow for capacity constraints to be more easily applied
  compartment_definition <- tribble(
    ~name, ~is_silent,
    "symptomatic_to_ED", FALSE,
    "ED", TRUE,
    "ward", TRUE,
    "ward_to_discharge", FALSE,
    "ward_to_death", FALSE,
    "ward_to_ICU", FALSE,
    "ICU", TRUE,
    "ICU_to_death", FALSE,
    "ICU_to_postICU_discharge", FALSE,
    "ICU_to_postICU_death", FALSE,
    "postICU_to_discharge", FALSE,
    "postICU_to_death", FALSE
  )
  
  compartment_names = compartment_definition$name
  
  silent_compartments <- compartment_definition %>%
    filter(is_silent) %>%
    pull(name)
  
  covariates_age <- model_params$covariates_age
  covariates_vaccine_status = model_params$covariates_vaccine_status
  
  
  covariates_all <- expand_grid(age = covariates_age, vacc = covariates_vaccine_status) %>%
    mutate(cov = str_c(age, "_", vacc))
  
  covariates_all_names <- covariates_all$cov
  
  covariates_lookup <- covariates_all %>%
    pivot_wider(names_from = vacc, values_from = cov) %>%
    select(-age) %>%
    as.matrix() %>%
    `rownames<-`(covariates_age)
  
  
  n_compartments = length(compartment_names)
  n_covariates = length(covariates_all_names)
  
  ## Defining the compartment transitions
  
  
  
  model_params <- get_model_parameters()
  
  compartment_LoS <- expand_grid(
    compartment_name = compartment_names,
    age = covariates_age,
    vaccine_status = covariates_vaccine_status
  ) %>%
    filter(!(compartment_name %in% silent_compartments)) %>%
    rowwise() %>%
    mutate(gamma_mean = model_params$delay_params$compartment_LoS_mean[age, compartment_name],
           gamma_shape = model_params$delay_params$compartment_LoS_shape[age, compartment_name],
           upper_bound = model_params$delay_params$compartment_LoS_upper[age, compartment_name])
  
  
  
  
  
  transitions <- tribble(
    ~from, ~to, ~probability,
    "symptomatic_to_ED", "ED",                         list(0.5), # Defined by a changing estimated param
    "ED",                "ward",                       list(1), # Silent compartment
    
    "ward",              "ward_to_discharge",          NA, # Complement of death and ICU 
    "ward",              "ward_to_death",              list(model_params$morbidity_params$prob_death_ward_lookup),
    "ward",              "ward_to_ICU",                list(0.5), # This is defined by a changing estimated param
    "ward_to_ICU",       "ICU",                        list(1), # Silent compartment
    
    "ICU",               "ICU_to_death",               list(model_params$morbidity_params$prob_death_ICU_lookup), 
    "ICU",               "ICU_to_postICU_death",       list(model_params$morbidity_params$prob_death_post_ICU_lookup),
    "ICU",               "ICU_to_postICU_discharge",   NA, # Complement of postICU death (or both? confirm this)
  )
  
  list(
    compartment_definition = compartment_definition,
    compartment_LoS = compartment_LoS,
    transitions = transitions,
    covariates_lookup = covariates_lookup,
    covariates_listing = covariates_all_names
    )
}
