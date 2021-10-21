

get_model_parameters <- function() {
  
  age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
  
  vacc_labels <- c("none", "az1", "az2", "pf1", "pf2")
  
  
  # Morbidity parameters
  
  prob_death_ward <- 0.46 * c(0.039, 0.037, 0.035, 0.035, 0.036, 0.039, 0.045, 0.055,
                              0.074, 0.107, 0.157, 0.238, 0.353, 0.502, 0.675, 0.832, 1) %>%
    `names<-`(age_labels)
  
  
  rel_prob_death_ward_VOC <- 0.35
  
  rel_prob_death_ward_vaccine <- 1 - c(0, 0.69, 0.9, 0.71, 0.92) %>%
    `names<-`(vacc_labels)
  
  
  prob_death_ICU <- 0.67 * c(0.282, 0.286, 0.291, 0.299, 0.310, 0.328, 0.353, 0.390, 
                             0.446, 0.520, 0.604, 0.705, 0.806, 0.899, 0.969, 1.0, 0.918) %>%
    `names<-`(age_labels)
  
  
  rel_prob_death_ICU_VOC <- 0.35
  
  rel_prob_death_ICU_vaccine <- 1 - c(0, 0.69, 0.9, 0.71, 0.92) %>%
    `names<-`(vacc_labels)
  
  prob_death_post_ICU <- 0.35 * c(0.091, 0.083, 0.077, 0.074, 0.074, 0.076, 0.08, 0.086,
                                  0.093, 0.102, 0.117, 0.148, 0.211, 0.332, 0.526, 0.753, 1.0) %>%
    `names<-`(age_labels)
  
  
  rel_prob_death_post_ICU_VOC <- 0.35
  
  rel_prob_death_post_ICU_vaccine <- 1 - c(0, 0.69, 0.9, 0.71, 0.92) %>%
    `names<-`(vacc_labels)
  
  morbidity_params <- list(
    prob_death_ward = prob_death_ward,
    rel_prob_death_ward_VOC = rel_prob_death_ward_VOC,
    rel_prob_death_ward_vaccine = rel_prob_death_ward_vaccine,
    
    prob_death_ICU = prob_death_ICU,
    rel_prob_death_ICU_VOC = rel_prob_death_ICU_VOC,
    rel_prob_death_ICU_vaccine = rel_prob_death_ICU_vaccine,
    
    prob_death_post_ICU = prob_death_post_ICU,
    rel_prob_death_post_ICU_VOC = rel_prob_death_post_ICU_VOC,
    rel_prob_death_post_ICU_vaccine = rel_prob_death_post_ICU_vaccine
  )
  
  
  
  ## Delay parameters
  
  
  maximum_delay <- 25
  
  mean_delay_by_age <- c(5.8, 5.8, 5.8, 5.8, 5.8, 5.8, 5.8, 5.8, 5.8,
                         5.8, 6.37, 6.37, 6.37, 6.37, 4.21, 4.21, 4.21) %>%
    `names<-`(age_labels)
  
  shape_delay_by_age <- c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7,
                          1.7, 1.9, 1.9, 1.9, 1.9, 1.3, 1.3, 1.3) %>%
    `names<-`(age_labels)
  
  
  compartment_labels <- c("non-ICU ward discharge", "ward death", "pre-ICU ward discharge",
                          "ICU death", "ICU to ward death", "ICU to ward discharge", "post ICU death",
                          "post ICU discharge")
  
  
  compartment_LoS_mean <- c(2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                            3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                            3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                            3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                            3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                            4.8, 10.7, 1.1, 12.86, 7.73, 6.76, 8.95,  8.64, 
                            4.8, 10.7, 1.1, 12.86, 7.73, 6.76, 8.95,  8.64, 
                            7.4, 9.7,  1.3, 9.55,  5.74, 6.75, 6.64,  8.64, 
                            7.4, 9.7,  1.3, 9.55,  5.74, 6.75, 6.64,  8.64) %>%
    matrix(ncol = 8, byrow = TRUE) %>%
    `colnames<-`(compartment_labels) %>%
    `rownames<-`(age_labels)
  
  
  compartment_LoS_upper <- c(2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                             3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                             3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                             3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                             3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                             5.3, 12.4, 1.4, 19.2, 9.0,  9.6,  10.3,  12.2, 
                             5.3, 12.4, 1.4, 19.2, 9.0,  9.6,  10.3,  12.2, 
                             8.1, 11.2, 1.6, 14.8, 6.9,  10.6, 8.0,   13.5, 
                             8.1, 11.2, 1.6, 14.8, 6.9,  10.6, 8.0,   13.5) %>%
    matrix(ncol = 8, byrow = TRUE) %>%
    `colnames<-`(compartment_labels) %>%
    `rownames<-`(age_labels)
  
  compartment_LoS_shape <- c(2, 2, 1, 2, 2, 2, 2, 2) %>%
    matrix(ncol = 8, nrow = length(age_labels), byrow = TRUE) %>%
    `colnames<-`(compartment_labels) %>%
    `rownames<-`(age_labels)
  
  
  # TODO We need something here about "post_los_maxes"
  
  delay_params <- list(
    maximum_delay = maximum_delay,
    
    mean_delay_by_age = mean_delay_by_age,
    shape_delay_by_age = shape_delay_by_age,
    
    compartment_LoS_mean = compartment_LoS_mean,
    compartment_LoS_upper = compartment_LoS_upper,
    compartment_LoS_shape = compartment_LoS_shape
  )
  
  
  
  list(
    morbidity_params = morbidity_params,
    delay_params = delay_params
  )
}
