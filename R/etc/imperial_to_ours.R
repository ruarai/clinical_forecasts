

covariates_age <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
## Delay parameters
compartment_labels <- c("symptomatic_to_ED", "ward_to_discharge", "ward_to_death",
                        "ward_to_ICU", "ICU_to_death", "ICU_to_postICU_death", 
                        "ICU_to_postICU_discharge", "postICU_to_death", "postICU_to_discharge")


compartment_LoS_mean <- c(5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  2,   11.3, 0.9, 9.5,   5.44, 5.21, 6.30,  4.08, 
                          5.8,  3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                          6.37, 3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                          6.37, 3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                          6.37, 3.3, 11.1, 1.1, 14.82, 8.91, 6.62, 10.31, 8.47, 
                          6.37, 4.8, 10.7, 1.1, 12.86, 7.73, 6.76, 8.95,  8.64, 
                          4.21, 4.8, 10.7, 1.1, 12.86, 7.73, 6.76, 8.95,  8.64, 
                          4.21, 7.4, 9.7,  1.3, 9.55,  5.74, 6.75, 6.64,  8.64, 
                          4.21, 7.4, 9.7,  1.3, 9.55,  5.74, 6.75, 6.64,  8.64) %>%
  matrix(ncol = 9, byrow = TRUE) %>%
  `colnames<-`(compartment_labels) %>%
  `rownames<-`(covariates_age)


compartment_LoS_upper <- c(25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  2.2, 15.9, 1.2, 22.9, 10.7, 5.7,  12.3,  7.3, 
                           25,  3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                           25,  3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                           25,  3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                           25,  3.6, 13.1, 1.4, 21.6, 10.1, 8.9,  11.6,  11.4, 
                           25,  5.3, 12.4, 1.4, 19.2, 9.0,  9.6,  10.3,  12.2, 
                           25,  5.3, 12.4, 1.4, 19.2, 9.0,  9.6,  10.3,  12.2, 
                           25,  8.1, 11.2, 1.6, 14.8, 6.9,  10.6, 8.0,   13.5, 
                           25,  8.1, 11.2, 1.6, 14.8, 6.9,  10.6, 8.0,   13.5) %>%
  matrix(ncol = 9, byrow = TRUE) %>%
  `colnames<-`(compartment_labels) %>%
  `rownames<-`(covariates_age)

compartment_LoS_shape <- c(0, 2, 2, 1, 2, 2, 2, 2, 2) %>%
  matrix(ncol = 9, nrow = length(covariates_age), byrow = TRUE) %>%
  `colnames<-`(compartment_labels) %>%
  `rownames<-`(covariates_age)

compartment_LoS_shape[, 1] <- c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7,
                                1.7, 1.9, 1.9, 1.9, 1.9, 1.3, 1.3, 1.3)



prob_death_post_ICU <- 0.35 * c(0.091, 0.083, 0.077, 0.074, 0.074, 0.076, 0.08, 0.086,
                                0.093, 0.102, 0.117, 0.148, 0.211, 0.332, 0.526, 0.753, 1.0) %>%
  `names<-`(covariates_age)



compartments_combine <- c("ICU_to_postICU_death", "ICU_to_postICU_discharge")

new_means <- matrix(nrow = 17) %>%
  `rownames<-`(covariates_age) %>%
  `colnames<-`("ICU_to_postICU")

for(i_age in covariates_age) {
  
  pr_death_post_ICU <- prob_death_post_ICU[i_age]
  
  samples_death <- rgamma(round(100000 * pr_death_post_ICU),
                          shape = compartment_LoS_shape[i_age, "ICU_to_postICU_death"],
                          rate = compartment_LoS_mean[i_age, "ICU_to_postICU_death"] / 
                            compartment_LoS_shape[i_age, "ICU_to_postICU_death"])
  
  samples_discharge <- rgamma(round(100000 * (1 - pr_death_post_ICU)),
                          shape = compartment_LoS_shape[i_age, "ICU_to_postICU_discharge"],
                          rate = compartment_LoS_mean[i_age, "ICU_to_postICU_discharge"] / 
                            compartment_LoS_shape[i_age, "ICU_to_postICU_discharge"])
  
  
  
  
  samples <- c(samples_death, samples_discharge)
  
  result <- fitdistrplus::fitdist(samples, 'gamma', fix.arg = list(shape = 2))
  
  new_means[i_age,] <- round(result$estimate * 2, 2)
}

keep_compartments <- setdiff(compartment_labels, compartments_combine)

compartment_LoS_mean_upd <- cbind(compartment_LoS_mean[, keep_compartments[1:5]],
                              new_means,
                              compartment_LoS_mean[, keep_compartments[6:7]])

new_labels <- colnames(compartment_LoS_mean_upd)

compartment_LoS_shape_upd <- compartment_LoS_shape[, -7] %>%
  `colnames<-`(new_labels)

compartment_LoS_upper_upd <- compartment_LoS_upper[, -7] %>%
  `colnames<-`(new_labels)

all_model_params <- get_model_parameters()

all_model_params$delay_params$compartment_LoS_mean <- compartment_LoS_mean_upd
all_model_params$delay_params$compartment_LoS_shape <- compartment_LoS_shape_upd
all_model_params$delay_params$compartment_LoS_upper <- compartment_LoS_upper_upd

write_rds(all_model_params, "data/parameters/imperial_updated.rds")
