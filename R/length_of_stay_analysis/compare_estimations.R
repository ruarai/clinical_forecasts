



compartment_labels <- c("ward_to_discharge", "ward_to_death",
                        "ward_to_ICU", "ICU_to_death", "ICU_to_postICU_death", 
                        "ICU_to_postICU_discharge", "postICU_to_death", "postICU_to_discharge")

covariates_age <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
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
  `rownames<-`(covariates_age)


compartment_LoS_shape <- c(2, 2, 1, 2, 2, 2, 2, 2) %>%
  matrix(ncol = 8, nrow = length(covariates_age), byrow = TRUE) %>%
  `colnames<-`(compartment_labels) %>%
  `rownames<-`(covariates_age)



draw_gamma_dist <- function(shape, rate) {
  x_vals = seq(0, 60, by = 0.01)
  y_vals = dgamma(x_vals, shape = shape, rate = rate)
  tibble(x = x_vals, y = y_vals)
}

compartment_expansion <- tribble(
  ~from, ~to,
  "censored_ward", "ward_to_discharge",
  "censored_ward", "ward_to_death",
  "censored_ward", "ward_to_ICU",
  
  "censored_ICU", "ICU_to_postICU_discharge",
  "censored_ICU", "ICU_to_postICU_death",
  "censored_ICU", "ICU_to_death",
  "censored_ICU", "ICU_to_discharge",
  
  
  "censored_postICU", "postICU_to_death",
  "censored_postICU", "postICU_to_discharge",
)

compartments_of_interest <- c(compartment_labels, "ICU_to_discharge", "ICU_to_postICU")
# compartments_of_interest <- c("ICU_to_postICU_discharge", "ICU_to_postICU_death", "ICU_to_death",
#                               "postICU_to_death", "postICU_to_discharge")

ages_of_interest <- c("20-24", "50-54", "80-84")

imperial_means <- compartment_LoS_mean %>% 
  as_tibble(rownames = "age_class") %>%
  pivot_longer(cols = -age_class, names_to = "compartment", values_to = "mean")

imperial_shapes <- compartment_LoS_shape %>% 
  as_tibble(rownames = "age_class") %>%
  pivot_longer(cols = -age_class, names_to = "compartment", values_to = "shape")

imperial_values <- imperial_means %>%
  left_join(imperial_shapes) %>%
  mutate(source = "imperial") %>%
  rowwise() %>%
  rename(wide_age_class = age_class) %>%
  mutate(narrow_age_class = list(breakdown_age_classes(wide_age_class, 5, 80))) %>% 
  unnest(narrow_age_class) %>%
  
  filter(compartment %in% compartments_of_interest,
         age_class %in% ages_of_interest)

our_values <- results %>%
  mutate(source = "ours") %>%
  
  filter(compartment %in% compartments_of_interest,
         age_class %in% ages_of_interest)

all_values <- bind_rows(
  our_values,
  imperial_values
) %>%
  
  rowwise() %>%
  mutate(plot_values = list(draw_gamma_dist(shape, shape / mean))) %>%
  ungroup() %>%
  
  unnest(plot_values)

observed_data <- bind_rows(
  bind_rows(ward_died_data,ward_discharge_data,ward_ICU_data) %>%
    mutate(ward_coding = if_else(ward_coding == "censored", "censored_ward", ward_coding)) %>%
    rename(LoS = ward_LoS,
           compartment = ward_coding),
  
  bind_rows(ICU_death_data, ICU_discharge_data, ICU_postICU_data) %>%
    mutate(ICU_coding = if_else(ICU_coding == "censored", "censored_ICU", ICU_coding)) %>%
    rename(LoS = ICU_LoS,
           compartment = ICU_coding),
  
  bind_rows(postICU_death_data, postICU_discharge_data) %>%
    mutate(postICU_coding = if_else(postICU_coding == "censored", "censored_postICU", postICU_coding)) %>%
    rename(LoS = postICU_LoS,
           compartment = postICU_coding)
) %>%
  select(LoS, compartment, age_class) %>%
  right_join(age_class_expansion_table, by = c("age_class" = "wide_age_class")) %>%
  mutate(is_censored = compartment %in% c("censored_ward", "censored_ICU", "censored_postICU")) %>%
  
  select(age_class = narrow_age_class, compartment, LoS, is_censored) %>%
  full_join(compartment_expansion, by = c("compartment" = "from")) %>%
  mutate(compartment = if_else(!is.na(to), to, compartment)) %>%
  
  filter(compartment %in% compartments_of_interest,
         age_class %in% ages_of_interest)
  

our_values_plot <- our_values %>%
  
  rowwise() %>%
  mutate(plot_values = list(draw_gamma_dist(shape, shape / mean))) %>%
  ungroup() %>%
  
  unnest(plot_values)

reorder_compartments <- . %>%
  mutate(compartment = factor(compartment, levels = compartment_labels))

ggplot() +
  geom_histogram(aes(x = LoS, y =..density.., fill = is_censored),
                 observed_data %>% reorder_compartments,
                 binwidth = 1) +
  
  geom_line(aes(x = x, y = y),
            our_values_plot %>% reorder_compartments) +
  
  facet_grid(cols = vars(compartment), rows = vars(age_class)) +
  
  coord_cartesian(xlim = c(0,60),
                  ylim = c(0, 0.5)) +
  
  scale_fill_brewer(type = 'qual',
                     palette = 6) +
  
  theme_minimal() +
  theme(legend.position = 'bottom')

ggsave(paste0(output_dir, "/fit_against_observed.png"),
       width = 15, height = 7, bg = 'white')

ggplot(all_values %>% reorder_compartments) +
  geom_line(aes(x = x, y = y, color = source)) +
  
  facet_grid(cols = vars(compartment), rows = vars(age_class)) +
  
  coord_cartesian(xlim = c(0,30),
                  ylim = c(0, 0.5)) +
  
  scale_color_brewer(type = 'qual',
                    palette = 2) +
  
  theme_minimal() +
  theme(legend.position = 'bottom')


ggsave(paste0(output_dir, "/fit_against_imperial.png"),
       width = 15, height = 7, bg = 'white')
