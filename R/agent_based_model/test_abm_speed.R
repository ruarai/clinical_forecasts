library(tidyverse)


setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
source("R/model_parameters.R")
model_params <- get_model_parameters()

state_modelled <- "NSW"

date_0 <- lubridate::ymd("2021-06-01")

days_sim <- as.numeric(lubridate::today() - date_0)

vaccination_prob_table <- read_rds("data/processed/vaccination_probability_table.rds")

vaccination_prob_table_adj <- vaccination_prob_table %>%
  group_by(state, date, age_class) %>%
  mutate(proportion = case_when(name == "none" ~ 0.9,
                                TRUE ~ 0.1 * proportion),
         proportion = proportion / sum(proportion))


clinical_prob_table <- read_rds("data/processed/clinical_probabilities.rds")

case_linelist_with_vacc_prob <- read_rds("data/processed/clinical_linelist.rds") %>%
  mutate(t = as.numeric(days_sim - (lubridate::today() - date_onset)),
         ix = row_number()) %>%
  filter(state == state_modelled, t >= 0) %>%
  
  filter(status_hospital == 1) %>%
  
  left_join(vaccination_prob_table_adj, by = c("state", "age_class", "date_onset" = "date")) %>%
  left_join(clinical_prob_table)


# Could be repeated
case_linelist <- case_linelist_with_vacc_prob %>%
  group_by(ix) %>%
  
  slice_sample(n = 1, weight_by = proportion) %>%
  ungroup() %>%
  select(-c(ix, proportion)) %>% rename(vaccine = name)


delay_compartment_names <- colnames(model_params$delay_params$compartment_LoS_shape)

case_delay_shapes <- model_params$delay_params$
  compartment_LoS_shape[case_linelist$age_class,]

case_delay_means <- model_params$delay_params$
  compartment_LoS_mean[case_linelist$age_class,]

case_delay_samples <- rgamma(length(case_delay_shapes),
                             shape = case_delay_shapes, rate = case_delay_shapes / case_delay_means) %>%
  matrix(ncol = ncol(case_delay_shapes)) %>%
  `colnames<-`(delay_compartment_names) %>%
  as_tibble(.name_repair = "minimal")

case_pr_death_ward <- model_params$morbidity_params$
  prob_death_ward_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]
case_pr_death_ICU <- model_params$morbidity_params$
  prob_death_ICU_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]
case_pr_death_postICU <- model_params$morbidity_params$
  prob_death_post_ICU_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]


case_parameter_samples = tibble(
  time_of_infection = case_linelist$t,
  case_delay_samples %>% rename_with(~ str_c("LoS_", .)),
  pr_ICU = case_linelist$pr_ICU,
  
  pr_death_ward = case_pr_death_ward,
  pr_death_ICU = case_pr_death_ICU,
  pr_death_postICU = case_pr_death_postICU
) %>%
  as.matrix()

Rcpp::sourceCpp("cpp/abm_loop.cpp")

a <- Sys.time()
results <- process_loop(case_parameter_samples, days_sim, 0.1)
print(Sys.time() - a)


results_plot <- do.call(rbind, results$transitions) %>%
  `colnames<-`(c("case_index", "t", "new_comp")) %>%
  as_tibble() %>%
  mutate(new_comp = as.character(new_comp),
         t = round(t),
         date = date_0 + t) %>%
  group_by(date, new_comp) %>%
  summarise(n = n(), .groups = "drop")

ggplot(results_plot) +
  geom_point(aes(x = date, y = n, color = new_comp))

compartment_names_true <- c("susceptible", "symptomatic", "ward",
                            "ward_died", "ward_discharged", "ICU", "ICU_died",
                            "postICU_to_death", "postICU_to_discharge", "postICU_died",
                            "postICU_discharged")

tbl_count <- apply(results$compartment_counts, MARGIN = 2, cumsum)[,1:length(compartment_names_true)] %>%
  `colnames<-`(compartment_names_true) %>%
  as_tibble() %>%
  
  mutate(t = row_number(),
         date = date_0 + t) %>%
  
  pivot_longer(cols = -c(date, t), names_to = "compartment", values_to = "count")

ggplot(tbl_count) +
  geom_point(aes(x = date, y = count)) +
  
  facet_wrap(~compartment, scales = "free_y")


summary_groups <- list("ward" = c("ward", "postICU_to_death", "postICU_to_discharge"), 
                       "ICU" = c("ICU"),
                       "died" = c("ward_died", "ICU_died", "postICU_died"),
                       "discharged" = c("ward_discharged", "postICU_discharged")) %>%
  map_dfr(~ tibble(compartment = .), .id="group")  %>%
  select(compartment, group) %>% deframe()

tbl_count_grouped <- tbl_count %>%
  mutate(group = summary_groups[compartment]) %>%
  
  group_by(date, group) %>%
  summarise(count = sum(count), .groups = "drop")


plot_c19data_cumulative <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == state_modelled,
         date >= date_0) %>%
  
  mutate(ward = hosp_cum - icu_cum,
         deaths_cum = deaths_cum - min(deaths_cum)) %>%
  select(date, ward, ICU = icu_cum, died = deaths_cum) %>%
  pivot_longer(cols = c(ward, ICU, died),
               names_to = "group", values_to = "count")

ggplot(tbl_count_grouped) +
  geom_point(aes(x = date, y = count)) +
  
  geom_line(aes(x = date, y = count),
            plot_c19data_cumulative,
            size = 0.5) +
  
  facet_wrap(~group, scales = "free_y")

