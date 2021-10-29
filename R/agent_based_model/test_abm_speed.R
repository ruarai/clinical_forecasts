library(tidyverse)


setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
source("R/model_parameters.R")
source("R/agent_based_model/covid_case.R")
model_params <- get_model_parameters()

state_modelled <- "VIC"

date_0 <- lubridate::ymd("2020-01-01")

days_sim <- as.numeric(lubridate::today() - date_0)

vaccination_prob_table <- read_rds("data/processed/vaccination_probability_table.rds")

case_linelist_with_vacc_prob <- read_rds("data/processed/clinical_linelist.rds") %>%
  mutate(t = as.numeric(days_sim - (lubridate::today() - date_onset)),
         ix = row_number()) %>%
  filter(state == state_modelled, date_onset >= lubridate::today() - days_sim) %>%
  
  filter(status_hospital == 1) %>%
  
  left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date"))


# Could be repeated
case_linelist <- case_linelist_with_vacc_prob %>%
  group_by(ix) %>%
  
  slice_sample(n = 1, weight_by = proportion) %>%
  ungroup() %>%
  select(-c(ix, proportion)) %>% rename(vaccine = name)


delay_compartment_names <- colnames(model_params$delay_params$compartment_LoS_shape)

case_delay_shapes <- model_params$delay_params$
  compartment_LoS_shape[case_linelist$age_class, delay_compartment_names]

case_delay_means <- model_params$delay_params$
  compartment_LoS_mean[case_linelist$age_class, delay_compartment_names]

case_delay_samples <- rgamma(length(case_delay_shapes),
                             shape = case_delay_shapes, rate = case_delay_shapes / case_delay_means) %>%
  matrix(ncol = ncol(case_delay_shapes)) %>%
  `colnames<-`(delay_compartment_names) %>%
  as_tibble(.name_repair = "minimal")


case_parameter_samples = tibble(
  time_of_infection = case_linelist$t,
  LoS_symptomatic_to_ED = case_delay_samples$symptomatic_to_ED,
  LoS_ward_to_discharge = case_delay_samples$ward_to_discharge
) %>%
  as.matrix()

Rcpp::sourceCpp("R/agent_based_model/abm_loop.cpp")

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


tbl_count <- apply(results$compartment_counts, MARGIN = 2, cumsum)[,1:3] %>%
  `colnames<-`(c("susceptible", "symptomatic", "ward")) %>%
  as_tibble() %>%
  
  mutate(t = row_number(),
         date = date_0 + t) %>%
  
  pivot_longer(cols = -c(date, t), names_to = "compartment", values_to = "count")

ggplot(tbl_count) +
  geom_point(aes(x = date, y = count, color = compartment))
