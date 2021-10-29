library(tidyverse)


setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
source("R/model_parameters.R")
source("R/agent_based_model/covid_case.R")
model_params <- get_model_parameters()

state_modelled <- "NSW"

days_sim <- as.numeric(lubridate::today() - lubridate::ymd("2020-01-01"))

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


n_cases <- nrow(case_linelist)


library(future.callr)
library(furrr)
plan(callr)

source("R/agent_based_model/run_simulation.R")

results <- future_map(1:100, function(i) run_simulation(case_linelist))
#results <- list(run_simulation(case_linelist))


plot_results_count_grouped <- map_dfr(results, function(result) {
  result$tbl_count_grouped
  }, .id = "index" ) %>%
  mutate(date = t + (lubridate::today() - days_sim)) %>%
  filter(group %in% c("died", "ward", "ICU"))

plot_results_transitions_grouped <- map_dfr(results, function(result) {
  result$tbl_transitions_grouped
}, .id = "index" ) %>%
  mutate(t = t %/% 10) %>%
  group_by(index, group, t) %>% 
  summarise(count = sum(count)) %>%
  
  mutate(date = t + (lubridate::today() - days_sim)) %>%
  filter(group %in% c("died", "ward", "ICU"))


covid19data <- read_rds("data/covid19data.rds")

plot_c19data_cumulative <- covid19data %>%
  filter(state_abbrev == state_modelled) %>%
  
  mutate(ward = hosp_cum - icu_cum) %>%
  select(date, ward, ICU = icu_cum, died = deaths_cum) %>%
  pivot_longer(cols = c(ward, ICU, died),
               names_to = "group", values_to = "count")

plot_c19data_instant <-  covid19data %>%
  filter(state_abbrev == state_modelled) %>%
  
  mutate(ward = hosp - icu) %>%
  select(date, ward, ICU = icu, died = deaths) %>%
  pivot_longer(cols = c(ward, ICU, died),
               names_to = "group", values_to = "n")


p1 <- ggplot() +
  geom_line(aes(x = date, y = count, group = index),
            plot_results_count_grouped) +
  
  geom_point(aes(x = date, y = count),
             plot_c19data_cumulative,
             size = 0.5, color = 'orangered') +
  
  facet_wrap(~group,
             scales = "free_y")
p1
p1 + coord_cartesian(xlim = c(today() - 60, today()))




p2 <- ggplot() +
  geom_linerange(aes(x = date, ymin = 0, ymax = count, group = index),
                 size = 0.5, 
                 plot_results_transitions_grouped %>% filter(group == "died")) +
  
  geom_point(aes(x = date, y = n),
             plot_c19data_instant %>% filter(group == "died"),
             size = 0.5, color = 'orangered')
p2
p2 + coord_cartesian(xlim = c(today() - 60, today()))




ggplot() +
  geom_line(aes(x = date, y = count, group = index, color = 'model, using NNDSS'),
            plot_results_count_grouped %>% filter(group == "ward"),
            alpha = 0.1) +
  
  geom_line(aes(x = date, y = count, color = 'public data'),
             plot_c19data_cumulative %>% filter(group == "ward"),
             size = 0.5) +
  
  facet_wrap(~group,
             scales = "free_y") + 
  coord_cartesian(xlim = c(today() - 120, today()))
