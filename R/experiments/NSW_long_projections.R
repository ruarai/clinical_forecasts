library(targets)
library(tidyverse)
library(curvemush)
library(lubridate)

source("R/experiments/adj_for_missing_RATs.R")

projections <- read_csv("~/random_data/projectionsBA2.csv") %>%
  
  mutate(date = dmy(Date), n = BA.1 + BA.2) %>%
  select(date, n)

local_cases <- tar_read(local_cases_state_NSW)

combined_incidence <- bind_rows(
  projections %>% 
    select(date_onset = date, count = n),
  
  local_cases %>% 
    filter(date_onset >= ymd("2021-12-01"),
           date_onset < min(projections$date)) %>%
    
    mutate(count = count / detection_probability)
)

forecast_dates <- tibble(
  forecast_start = min(projections$date),
  simulation_start = min(combined_incidence$date_onset),
  forecast_horizon = max(combined_incidence$date_onset),
  
)


ggplot(combined_incidence) +
  geom_line(aes(x = date_onset, y = count)) +
  
  theme_minimal() +
  
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Case incidence (backcast and forecast)")


age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")



recent_age_dist <- tar_read(nindss_state_NSW) %>%
  filter(date_onset <= forecast_dates$forecast_start,
         date_onset > forecast_dates$forecast_start - ddays(14)) %>%
  group_by(age_group) %>%
  summarise(pr_age_given_case = n() / nrow(.)) %>%
  
  complete(age_group = age_groups, fill = list(pr_age_given_case = 0))

forecasting_parameters <- tar_read(clinical_parameter_samples) %>%
  
  left_join(tar_read(morbidity_estimates_state_NSW), by = c("age_group", "sample")) %>%
  
  mutate(pr_ward_to_death = 1 - pr_ward_to_ICU - pr_ward_to_discharge,
         pr_not_ICU_true = 1 - pr_ICU,
         pr_ward_to_discharge_given_not_ICU = pr_ward_to_discharge / (pr_ward_to_discharge + pr_ward_to_death)) %>%
  
  mutate(pr_ward_to_discharge = pr_ward_to_discharge_given_not_ICU * pr_not_ICU_true,
         pr_ward_to_ICU = pr_ICU) %>%
  
  select(-c(pr_ICU, pr_ward_to_death)) %>%
  
  left_join(recent_age_dist, by = 'age_group') %>%
  
  mutate(shape_onset_to_ward = 0.7 * shape_onset_to_ward,
         scale_onset_to_ward = 0.7 * scale_onset_to_ward)

morbidity_trajectories <- tar_read(morbidity_trajectories_NSW) %>%
  filter(date >= forecast_dates$simulation_start) %>%
  complete(date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days'),
           bootstrap = 1:50,
           age_group = age_groups) %>%
  
  select(-c(pr_age_given_case, pr_hosp)) %>%
  
  left_join(
    adj_data %>%
      select(bootstrap, age_group, date, pr_age_given_case = pr_age_given_case_adj, pr_hosp = pr_hosp_adj) %>%
      rowwise() %>%
      mutate(age_group = case_when(
        age_group == "0-19" ~ list(c("0-9", "10-19")),
        age_group == "70+" ~ list(c("70-79", "80+")),
        TRUE ~ list(age_group)
      )) %>%
      ungroup() %>%
      unnest(age_group) %>%
      
      mutate(pr_age_given_case = case_when(
        age_group %in% c("0-9", "10-19", "70-79", "80+") ~ pr_age_given_case / 2,
        TRUE ~ pr_age_given_case
      ))
    ) %>%
  
  group_by(age_group, bootstrap) %>%
  arrange(date) %>%
  fill(pr_hosp, pr_ICU, pr_age_given_case, .direction = "downup") %>%
  ungroup()

plot_morbidity_trajectories <- morbidity_trajectories %>%
  pivot_longer(c(pr_age_given_case, pr_hosp, pr_ICU))
# 
# ggplot(plot_morbidity_trajectories) +
#   geom_line(aes(x = date, y = value, group = bootstrap),
#             color = ggokabeito::palette_okabe_ito()[3],
#             size = 0.5, alpha = 0.5) +
# 
#   facet_wrap(~age_group * name, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
# 
#   geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed") +
#   
#   coord_cartesian(xlim = c(NA_Date_, forecast_dates$forecast_start + ddays(14))) +
# 
#   theme_minimal()

#ggsave("results/NSW_Jwood_forecasts/morbidity.png", width = 8, height = 10, bg = "white")


mat_pr_age_given_case <- morbidity_trajectories %>%
  select(date, bootstrap, age_group, pr_age_given_case) %>%
  pivot_wider(names_from = bootstrap,
              values_from = pr_age_given_case) %>%
  
  arrange(date, age_group) %>%
  select(-c(date, age_group)) %>%
  as.matrix()


mat_pr_hosp <- morbidity_trajectories %>%
  select(date, bootstrap, age_group, pr_hosp) %>%
  pivot_wider(names_from = bootstrap,
              values_from = pr_hosp) %>%
  
  arrange(date, age_group) %>%
  select(-c(date, age_group)) %>%
  as.matrix()

mat_pr_ICU <- morbidity_trajectories %>%
  select(date, bootstrap, age_group, pr_ICU) %>%
  pivot_wider(names_from = bootstrap,
              values_from = pr_ICU) %>%
  
  arrange(date, age_group) %>%
  select(-c(date, age_group)) %>%
  as.matrix()


case_matrix <- combined_incidence %>%
  arrange(date_onset) %>%
  pull(count) %>%
  as.matrix()


occupancy_curve_match <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
) %>%
  mutate(do_match = date >= forecast_dates$forecast_start & date <= forecast_dates$forecast_start + ddays(7)) %>%
  
  left_join(
    
    tar_read(known_occupancy_ts_NSW) %>%
      filter(source == "c19", state == "NSW") %>%
      
      filter(date >= forecast_dates$simulation_start) %>%
      select(date, group, count) %>%
      pivot_wider(names_from = group, values_from = count)
    
  ) %>%
  
  mutate(ward_vec = if_else(do_match, ward, -1),
         ICU_vec = if_else(do_match, ICU, -1))

results <- curvemush::mush_abc(
  n_samples = 2000,
  n_delay_samples = 512,
  
  n_outputs = 1000,
  
  n_days = nrow(case_matrix),
  steps_per_day = 16,
  
  ward_threshold = 3000000,
  ICU_threshold = 300000,
  
  prior_sigma_los = 0,
  prior_sigma_hosp = 0,
  
  t_forecast_start = 0,
  
  ensemble_curves = case_matrix,
  
  forecasting_parameters = forecasting_parameters,
  
  known_ward_vec = occupancy_curve_match$ward_vec,
  known_ICU_vec = occupancy_curve_match$ICU_vec,
  
  mat_pr_age_given_case = mat_pr_age_given_case,
  mat_pr_hosp = mat_pr_hosp,
  mat_pr_ICU = mat_pr_ICU
)


source("R/progression_model.R")
group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
compartment_labels <- c(
  "symptomatic_clinical", "ward", "discharged_ward", "died_ward", "ICU",
  "discharged_ICU", "died_ICU", "postICU_to_discharge", "postICU_to_death",
  "discharged_postICU", "died_postICU"
)

format_grouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         group = group_labels[compartment_group + 1])

format_ungrouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         compartment = compartment_labels[compartment + 1])



results_count_quants <- results$grouped_results %>%
  select(-c(transitions, los_scale, pr_hosp_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_count_quants <- results$results %>%
  select(-c(transitions, los_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_ungrouped()

results_transitions_quants <- results$grouped_results %>%
  select(-c(count, los_scale, pr_hosp_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "transitions") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_transitions_quants <- results$results %>%
  select(-c(count, los_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "transitions") %>%
  make_results_quants() %>%
  format_ungrouped()


results_formatted <- results$grouped_results  %>%
  format_grouped()

results_ungrouped_formatted <- results$results  %>%
  format_ungrouped()


true_occupancy_curve <- tar_read(known_occupancy_ts_NSW) %>%
  filter(state == "NSW", source == "c19") %>%
  
  filter(date >= forecast_dates$simulation_start)


p_ward <- ggplot(results_count_quants %>%
         filter(group == "ward")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'green4', alpha = 0.2) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ward")) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "Ward beds occupied") +
  
  theme_minimal()


p_ICU <- ggplot(results_count_quants %>%
         filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'purple', alpha = 0.2) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ICU")) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "ICU beds occupied") +
  
  theme_minimal()

cowplot::plot_grid(
  p_ward,
  p_ICU,
  ncol = 1,
  align = "hv", axis = "lr"
)
