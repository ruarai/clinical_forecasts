library(targets)
library(tidyverse)
library(curvemush)
library(lubridate)

source("R/experiments/adj_for_missing_RATs.R")

run_name <- "26-04-2022"
run_subname <- "v1_normal_delay"
save <- FALSE

projections <- read_csv("~/random_data/JWood/Projections_20220426.csv") %>%
  mutate(date = ymd(Date), n = Cases) %>%
  drop_na(n) %>%
  select(date, n) %>%
  
  filter(date >= ymd("2022-04-15"))

local_cases <- tar_read(local_cases_state_NSW)

combined_incidence <- bind_rows(
  projections %>% 
    select(date_onset = date, count = n),
  
  local_cases %>% 
    filter(date_onset >= ymd("2021-11-01"),
           date_onset < min(projections$date)) %>%
    
    mutate(count = count / detection_probability)
)


forecast_dates <- tibble(
  forecast_start = min(projections$date),
  simulation_start = min(combined_incidence$date_onset),
  forecast_horizon = max(combined_incidence$date_onset)
)


ggplot(combined_incidence) +
  geom_line(aes(x = date_onset, y = count)) +
  
  theme_minimal() +
    
  scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
  
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = "dashed") +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Case incidence (backcast and forecast)")


age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


forecasting_parameters <- tar_read(clinical_parameter_samples)# %>%
  # mutate(shape_onset_to_ward = 1.1 * shape_onset_to_ward,
  #        scale_onset_to_ward = 1.1 * scale_onset_to_ward)

morbidity_trajectories <- tar_read(morbidity_trajectories_state_NSW) %>%
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

quants_plot <- morbidity_trajectories %>%
  pivot_longer(starts_with("pr_")) %>%
  group_by(date, age_group, name) %>%
  summarise(q95_lower = quantile(value, 0.025),
            q95_upper = quantile(value, 0.975),
            q50_lower = quantile(value, 0.25),
            q50_upper = quantile(value, 0.75),
            median = median(value))

ggplot(quants_plot %>% filter(date >= ymd("2021-12-15"))) +
  
  geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = name),
              alpha = 0.5) +
  geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = name),
              alpha = 0.75) +
  
  ggokabeito::scale_fill_okabe_ito() +
  ggokabeito::scale_color_okabe_ito() +
  
  facet_wrap(~age_group * name, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
  
  theme_minimal() +
  theme(legend.position = "none")


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
  n_samples = 4000,
  n_delay_samples = 512,
  
  n_outputs = 4000,
  
  n_days = nrow(case_matrix),
  steps_per_day = 16,
  
  do_ABC = FALSE,
  thresholds = c(10000),
  rejections_per_selections = 1,
  
  prior_sigma_los = 0,
  prior_sigma_hosp = 0,
  
  ensemble_curves = case_matrix,
  
  forecasting_parameters = forecasting_parameters,
  
  known_ward_vec = occupancy_curve_match$ward_vec,
  known_ICU_vec = occupancy_curve_match$ICU_vec,
  
  mat_pr_age_given_case = mat_pr_age_given_case,
  mat_pr_hosp = mat_pr_hosp,
  mat_pr_ICU = mat_pr_ICU
)

if(save) {
  results %>%
    write_rds(paste0("results/NSW_Jwood_forecasts/", run_name, "_", run_subname, "_results.rds")) 
}

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
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_count_quants <- results$results %>%
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_ungrouped()

results_transitions_quants <- results$grouped_results %>%
  select(-c(count)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "transitions") %>%
  make_results_quants() %>%
  format_grouped()

results_ungrouped_transitions_quants <- results$results %>%
  select(-c(count)) %>%
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

if(save) {
  results_count_quants %>%
    write_rds(paste0("results/NSW_Jwood_forecasts/", run_name, "_", run_subname, "_quants.rds"))
}


p_ward <- ggplot(results_count_quants %>%
         filter(group == "ward")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'purple', alpha = 0.2) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ward")) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  coord_cartesian(xlim = c(ymd("2022-02-01"), NA)) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "Ward beds occupied") +
  
  theme_minimal()


p_ICU <- ggplot(results_count_quants %>%
         filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'green4', alpha = 0.2) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ICU")) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  coord_cartesian(xlim = c(ymd("2022-02-01"), NA)) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "ICU beds occupied") +
  
  theme_minimal()

cowplot::plot_grid(
  p_ward,
  p_ICU,
  ncol = 1,
  align = "hv", axis = "lr"
)

if(save) {
  
  ggsave(
    paste0("results/NSW_Jwood_forecasts/", run_name, "_", run_subname, "_estimates.png"),
    width = 10, height = 8, bg = "white"
  )
  
  
  
  
  results$grouped_results  %>%
    format_grouped() %>%
    select(sample, date, group, count) %>%
    filter(group == "ward" | group == "ICU") %>%
    pivot_wider(names_from = group, values_from = count) %>%
    
    group_by(date) %>%
    
    summarise(
      ward_median = median(ward),
      ward_lower90 = quantile(ward, 0.05),
      ward_upper90 = quantile(ward, 0.95),
      ward_lower95 = quantile(ward, 0.025),
      ward_upper95 = quantile(ward, 0.975),
      
      ICU_median = median(ICU),
      ICU_lower90 = quantile(ICU, 0.05),
      ICU_upper90 = quantile(ICU, 0.95),
      ICU_lower95 = quantile(ICU, 0.025),
      ICU_upper95 = quantile(ICU, 0.975),
    ) %>%
    
    write_csv(paste0("results/NSW_Jwood_forecasts/", run_name, "_", run_subname, "_result_summaries.csv"))
}



