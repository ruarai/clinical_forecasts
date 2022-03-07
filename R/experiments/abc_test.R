library(targets)
library(tidyverse)
library(curvemush)
library(lubridate)


case_trajectories <- tar_read(case_trajectories_NSW)
nindss_state <- tar_read(nindss_state_NSW)
morbidity_estimates_state <- tar_read(morbidity_estimates_state_NSW)
clinical_parameter_samples <- tar_read(clinical_parameter_samples)
forecast_dates <- tar_read(forecast_dates)



make_results_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}



nindss_recent <- nindss_state %>%
  filter(date_onset <= forecast_dates$forecast_start,
         date_onset > forecast_dates$forecast_start - ddays(14))

age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
recent_age_dist <- nindss_recent %>%
  group_by(age_group) %>%
  summarise(pr_age_given_case = n() / nrow(.)) %>%
  
  complete(age_group = age_groups, fill = list(pr_age_given_case = 0))

forecasting_parameters <- clinical_parameter_samples %>%
  
  left_join(morbidity_estimates_state, by = c("age_group", "sample")) %>%
  
  mutate(pr_ward_to_death = 1 - pr_ward_to_ICU - pr_ward_to_discharge,
         pr_not_ICU_true = 1 - pr_ICU,
         pr_ward_to_discharge_given_not_ICU = pr_ward_to_discharge / (pr_ward_to_discharge + pr_ward_to_death)) %>%
  
  mutate(pr_ward_to_discharge = pr_ward_to_discharge_given_not_ICU * pr_not_ICU_true,
         pr_ward_to_ICU = pr_ICU) %>%
  
  select(-c(pr_ICU, pr_ward_to_death)) %>%
  
  left_join(recent_age_dist, by = 'age_group')

true_occupancy_curve <- tar_read(known_occupancy_ts_NSW) %>%
  filter(source == "c19", group == "ward", state == "NSW") %>%
  
  filter(date >= forecast_dates$simulation_start)


occupancy_curve_match <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
) %>%
  mutate(do_match = date >= forecast_dates$forecast_start & date <= forecast_dates$forecast_start + ddays(7)) %>%
  
  left_join(
    true_occupancy_curve %>% select(date, count)
  ) %>%
  
  mutate(count_vec = if_else(do_match, count, -1))


case_curves <- case_trajectories$curve_set

print("Starting...")

save.image(file = "../curvemush/.debug")

prior_sigma_los <- 0.3
prior_sigma_hosp <- 0.5

a <- Sys.time()
results <- curvemush::mush_abc(
  n_samples = 4000,
  n_delay_samples = 512,
  
  n_outputs = 1000,
  
  n_days = case_trajectories$n_days,
  steps_per_day = 16,
  
  ward_threshold = 100,
  
  prior_sigma_los = prior_sigma_los,
  prior_sigma_hosp = prior_sigma_hosp,
  
  t_forecast_start = case_trajectories$step_sampling_start,
  
  ensemble_curves = case_curves,
  
  forecasting_parameters = forecasting_parameters,
  
  known_ward_vec = occupancy_curve_match$count_vec
)
b <- Sys.time()


print(str_c("Simulation mush ABC ran in ", round(b - a, 2), " ", units(b - a)))


group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")


results_formatted <- results$grouped_results %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         group = group_labels[compartment_group + 1])



plot_data <- results_formatted %>%
  group_by(sample, group) %>%
  summarise(los_scale = first(los_scale),
            pr_hosp_scale = first(pr_hosp_scale),
            max = max(count))

ggplot(results_formatted %>%
         filter(group == "ward")) +
  geom_line(aes(x = date, y = count, group = sample),
            alpha = 0.1) +
  
  geom_line(aes(x = date, y = count),
            color = 'red3',
            true_occupancy_curve)

prior_data <- tibble(
  los_scale = rnorm(nrow(plot_data), 0, prior_sigma_los),
  pr_hosp_scale = rnorm(nrow(plot_data), 0, prior_sigma_hosp),
)

ggplot() +
  geom_histogram(aes(x = los_scale), binwidth = 0.05,
                 plot_data) +
  geom_histogram(aes(x = los_scale), binwidth = 0.05,
                 alpha = 0.5, fill = 'blue',
                 prior_data) +
  
  coord_cartesian(xlim = c(-3, 3))

ggplot()  +
  geom_histogram(aes(x = pr_hosp_scale), binwidth = 0.05,
                 plot_data) +
  geom_histogram(aes(x = pr_hosp_scale), binwidth = 0.05,
                 alpha = 0.5, fill = 'blue',
                 prior_data)  +
  
  coord_cartesian(xlim = c(-3 , 3))

ggplot() +
  geom_point(aes(x = los_scale, y = pr_hosp_scale),
             alpha = 0.05,
             prior_data) +
  geom_point(aes(x = los_scale, y = pr_hosp_scale),
             plot_data) +
  
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))

ggplot(plot_data  ) +
  
  geom_point(aes(x = los_scale, y = max)) +
  
  scale_x_log10() +
  
  facet_wrap(~group, scales = "free")

ggplot(plot_data  ) +
  
  geom_point(aes(x = pr_hosp_scale, y = max)) +
  
  scale_x_log10() +
  
  facet_wrap(~group, scales = "free")



format_grouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         group = group_labels[compartment_group + 1])


results_count_quants <- results$grouped_results %>%
  select(-c(transitions, los_scale, pr_hosp_scale)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()


ggplot(results_count_quants %>% filter(group == "ICU")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'orangered', alpha = 0.2) +
  
  geom_line(aes(x = date, y = count, linetype = do_match, group = data.table::rleid(do_match)),
            occupancy_curve_match) +
  
  
  coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(7), NA))

cowplot::plot_grid(
  
  ggplot(tar_read(state_result_quants_NSW) %>% filter(group == "ward")) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed') +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'green4', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count, linetype = do_match, group = data.table::rleid(do_match)),
              occupancy_curve_match) +
    
    
    coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(21), NA),
                    ylim = c(0, 3000)) +
    
    theme_minimal() +
    
    ggtitle("NSW", "Without ABC") +
    
    theme(legend.position = "none"),
  
  ggplot(results_count_quants %>% filter(group == "ward")) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed') +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'orangered', alpha = 0.2) +
    
    geom_line(aes(x = date, y = count, linetype = do_match, group = data.table::rleid(do_match)),
              occupancy_curve_match) +
    
    
    coord_cartesian(xlim = c(forecast_dates$forecast_start - ddays(21), NA),
                    ylim = c(0, 3000)) +
    
    ggtitle(NULL, "With ABC + scaling factors") +
    
    theme_minimal() +
    theme(legend.position = "none"),
  
  ncol = 1
)

