
library(targets)
library(tidyverse)
library(lubridate)

load(file = "../curvemush/.debug")

devtools::load_all("../curvemush/")

occupancy_curve_match <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
) %>%
  mutate(do_match = date >= forecast_dates$forecast_start & date <= forecast_dates$forecast_start + ddays(7)) %>%
  
  left_join(
    
    true_occupancy_curve %>%
      filter(date >= forecast_dates$simulation_start) %>%
      select(date, group, count) %>%
      pivot_wider(names_from = group, values_from = count)
    
  ) %>%
  
  mutate(ward_vec = if_else(do_match, ward, -1),
         ICU_vec = if_else(do_match, ICU, -1))



thresholds_vec <- c(seq(10, 5, by = -0.5), seq(4.5, 1, by = -0.1), seq(0.99, 0.1, by = -0.01))
#thresholds_vec <- c(seq(10, 5, by = -0.5), seq(4.5, 1, by = -0.1))
#thresholds_vec <- 10

results_null <- curvemush::mush_abc_smc(
  n_parameter_samples = 4000,
  n_particles = 1000,
  n_delay_samples = 512,
  
  n_days = case_trajectories$n_days,
  steps_per_day = 4,
  
  thresholds_vec = thresholds_vec,
  
  ensemble_curves = case_curves,
  
  forecasting_parameters = forecasting_parameters,
  
  known_ward_vec = occupancy_curve_match$ward_vec,
  known_ICU_vec = occupancy_curve_match$ICU_vec,
  
  mat_pr_age_given_case = mat_pr_age_given_case,
  mat_pr_hosp = mat_pr_hosp,
  mat_pr_ICU = mat_pr_ICU
)


group_labels <- c("symptomatic_clinical", "ward", "ICU", "discharged", "died")
format_grouped <- . %>%
  mutate(date = forecast_dates$simulation_start + ddays(t_day),
         group = group_labels[compartment_group + 1])


results_count_quants_null <- results_null$grouped_results %>%
  select(-c(transitions)) %>%
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  make_results_quants() %>%
  format_grouped()


ggplot(results_count_quants_null %>% filter(group == "ward")) +
  
  geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed') +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
              fill = 'purple', alpha = 0.2) +
  
  geom_line(aes(x = date, y = ward, linetype = do_match, group = data.table::rleid(do_match)),
            occupancy_curve_match) +
  
  
  coord_cartesian(#xlim = c(forecast_dates$forecast_start - ddays(120), NA),
    ylim = c(0, 3500)) +
  
  theme_minimal() +
  
  ggtitle("NSW", "With ABC-SMC") +
  
  theme(legend.position = "none")

cowplot::plot_grid(
    
  ggplot(results_count_quants_null %>% filter(group == "ward")) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed') +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'purple', alpha = 0.2) +
    
    geom_line(aes(x = date, y = ward, linetype = do_match, group = data.table::rleid(do_match)),
              occupancy_curve_match) +
    
    
    coord_cartesian(#xlim = c(forecast_dates$forecast_start - ddays(120), NA),
      ylim = c(0, 3500)) +
    
    theme_minimal() +
    
    ggtitle("NSW", "With ABC-SMC") +
    
    theme(legend.position = "none"),
  
  ggplot(results_count_quants_null %>% filter(group == "ICU")) +
    
    geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dashed') +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant),
                fill = 'green4', alpha = 0.2) +
    
    geom_line(aes(x = date, y = ICU, linetype = do_match, group = data.table::rleid(do_match)),
              occupancy_curve_match) +
    
    
    coord_cartesian(#xlim = c(forecast_dates$forecast_start - ddays(120), NA),
      ylim = c(0, 300)) +
    
    theme_minimal() +
    
    ggtitle("", "With ABC - SMC") +
    
    theme(legend.position = "none"),
  
  ncol = 1
)

results_null$n_accepted







do.call(cbind, results_null$smc_spline_fits) %>%
  as_tibble() %>%
  `colnames<-`(str_c("p_", 1:2000)) %>%
  mutate(t_day = row_number(),
         date = forecast_dates$simulation_start + ddays(t_day)) %>%
  
  left_join(occupancy_curve_match %>% select(date, do_match)) %>%
  filter(do_match) %>%
  select(-do_match) %>%
  
  pivot_longer(cols = -c(date, t_day)) %>%
  
  ggplot() +
  geom_line(aes(x = date, y = value, group = name))
