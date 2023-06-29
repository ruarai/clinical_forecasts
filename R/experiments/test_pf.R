
library(targets)
library(tidyverse)
library(lubridate)

morbidity_trajectories_state <- tar_read(morbidity_trajectories_state_NSW)
forecast_dates <- tar_read(forecast_dates)

clinical_parameters <- tar_read(clinical_parameters)
case_trajectories <- tar_read(case_trajectories_NSW)

clinical_parameter_samples <- tar_read(clinical_parameter_samples)
known_occupancy_ts <- tar_read(known_occupancy_ts_NSW)

state_forecast_start <- tar_read(state_forecast_start_NSW)




Sys.setenv(JULIA_PROJECT="/home/forecast/source/stochastic_progression/StochasticProgression/", JULIA_NUM_THREADS = 32)

library(JuliaCall)


case_curves <- case_trajectories$curve_set
n_steps_per_day <- 4


occupancy_curve_match <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
)  %>%
  mutate(do_match = date > state_forecast_start - days(120)) %>%
  left_join(
    
    known_occupancy_ts %>%
      filter(date >= forecast_dates$simulation_start) %>%
      select(date, group, count) %>%
      
      # Have to do this for... some reason
      group_by(date, group) %>%
      slice(1) %>%
      ungroup() %>%
      
      pivot_wider(names_from = group, values_from = count)
    
  ) %>%
  
  mutate(ward_vec = if_else(do_match, ward, -1),
         ward_vec = replace_na(ward_vec, -1),
         ICU_vec = if_else(do_match, ICU, -1),
         ICU_vec = replace_na(ICU_vec, -1)) %>%
  mutate(t = row_number())


morbidity_trajectories_state_ix <- morbidity_trajectories_state %>%
  mutate(t = match(date, unique(date)))

epsilons <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 10.0)


julia_source(
  "../stochastic_progression/inference_pf.jl"
)


results <- julia_call(
  "run_inference_exact",
  case_trajectories$n_days,
  n_steps_per_day,
  1200,
  
  case_curves,
  clinical_parameter_samples,
  morbidity_trajectories_state_ix,
  
  epsilons,
  
  cbind(occupancy_curve_match$ward_vec, occupancy_curve_match$ICU_vec)
)

ggplot(results %>% filter(particle < 400, day > 150)) +
  geom_line(aes(x = day, y = sim_ward, group = particle),
            size = 0.1, alpha = 0.5) +
  geom_point(aes(x = t, y = ward_vec),
             occupancy_curve_match %>% filter(ward_vec > -0.5, t > 150)) +

  theme_minimal() +

  coord_cartesian(ylim = c(0, 100))
# 
# ggplot(results) +
#   geom_line(aes(x = day, y = sim_ward_outbreak, group = particle),
#             size = 0.1, alpha = 0.1) +
#   
#   theme_minimal() +
#   
#   coord_cartesian(ylim = c(0, 30))
# 

source("R/make_result_quants.R")

ward_quants <- results %>%
  select(day, particle, value = sim_ward) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(c(0.5, 0.9, 0.95))
ward_outbreak_quants <- results %>%
  select(day, particle, value = sim_ward_outbreak) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(c(0.5, 0.9, 0.95))

ward_only_quants <- results %>%
  select(day, particle, value = sim_ward_progression) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(c(0.5, 0.9, 0.95))

y_lim <- 2500

ggplot() +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
              ward_quants, fill = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),
            ward_quants,
            colour = ggokabeito::palette_okabe_ito(5),
            alpha = 0.2) +
  geom_point(aes(x = t, y = ward_vec),
             occupancy_curve_match %>% filter(ward_vec > -0.5)) +
  
  coord_cartesian(ylim = c(0, y_lim)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Total ward occupancy")
cowplot::plot_grid(
  ggplot() +
    geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
                ward_quants, fill = ggokabeito::palette_okabe_ito(5),
                alpha = 0.2) +
    geom_line(aes(x = day, y = median),
              ward_quants,
              colour = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
    geom_point(aes(x = t, y = ward_vec),
               occupancy_curve_match %>% filter(ward_vec > -0.5)) +
    
    coord_cartesian(ylim = c(0, y_lim)) +
    
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Total ward occupancy"),
  
  ggplot() +
    geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
                ward_only_quants, fill = ggokabeito::palette_okabe_ito(3),
                alpha = 0.2) +
    geom_line(aes(x = day, y = median),
              ward_only_quants,
              colour = ggokabeito::palette_okabe_ito(3),
              alpha = 0.2) +
    geom_point(aes(x = t, y = ward_vec),
               occupancy_curve_match %>% filter(ward_vec > -0.5)) +
    
    coord_cartesian(ylim = c(0, y_lim)) +
    
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Progression ward occupancy"),
  
  ggplot() +
    geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
                ward_outbreak_quants, fill = ggokabeito::palette_okabe_ito(2),
                alpha = 0.2) +
    geom_line(aes(x = day, y = median),
              ward_outbreak_quants,
              colour = ggokabeito::palette_okabe_ito(2),
              alpha = 0.2) +
    geom_point(aes(x = t, y = ward_vec),
               occupancy_curve_match %>% filter(ward_vec > -0.5)) +
    
    coord_cartesian(ylim = c(0, y_lim)) +
    
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Outbreak ward occupancy"),

  ncol = 1,
  align = "v"
)



ICU_quants <- results %>%
  select(day, particle, value = sim_ICU) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(c(0.5, 0.9, 0.95))

ggplot() +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
              ICU_quants, fill = ggokabeito::palette_okabe_ito(2),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),
            ICU_quants,
            colour = ggokabeito::palette_okabe_ito(2),
            alpha = 0.5) +
  geom_point(aes(x = t, y = ICU_vec),
             occupancy_curve_match %>% filter(ward_vec > -0.5)) +
  
  coord_cartesian(ylim = c(0, 50)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("ICU occupancy")



plot_tiled <- function(results, col, y_res = 100) {
  results_val <- results %>%
    mutate(val = !!sym(col))
  
  val_range <- range(results_val$val)
  step_size <- (val_range[2] - val_range[1]) / y_res
  
  counts <- results_val %>%
    mutate(val = round(val / step_size, digits = 0) * step_size) %>% 
    #group_by(day) %>%
    #mutate(day_weight_sum = sum(weight)) %>% 
    count(day, val)# %>%
    #summarise(n = sum(weight) / day_weight_sum)
  
  ggplot() +
    geom_tile(aes(x = day, y = val, fill = log(n)),
              counts) +
    
    theme_minimal()
}

results %>%
  plot_tiled("sim_ward", y_res = 100) +
  geom_point(aes(x = t, y = ward_vec),
             colour = "red", size = 0.5,
             occupancy_curve_match %>% filter(ward_vec > -0.5))

cowplot::plot_grid(
  results %>%
    plot_tiled("sim_ward", y_res = 50) +
    geom_point(aes(x = t, y = ward_vec),
               colour = "red", size = 0.5,
               occupancy_curve_match %>% filter(ward_vec > -0.5)),
  
  results %>%
    plot_tiled("adj_pr_hosp", y_res = 100),
  
  results %>%
    plot_tiled("adj_los", y_res = 100),
  results %>%
    mutate(importation_rate = 1 / exp(log_importation_rate)) %>%
    plot_tiled("log_importation_rate", y_res = 100),
  results %>%
    mutate(clearance_mean = 1 / exp(log_importation_rate)) %>%
    plot_tiled("log_importation_rate", y_res = 100),
  
  ncol = 1, align = "v"
)



results %>%
  plot_tiled("sim_ICU", y_res = 100) +
  geom_point(aes(x = t, y = ICU_vec),
             colour = "red", size = 0.5,
             occupancy_curve_match %>% filter(ward_vec > -0.5))




results %>%
  plot_tiled("adj_los", y_res = 100)


results %>%
  plot_tiled("adj_pr_hosp", y_res = 100)
