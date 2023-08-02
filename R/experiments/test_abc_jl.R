
library(targets)
library(tidyverse)
library(lubridate)

morbidity_trajectories_state <- tar_read(morbidity_trajectories_state_ACT)
forecast_dates <- tar_read(forecast_dates)

case_trajectories <- tar_read(case_trajectories_ACT)

clinical_parameters <- tar_read(clinical_parameters)
known_occupancy_ts <- tar_read(known_occupancy_ts_ACT)

state_forecast_start <- tar_read(state_forecast_start_ACT)



Sys.setenv(JULIA_PROJECT="/home/forecast/source/stochastic_progression/StochasticProgression/", JULIA_NUM_THREADS = 32)

library(JuliaCall)


case_curves <- case_trajectories$curve_set


occupancy_curve_match <- tibble(
  date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = 'days')
) %>%
  mutate(do_match_wide = date > state_forecast_start - days(120) & date <= state_forecast_start + days(7),
         do_match_refine = date > state_forecast_start & date <= state_forecast_start + days(7)) %>%
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
  
  mutate(ward_vec_wide = replace_na(if_else(do_match_wide, ward, -1), -1),
         ICU_vec_wide = replace_na(if_else(do_match_wide, ICU, -1), -1),
         ward_vec_refine = replace_na(if_else(do_match_refine, ward, -1), -1),
         ICU_vec_refine = replace_na(if_else(do_match_refine, ICU, -1), -1),
         obs_weight = if_else(do_match_refine, 4.0, 1.0),
         t = row_number())


morbidity_trajectories_state_ix <- morbidity_trajectories_state %>%
  mutate(t = match(date, unique(date)))


julia_source("../stochastic_progression/dependencies.jl")


results_loose <- julia_call(
  "run_inference_loose",
  
  case_trajectories$n_days,
  4,
  
  1000,
  4,
  0.25,
  100,
  
  case_curves,
  clinical_parameters,
  morbidity_trajectories_state_ix,
  
  cbind(occupancy_curve_match$ward_vec_wide, occupancy_curve_match$ICU_vec_wide),
  occupancy_curve_match$obs_weight
)

ggplot(results_loose$simulations %>% filter(particle < 500)) +
  geom_line(aes(x = day, y = sim_ward, group = particle),
            size = 0.1, alpha = 0.5) +
  geom_point(aes(x = t, y = ward),
             colour = "red",
             occupancy_curve_match %>% filter(ward_vec_wide > 0)) +
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(0, 150))

ggplot(results_loose$simulations %>% filter(particle < 500)) +
  geom_line(aes(x = day, y = sim_ward_outbreak, group = particle),
            size = 0.1, alpha = 0.5) +
  geom_point(aes(x = t, y = ward),
             colour = "red",
             occupancy_curve_match %>% filter(ward_vec_wide > 0)) +
  
  theme_minimal() +
  
  
  coord_cartesian(ylim = c(0, 150))

results_loose$parameters %>%
  ggplot() +
  geom_point(aes(x = adj_pr_hosp, adj_los)) +
  
  facet_wrap(~threshold)


results_loose$parameters %>%
  ggplot() +
  geom_histogram(aes(x = log_importation_rate)) +
  
  facet_wrap(~threshold)

results_loose$parameters %>%
  filter(threshold == max(threshold)) %>%
  mutate(rate_prior = rnorm(n(), -8, 2)) %>% 
  ggplot() +
  geom_histogram(aes(x = log_importation_rate), binwidth = 0.1) +
  geom_histogram(aes(x = rate_prior), alpha = 0.1, fill = "red", binwidth = 0.1)


results_loose$parameters %>%
  ggplot() +
  geom_line(aes(x = threshold, y = log_importation_rate, group = particle),
            alpha = 0.1)

results_loose$parameters %>%
  ggplot() +
  geom_line(aes(x = threshold, y = adj_pr_hosp + adj_los, group = particle),
            alpha = 0.1)

ggplot(results_loose$simulations %>% filter(particle < 500)) +
  geom_line(aes(x = day, y = sim_ICU, group = particle),
            size = 0.1, alpha = 0.5) +
  geom_point(aes(x = t, y = ICU),
             colour = "red",
             occupancy_curve_match) +
  
  theme_minimal() +
  
  coord_cartesian(ylim = c(0, 150))

source("R/make_result_quants.R")

ward_quants <- results_loose$simulations %>%
  select(day, particle, value = sim_ward) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(seq(0.2, 0.9, by = 0.1))

ward_outbreak_quants <- results_loose$simulations %>%
  select(day, particle, value = sim_ward_outbreak) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(c(0.5, 0.9, 0.95))

ward_only_quants <- results_loose$simulations %>%
  mutate(sim_ward_progression = sim_ward - sim_ward_outbreak) %>% 
  select(day, particle, value = sim_ward_progression) %>% 
  pivot_wider(names_from = particle, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants()




y_lim <- 2000

ggplot() +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
              ward_quants, fill = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),
            ward_quants,
            colour = ggokabeito::palette_okabe_ito(5),
            alpha = 0.2) +
  geom_point(aes(x = t, y = ward_vec_wide),
             occupancy_curve_match) +
  
  coord_cartesian(ylim = c(0, y_lim)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Total ward occupancy")


ward_curve_intervals <- results_loose$simulations %>%
  select(day, particle, value = sim_ward) %>%
  group_by(day) %>%
  ggdist::curve_interval(value, .interval = "mbd", .width = c(0.5, 0.9, 0.95))

ggplot() +
  geom_ribbon(aes(x = day, ymin = .lower, ymax = .upper, group = .width),
              ward_curve_intervals, fill = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),
            ward_quants,
            colour = ggokabeito::palette_okabe_ito(5),
            alpha = 0.2) +
  geom_point(aes(x = t, y = ward),
             occupancy_curve_match) +
  
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
    geom_point(aes(x = t, y = ward_vec_wide),
               occupancy_curve_match %>% filter(ward_vec_wide > -0.5)) +
    
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
    geom_point(aes(x = t, y = ward_vec_wide),
               occupancy_curve_match %>% filter(ward_vec_wide > -0.5)) +
    
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
    geom_point(aes(x = t, y = ward_vec_wide),
               occupancy_curve_match %>% filter(ward_vec_wide > -0.5)) +
    
    coord_cartesian(ylim = c(0, y_lim)) +
    
    theme_minimal() +
    xlab(NULL) + ylab(NULL) +
    
    ggtitle("Outbreak ward occupancy"),
  
  ncol = 1,
  align = "v"
)



ICU_quants <- results_loose$simulations %>%
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
  geom_point(aes(x = t, y = ICU_vec_wide),
             occupancy_curve_match %>% filter(ICU_vec_wide > -0.5)) +
  
  coord_cartesian(ylim = c(0, 50)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("ICU occupancy")



plot_tiled <- function(results, col, res_factor = 1.0) {
  results_val <- results %>%
    mutate(val = !!sym(col))
  
  val_range <- range(results_val$val)
  y_res <- floor(val_range[2] * res_factor)
  
  
  step_size <- floor((val_range[2] - val_range[1]) / y_res)
  
  counts <- results_val %>%
    filter(day > 50) %>% 
    mutate(val = val - val %% step_size) %>% 
    count(day, val) %>%
    complete(day, val = seq(0, max(val), by = step_size), fill = list(n = 0))
  
  ggplot() +
    geom_tile(aes(x = day, y = val, fill = log(n)),
              counts) +
    
    scale_fill_viridis_c(na.value = "black") +
    
    theme_minimal()
}

results_loose$simulations %>%
  plot_tiled("sim_ward", res_factor = 0.1) +
  geom_point(aes(x = t, y = ward_vec_wide),
             colour = "red", size = 0.5,
             occupancy_curve_match %>% filter(ward_vec_wide > -0.5)) +
  coord_cartesian(ylim = c(0, y_lim))


prior_parameters <- results_loose$parameters %>%
  filter(threshold == max(threshold))

results_refined <- julia_call(
  "run_inference_refine",
  
  case_trajectories$n_days,
  4,
  
  1000,
  c(0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 10.0),
  #c(10, 20),
  0.001,
  
  case_curves,
  clinical_parameter_samples,
  morbidity_trajectories_state_ix,
  
  prior_parameters,
  
  cbind(occupancy_curve_match$ward_vec_refine, occupancy_curve_match$ICU_vec_refine)
)

results_refined %>%
  plot_tiled("sim_ward", res_factor = 1.0) +
  geom_point(aes(x = t, y = ward_vec_wide),
             colour = "red", size = 0.5,
             occupancy_curve_match %>% filter(ward_vec_wide > -0.5)) +
  coord_cartesian(ylim = c(0, y_lim))


results_refined %>%
  select(day, sample, value = sim_ward) %>% 
  pivot_wider(names_from = sample, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(seq(0.2, 0.9, by = 0.1)) %>% 

  ggplot() +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
              fill = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),colour = ggokabeito::palette_okabe_ito(5),
            alpha = 0.2) +
  geom_point(aes(x = t, y = ward),
             occupancy_curve_match) +
  
  coord_cartesian(ylim = c(0, y_lim)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Total ward occupancy")



results_refined %>%
  select(day, sample, value = sim_ICU) %>% 
  pivot_wider(names_from = sample, names_prefix = "sim_", values_from = value) %>% 
  
  make_results_quants(seq(0.2, 0.9, by = 0.1)) %>% 
  
  ggplot() +
  geom_ribbon(aes(x = day, ymin = lower, ymax = upper, group = quant),
              fill = ggokabeito::palette_okabe_ito(5),
              alpha = 0.2) +
  geom_line(aes(x = day, y = median),colour = ggokabeito::palette_okabe_ito(5),
            alpha = 0.2) +
  geom_point(aes(x = t, y = ICU),
             occupancy_curve_match) +
  
  coord_cartesian(ylim = c(0, 50)) +
  
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  
  ggtitle("Total ICU occupancy")

