
library(targets)
library(tidyverse)
library(lubridate)


sim_results <- tar_read(sim_results_ACT)
state_modelled <- "ACT"
forecast_dates <- tar_read(forecast_dates)
state_forecast_start <- tar_read(state_forecast_start_ACT)
forecast_name <- tar_read(forecast_name)
occupancy_data <- tar_read(occupancy_data)
plot_dir <- tar_read(plot_dir)




trajs <- sim_results$trajectories

occ_data <- occupancy_data %>% 
  filter(state == state_modelled) %>%
  filter(date >= min(trajs$date))

p_common <- list(
  xlab(NULL),
  scale_x_date(date_breaks = "months", labels = scales::label_date_short()),
  theme_minimal(),
  geom_vline(xintercept = state_forecast_start, linetype = "44", colour = ggokabeito::palette_okabe_ito(5)),
  geom_vline(xintercept = state_forecast_start + days(7), linetype = "44", colour = ggokabeito::palette_okabe_ito(5))
)

plot_trajs <- function(trajs) {
  cowplot::plot_grid(
    ggplot() +
      geom_line(aes(x = date, y = count, group = sample),
                trajs %>% filter(group == "ward"),
                alpha = 0.1) +
      
      geom_point(aes(x = date, y = count),
                 colour = ggokabeito::palette_okabe_ito(1),
                 occ_data %>% filter(group == "ward")) +
      
      ylab("Ward count") +
      
      p_common,
    
    ggplot() +
      geom_line(aes(x = date, y = count, group = sample),
                trajs %>% filter(group == "ward_outbreak"),
                alpha = 0.1) +
      
      geom_point(aes(x = date, y = count),
                 colour = ggokabeito::palette_okabe_ito(1),
                 occ_data %>% filter(group == "ward")) +
      
      ylab("Ward outbreak count") +
      
      p_common,
    
    ggplot() +
      geom_line(aes(x = date, y = count, group = sample),
                trajs %>% filter(group == "ICU"),
                alpha = 0.1) +
      
      geom_point(aes(x = date, y = count),
                 colour = ggokabeito::palette_okabe_ito(1),
                 occ_data %>% filter(group == "ICU")) +
      
      ylab("ICU count") +
      
      p_common,
    ncol = 1, align = "v", axis = "lr"
  )
}

trajs_loose <- sim_results$results_loose$simulations %>%
  mutate(date = seq(forecast_dates$simulation_start, forecast_dates$forecast_horizon, by = "day")[day]) %>%
  rename(ward = sim_ward, ward_outbreak = sim_ward_outbreak, ICU = sim_ICU,
         sample = particle) %>% 
  pivot_longer(c(ward, ward_outbreak, ICU), names_to = "group", values_to = "count") %>%
  filter(sample < 500)

plot_trajs(trajs)
plot_trajs(trajs_loose)




sim_results$results_loose$parameters %>%
  ggplot() +
  geom_point(aes(x = adj_pr_hosp, y = adj_los),
             sim_results$results_loose$parameters,
             size = 0.5) +
  
  facet_wrap(~threshold) +
  
  theme_minimal()

ggplot() +
  geom_point(aes(x = adj_pr_hosp + adj_los, y = log_importation_rate),
             sim_results$results_loose$parameters,
             size = 0.5) +
  
  facet_wrap(~threshold) +
  theme_minimal()


sim_results$results_loose$parameters %>%
  pivot_longer(c(adj_pr_hosp, adj_los, log_importation_rate)) %>% 


  ggplot() +
  geom_line(aes(x = threshold, y = value, group = particle),
            alpha = 0.2, size = 0.2) +
  
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  
  xlab("Threshold step") +
  ylab("Value") +
  
  theme_minimal()



