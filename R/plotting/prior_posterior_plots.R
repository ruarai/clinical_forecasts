

make_prior_posterior_plot <- function(
  prior_results, posterior_results,
  
  forecast_dates,
  state_forecast_start,
  known_occupancy_ts,
  plot_dir,
  state_modelled
) {
  
  known_occupancy_ts <- known_occupancy_ts %>%
    mutate(group = factor(group, levels = c("ward", "ICU")),
           do_match = date > state_forecast_start & date <= state_forecast_start + ddays(7))
  
  plot_data <- bind_rows(
    prior_results$quants_count %>%
      filter(group %in% c("ward", "ICU")) %>%
      mutate(source = "prior"),
    posterior_results$quants_count %>%
      filter(group %in% c("ward", "ICU")) %>%
      mutate(source = "posterior")
  ) %>%
    mutate(source = factor(source, levels = c("posterior", "prior")),
           group = factor(group, levels = c("ward", "ICU"))) %>%
    
    filter(quant %in% c("90", "50", "20"),
           date >= state_forecast_start - ddays(35))
  
  
  library(tidyverse)
  library(lubridate)
  
  anzics_data <- read_csv("../ancizs_analysis/all_data.csv") %>%
    filter(name == "ICU_HDU_patients_COVID", state == state_modelled) %>%
    mutate(group = "ICU",
           group = factor(group, levels = c("ward", "ICU")))
  
  
  ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant, source),
                    fill = source, color = source),
                alpha = 0.3, size = 0.3,
                plot_data %>% filter(quant != "90")) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant, source),
                    fill = source, color = source),
                alpha = 0, size = 0.3, linetype = 'dashed',
                plot_data %>% filter(quant == "90")) +
    
    geom_line(aes(x = date, y = count),
              known_occupancy_ts) +
    
    geom_point(aes(x = date, y = count),
               known_occupancy_ts %>% filter(do_match), size = 0.8) +
    
    geom_line(aes(x = date, y = value),
              anzics_data) +
    
    facet_wrap(~group, ncol = 1, scales = "free_y") +
    
    xlab(NULL) + ylab(NULL) +
    
    ggokabeito::scale_fill_okabe_ito(name = "Type",
                                     labels = c("prior" = "No fitting, no adj. factors", "posterior" = "Fitted")) +
    ggokabeito::scale_color_okabe_ito(name = "Type",
                                      labels = c("prior" = "No fitting, no adj. factors", "posterior" = "Fitted")) +
    
    scale_y_continuous(breaks = scales::breaks_extended(),
                       labels = scales::label_comma()) +
    
    
    coord_cartesian(xlim = c(state_forecast_start - ddays(30), NA)) +
    ggtitle(NULL, state_modelled) +
    
    theme_minimal() +
    
    theme(legend.position = "bottom",
          strip.text = element_text(hjust = 0))
  
  ggsave(
    paste0(plot_dir, "/", state_modelled, "_prior_posterior_plot.png"),
    width = 8, height = 6, bg = "white"
  )
  
}


