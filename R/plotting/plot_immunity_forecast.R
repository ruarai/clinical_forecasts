
plot_immunity_forecast <- function(
  immune_predictions_state,
  forecast_dates,
  plot_dir,
  state_modelled
) {

  plots_common <- list(
    scale_x_date(breaks = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "2 months"),
                 labels = scales::label_date_short(format = c("%Y", "%b")),
                 expand = expansion(mult = c(0, 0.05))),
    scale_y_continuous(breaks = scales::extended_breaks(),
                       labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.01))),
    theme_minimal(),
    theme(legend.position = "none",
          panel.grid = element_blank(),
          panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
          panel.grid.major.x = element_blank(),
          axis.ticks = element_line(colour = "grey60"),
          axis.ticks.length = unit(5, "pt"),
          axis.line = element_line(colour = "grey40"),
          plot.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          text = element_text(family = "Helvetica"))
  )
  
  plot_data <- immune_predictions_state %>%
    group_by(age_group, date) %>%
    summarise(lower = quantile(m_hosp, c(0.025, 0.05, 0.25)),
              upper = quantile(m_hosp, c(0.975, 0.95, 0.75)),
              quant = c("95","90","50"), .groups = "drop")
  
  
  plot_data_median <- immune_predictions_state %>%
    group_by(age_group, date) %>%
    summarise(m = median(m_hosp), .groups = "drop_last") %>%
    mutate(m = zoo::rollmean(m, 3, fill = "extend"))
    
  quant_cols <- shades::opacity(ggokabeito::palette_okabe_ito(5), scales::rescale(rev(1/1.7^(1:3)), to = c(0.25, 1)))
    
  ggplot() +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                plot_data %>% filter(date >= forecast_dates$simulation_start) %>% mutate(quant = factor(quant, c("95", "90", "50"))))  +
    
    geom_line(aes(x = date, y = m),
              plot_data_median,
              color = ggokabeito::palette_okabe_ito(5)) +
    
    coord_cartesian(ylim = c(0, 1),
                    xlim = c(ymd("2021-12-01"), forecast_dates$forecast_horizon)) +
    
    facet_wrap(~age_group) +
    
    scale_fill_manual(values = quant_cols) +
    
    geom_hline(yintercept = 0, size = 0.8, col = 'grey40')  +
    
    geom_vline(xintercept = ymd("2021-12-01"), size = 0.8, col = 'grey40') +
    
    geom_rug(aes(x = date), tibble(date = seq(ymd("2021-01-01"), ymd("2023-01-01"), by = "month")),
             col = 'grey60') +
    
    geom_vline(xintercept = forecast_dates$NNDSS, linetype = 'dashed', col = 'grey40') +
    
    xlab(NULL) + ylab("Change in probability of severe disease") +
    
    theme_minimal() +
    
    plots_common
  
  
  ggsave(
    filename = paste0(plot_dir, "/", state_modelled, "_immunity_effect_forecast.png"),
    width = 8, height = 6,
    bg = "white"
  )

}
