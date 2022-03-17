

results <- bind_rows(
  read_rds("results/NSW_Jwood_forecasts/14-03-2022_version_3_cases_quants.rds") %>% mutate(name = "cases"),
  read_rds("results/NSW_Jwood_forecasts/14-03-2022_version_3_cases15_quants.rds") %>% mutate(name = "cases15"),
  read_rds("results/NSW_Jwood_forecasts/14-03-2022_version_3_cases15Imm_quants.rds") %>% mutate(name = "cases15imm")
)

p_ward <- ggplot(results %>%
         filter(group == "ward", date >= forecast_dates$forecast_start, as.numeric(as.character(quant)) < 80)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant,name), fill = name),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant,name)),
              fill = 'green4', alpha = 0.2,
              results %>% filter(group == "ward", name == first(name), date <= forecast_dates$forecast_start, as.numeric(as.character(quant)) < 80)) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ward")) +
  
  scale_fill_manual(
    values = c("cases15imm" = "#b94f63", "cases" = "#3790cb", "cases15" = "#86d3f4")
  ) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "Ward beds occupied") +
  
  theme_minimal() +
  theme(legend.position = "none")

p_ICU <- ggplot(results %>%
         filter(group == "ICU", date >= forecast_dates$forecast_start, as.numeric(as.character(quant)) < 80)) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant,name), fill = name),
              alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(quant,name)),
              fill = 'purple', alpha = 0.2,
              results %>% filter(group == "ICU", name == first(name), date <= forecast_dates$forecast_start, as.numeric(as.character(quant)) < 80)) +
  
  geom_line(aes(x = date, y = count),
            true_occupancy_curve %>%
              filter(group == "ICU")) +
  
  scale_fill_manual(
    values = c("cases15imm" = "#b94f63", "cases" = "#3790cb", "cases15" = "#86d3f4")
  ) +
  
  geom_vline(aes(xintercept = forecast_dates$forecast_start), linetype = 'dashed') +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  ) +
  
  xlab(NULL) + ylab("Count") +
  
  ggtitle(NULL, "ICU beds occupied") +
  
  theme_minimal() +
  theme(legend.position = "none")


cowplot::plot_grid(
  p_ward,
  p_ICU,
  ncol = 1,
  align = "hv", axis = "lr"
)


ggsave(
  paste0("results/NSW_Jwood_forecasts/", run_name, "_", "joint", "_estimates.png"),
  width = 10, height = 8, bg = "white"
)

