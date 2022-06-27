

morbidity_trajectories_state <- tar_read(morbidity_trajectories_state_NSW)
forecast_dates <- tar_read(forecast_dates)
plot_dir <- tar_read(plot_dir)

plot_data_age <- morbidity_trajectories_state %>%
  select(bootstrap, date, age_group, pr_age_adj = pr_age_given_case, pr_age_old) %>%
  
  pivot_longer(c(pr_age_adj, pr_age_old), names_prefix = "pr_age_") %>%
  
  group_by(date, age_group, name) %>%
  
  summarise(median = median(value),
            lower_90 = quantile(value, 0.05),
            upper_90 = quantile(value, 0.95))


ggplot(plot_data_age) +
  geom_line(aes(x = date, y = median, color = age_group, linetype = name)) +
  
  geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = age_group),
              alpha = 0.3,
              plot_data %>% filter(name == "adj")) +
  
  geom_vline(xintercept = forecast_dates$NNDSS - ddays(7),
             linetype = 'dashed') +
  
  theme_minimal()

ggsave(paste0(plot_dir, "/age_distribution.png"),
       width = 9, height = 9, bg = "white")



plot_data_hosp <- morbidity_trajectories_state %>%
  select(bootstrap, date, age_group, pr_hosp_adj = pr_hosp, pr_hosp_old) %>%
  
  pivot_longer(c(pr_hosp_adj, pr_hosp_old), names_prefix = "pr_hosp_") %>%
  
  group_by(date, age_group, name) %>%
  
  summarise(median = median(value),
            lower_90 = quantile(value, 0.05),
            upper_90 = quantile(value, 0.95))




ggplot(plot_data_hosp) +
  geom_line(aes(x = date, y = median, color = age_group, linetype = name)) +
  
  geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = age_group),
              alpha = 0.3,
              plot_data_hosp %>% filter(name == "adj")) +
  
  geom_vline(xintercept = forecast_dates$NNDSS - ddays(7),
             linetype = 'dashed') +
  
  facet_wrap(~age_group, scales = "free_y") +
  
  theme_minimal()


ggsave(paste0(plot_dir, "/morbidity_ests.png"),
       width = 9, height = 9, bg = "white")
