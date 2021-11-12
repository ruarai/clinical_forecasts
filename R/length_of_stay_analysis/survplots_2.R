

observed_data <- ward_discharge_data %>%
  rename(LoS = ward_LoS, coding = ward_coding)
LoS_fit <- create_parameter_table(ward_LoS_discharged, ward_age_groups)



plot_fit <- expand_grid(
  age_class = LoS_fit$age_class,
  x = seq(0, 90, by = 1)
) %>%
  left_join(LoS_fit) %>%
  mutate(y = 1 - pgamma(x, shape, rate))


ggplot() +
  stat_ecdf(aes(x = LoS, y = 1 - ..y..),
            data = observed_data,
            geom = 'step') +
  stat_ecdf(aes(x = LoS, y = 1 - ..y.., color = coding),
            data = observed_data %>% filter(coding == "censored"),
            geom = 'point') +
  
  geom_line(aes(x = x, y = y),
            data = plot_fit) +
  
  facet_wrap(~age_class)
