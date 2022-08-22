
date_plot_start <- ymd("2022-01-01")
show_capacity <- FALSE


case_ensemble_state <- read_ensemble_state(ensemble_path, i_state, ensemble_models_included)
clinical_trajectories_wide_state <- get_state_clinical_trajectories_wide(clinical_trajectories, i_state)

local_cases_state <- local_cases %>%
  filter(state == i_state)

forecast_start_date <- get_forecast_start_date(local_cases_state, pr_detect = 0.95)
case_ensemble_state <- case_ensemble_state %>%
  filter(date <= forecast_start_date + ddays(days_horizon))

case_ensemble_wide <- case_ensemble_state %>%
  select(date, .model, starts_with("sim")) %>%
  
  pivot_wider(names_from = ".model",
              values_from = starts_with("sim"))




ensemble_quants <- case_ensemble_wide %>%
  rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
  make_results_quants(plot_quant_widths) %>%
  drop_na(lower) %>%
  
  filter(date <= forecast_start_date + ddays(days_horizon)) %>%
  
  left_join(ascertainment_ts, by = "date") %>%
  mutate(lower = lower / time_varying_75,
         upper = upper / time_varying_75)

clinical_quants_state <- clinical_trajectories_wide_state %>%
  make_results_quants(plot_quant_widths) %>%
  filter(date > forecast_start_date - ddays(days_before_fit), date <= forecast_start_date + ddays(days_horizon))

cases_known <- process_local_cases(local_cases_state, ascertainment_ts)

ward_quants <- clinical_quants_state %>%
  filter(group == "ward")
ward_known <- public_occupancy_data %>%
  filter(group == "ward", state == i_state)

ICU_quants <- clinical_quants_state %>%
  filter(group == "ICU")
ICU_known <- public_occupancy_data %>%
  filter(group == "ICU", state == i_state)



if(is_longterm) {
  ward_rep <- clinical_trajectories_wide_state %>% 
    filter(group == "ward") %>% mutate(state = 1, .model = 1) %>%
    get_quants("max", probs = seq(0.1, 0.9, by = 0.05)) %>%
    filter(date >= forecast_start_date + ddays(7))
  ICU_rep <- clinical_trajectories_wide_state %>% 
    filter(group == "ICU") %>% mutate(state = 1, .model = 1) %>%
    get_quants("max", probs = seq(0.1, 0.9, by = 0.05)) %>%
    filter(date >= forecast_start_date + ddays(7))
  
  ensemble_smooth <- bind_rows(
    case_ensemble_state %>% 
      filter(.model != "moss"),
    case_ensemble_state %>% 
      filter(.model == "moss") %>%
      mutate(across(starts_with("sim"), ~ slider::slide_index_dbl(., .i = date, .f = mean, .before = 7, .after =0)))
  )
  
  ensemble_rep <- get_quants(ensemble_smooth, "max", probs = seq(0.1, 0.9, by = 0.05))
}

# Important - don't plot capacity limits where they're well out of range of what's observed or predicted
state_capacity_limits <- capacity_limits_tbl %>%
  filter(state == i_state,
         case_when(group == "ward" ~ capacity <= max(c(ward_known$count * 1.5, ward_quants$upper * 1.5)),
                   group == "ICU" ~ capacity <= max(c(ICU_known$count * 1.5, ICU_quants$upper * 1.5)))) %>%
  
  mutate(padded_capacity = str_pad(capacity, max(str_length(as.character(capacity))), side = "r"),
         label = padded_capacity)


plots_common <- list(
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%b")),
               expand = expansion(mult = c(0.01, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0.1)),
                     sec.axis = dup_axis(name = "")),
  geom_blank(aes(y = 0)), geom_blank(aes(y = 30)),
  xlab(NULL), ylab("Count"),
  theme_minimal(),
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dotted"),
        axis.ticks = element_line(colour = "grey60"),
        axis.ticks.length = unit(5, "pt"),
        axis.line = element_line(colour = "grey40"),
        plot.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        text = element_text(family = "Helvetica")),
  
  coord_cartesian(xlim = c(date_plot_start, forecast_start_date + ddays(days_horizon) - ddays(4) + ddays(7)))
)

p_cases <- ggplot() +
  
  geom_vline(xintercept = date_reporting_line, colour = "grey60") +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
              
              data = ensemble_quants) +
  
  {
    if(is_longterm) 
      geom_line(aes(x = date, y = value / 0.75, group = name),
                color = rep_colour, size = rep_thinness,
                ensemble_rep %>% pivot_longer(-c(state, date)))
  } +
  
  
  geom_point(aes(x = date, y = count),
             cases_known %>% filter(detection_probability >= 0.95),
             color = "grey20",
             
             size = 1, stroke = 0.2) +
  
  geom_point(aes(x = date, y = count / detection_probability),
             cases_known %>% filter(detection_probability <= 0.99),
             color = 'black',
             
             size = 0.6, stroke = 0.2) +
  
  # Currently smaller than the points themselves!
  geom_linerange(aes(x = date, ymin = lower90, ymax = upper90),
                 cases_known %>% filter(detection_probability <= 0.99)) +
  
  scale_fill_manual(values = plot_cols$case_or_inf) +
  
  
  geom_vline(xintercept = forecast_start_date + ddays(1),
             colour = "grey40", linetype = "dashed") +
  
  ggtitle(str_c("Case incidence")) +
  
  plots_common

p_ward <- ggplot() +
  
  geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
              
              data = ward_quants) +
  
  {
    if(is_longterm) 
      geom_line(aes(x = date, y = value, group = name),
                color = rep_colour, size = rep_thinness,
                ward_rep %>% pivot_longer(-c(state, date)))
  } +
  
  geom_point(aes(x = date, y = count),
             color = "grey20",
             ward_known %>% filter(date <= forecast_start_date + ddays(7)),
             
             size = 1, stroke = 0.2) +
  
  scale_fill_manual(values = plot_cols$ward) +
  
  geom_vline(xintercept = forecast_start_date + ddays(7),
             colour = "grey40", linetype = "dashed") +
  
  {
    if(show_capacity)
      geom_hline(aes(yintercept = capacity),
                 state_capacity_limits %>% filter(group == "ward"),
                 linetype = 'dashed', size = 0.3)
  } +
  {
    if(show_capacity)
      geom_label(aes(x = date_plot_start + ddays(7), y = capacity, label = capacity),
                 hjust = 0, vjust = 0.5, label.r = unit(0, "cm"), label.size = 0,
                 state_capacity_limits %>% filter(group == "ward"))
  } +
  
  ggtitle(str_c("Ward bed occupancy")) +
  
  plots_common

p_ICU <- ggplot() +
  
  geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
              
              data = ICU_quants) +
  
  {
    if(is_longterm) 
      geom_line(aes(x = date, y = value, group = name),
                color = rep_colour, size = rep_thinness,
                ICU_rep %>% pivot_longer(-c(state, date)))
  } +
  
  
  geom_point(aes(x = date, y = count),
             color = "grey20",
             ICU_known %>% filter(date <= forecast_start_date + ddays(7)),
             
             size = 1, stroke = 0.2) +
  
  
  scale_fill_manual(values = plot_cols$ICU) +
  
  geom_vline(xintercept = forecast_start_date + ddays(7),
             colour = "grey40", linetype = "dashed") +
  
  {
    if(show_capacity)
      geom_hline(aes(yintercept = capacity),
                 state_capacity_limits %>% filter(group == "ICU"),
                 linetype = 'dashed', size = 0.3)
  } +
  {
    if(show_capacity)
      geom_label(aes(x = date_plot_start + ddays(7), y = capacity, label = capacity),
                 hjust = 0, vjust = 0.5, label.r = unit(0, "cm"), label.size = 0,
                 state_capacity_limits %>% filter(group == "ICU"))
  } +
  
  ggtitle(str_c("ICU bed occupancy")) +
  
  plots_common


p <- cowplot::plot_grid(
  p_cases, p_ward, p_ICU,
  ncol = 1, align = "v", axis = "lr"
)

p

