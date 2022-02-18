
plot_diagnostics <- function(
  case_trajectories,
  forecast_dates,
  nindss_state,
  clinical_table_state,
  plot_dir,
  
  state_modelled
) {
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  n_days <- case_trajectories$n_days
  t_start <- case_trajectories$step_sampling_start
  
  plot_backcast <- case_trajectories$curve_set[1:(t_start*9), 1] %>%
    as_tibble() %>%
    mutate(age_group = rep(age_groups, times = t_start),
           
           t = rep(1:t_start, each = 9),
           
           date = t + forecast_dates$simulation_start)
  
  
  p1_backcast <- ggplot(plot_backcast) +
    geom_line(aes(x = date, y = value, color = age_group)) +
    
    ggokabeito::scale_color_okabe_ito() +
    xlab(NULL) + ylab(NULL) +
    ggtitle(NULL, "Backcast (hospitalised cases by onset date)") +
    
    theme_minimal()
  
  
  p2_backcast <- ggplot(plot_backcast %>%
                          group_by(date) %>%
                          summarise(value = sum(value))) +
    geom_col(aes(x = date, y = value)) +
    
    xlab(NULL) + ylab(NULL) +
    ggtitle(NULL, "Backcast (total hospitalised cases by onset date)") +
    
    theme_minimal()
  
  
  
  
  
  plot_forecast <- case_trajectories$curve_set[(t_start * 9 + 1):(t_start * 9 + n_days - t_start), ] %>%
    as_tibble() %>%
    mutate(t = (t_start + 1):n_days,
           
           date = t + forecast_dates$simulation_start) %>%
    
    pivot_longer(starts_with("sim"),
                 names_to = c("sample", "model"),
                 names_sep = "_") %>%
    
    mutate(sample = as.numeric(str_remove(sample, "sim")))
  
  
  
  plot_fc <- function(i_model) {
    
    ggplot(plot_forecast %>% filter(model == i_model, sample %% 50 == 0)) +
      geom_line(aes(x = date, y = value, group = sample), size = 0.1, alpha = 0.5) +
      
      xlab(NULL) + ylab(NULL) +
      ggtitle(NULL, str_c("Case nowcast and forecast (", i_model, ")")) +
      
      geom_vline(xintercept = forecast_dates$forecast_start, linetype = 'dotted') +
      
      theme_minimal()
  }
  
  p_top <- cowplot::plot_grid(
    
    cowplot::plot_grid(p1_backcast, p2_backcast, ncol = 1, align = "v", axis = "lr"),
    
    cowplot::plot_grid(
      plot_fc("gar"), plot_fc("uoa"), plot_fc("moss"), plot_fc("dst"), ncol = 1
    ),
    ncol = 2
  )
  
  
  
  
  nindss_recent <- nindss_state %>%
    filter(date_onset <= forecast_dates$forecast_start,
           date_onset > forecast_dates$forecast_start - ddays(14))
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  recent_age_dist <- nindss_recent %>%
    group_by(age_group) %>%
    summarise(pr_age_given_case = n() / nrow(.)) %>%
    
    complete(age_group = age_groups, fill = list(pr_age_given_case = 0))
  
  
  p_pr_age_given_case <- ggplot(recent_age_dist) +
    geom_col(aes(x = age_group, y = pr_age_given_case, fill = age_group)) +
    
    ggokabeito::scale_fill_okabe_ito() +
    xlab(NULL) + ylab("P(age|case)") +
    coord_cartesian(ylim = c(0, 0.25)) + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
    
    theme_minimal() +
    theme(legend.position = "none")
  
  
  clinical_table_ordered <- clinical_table_state %>%
    filter(date_onset >= forecast_dates$simulation_start,
           date_onset <= forecast_dates$forecast_horizon) %>%
    arrange(date_onset, age_group)
  
  forecasting_clinical_table <- clinical_table_ordered %>%
    filter(date_onset == max(date_onset)) %>%
    select(age_group, pr_hosp, pr_ICU)
  
  p_pr_hosp_given_case <- ggplot(forecasting_clinical_table) +
    geom_col(aes(x = age_group, y = pr_hosp, fill = age_group)) +
    
    ggokabeito::scale_fill_okabe_ito() +
    xlab(NULL) + ylab("P(hosp|case)") +
    coord_cartesian(ylim = c(0, 0.25)) + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
    
    theme_minimal() +
    theme(legend.position = "none")
  
  p_pr_ICU_given_hosp <- ggplot(forecasting_clinical_table) +
    geom_col(aes(x = age_group, y = pr_ICU, fill = age_group)) +
    
    ggokabeito::scale_fill_okabe_ito() +
    xlab(NULL) + ylab("P(ICU|hosp)") +
    coord_cartesian(ylim = c(0, 0.25)) + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
    
    theme_minimal() +
    theme(legend.position = "none")
  
  
  p_bottom <- cowplot::plot_grid(
    p_pr_age_given_case, p_pr_hosp_given_case, p_pr_ICU_given_hosp,
    ncol = 3
  )
  
  
  cowplot::plot_grid(
    p_top,
    p_bottom,
    ncol = 1, rel_heights = c(3, 1)
  )
  
  ggsave(
    paste0(plot_dir, "/_", state_modelled, "_diagnostics.png"),
    bg = "white",
    height = 10, width = 12
  )
  
}
