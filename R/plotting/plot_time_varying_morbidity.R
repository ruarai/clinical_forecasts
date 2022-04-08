
plot_morbidity_trajectories <- function(
  morbidity_trajectories,
  state_modelled,
  
  plot_dir
) {
  line_plot <- morbidity_trajectories %>%
    pivot_longer(starts_with("pr_"))
  
  quants_plot <- line_plot %>%
    group_by(date, age_group, name) %>%
    summarise(q95_lower = quantile(value, 0.025),
           q95_upper = quantile(value, 0.975),
           q50_lower = quantile(value, 0.25),
           q50_upper = quantile(value, 0.75),
           median = median(value))
  
  p <- ggplot(quants_plot %>% filter(date >= ymd("2021-12-15"))) +
    
    geom_line(aes(x = date, y = value, group = bootstrap, color = name),
              line_plot %>% filter(date >= ymd("2021-12-15")),
              size = 0.3, alpha = 0.1) +
    geom_ribbon(aes(x = date, ymin = q95_lower, ymax = q95_upper, fill = name),
                alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = q50_lower, ymax = q50_upper, fill = name),
                alpha = 0.75) +
    
    ggokabeito::scale_fill_okabe_ito() +
    ggokabeito::scale_color_okabe_ito() +
    
    #coord_cartesian(ylim = c(0, 0.1)) +
    
    facet_wrap(~age_group * name, ncol = 3, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
    
    theme_minimal() +
    theme(legend.position = "none") +
    
    ggtitle(state_modelled)
  
  path <- paste0(plot_dir, "/", state_modelled, "_morbidity_ests.png")
  
  ggsave(
    path, plot = p,
    width = 12, height = 10, bg = "white"
  )
  
  
  return(path)
}

