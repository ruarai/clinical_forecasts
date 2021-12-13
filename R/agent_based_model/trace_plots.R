plot_trace_plots <- function(sim_results, simulation_options, forecast_date_lines) {
  
  p1 <- ggplot(sim_results$tbl_transitions %>%
                 filter(str_detect(ix, "-1-"))) +
    geom_line(aes(x = date, y = n, group = ix),
              size = 0.1) +
    
    facet_wrap(~new_comp, scales = "free_y", ncol = 1) +
    forecast_date_lines +
    
    theme_minimal() +
    ggtitle("Transitions into compartment",
            simulation_options$run_name) + xlab("")
  
  
  p2 <- ggplot(sim_results$tbl_count %>%
                 filter(str_detect(ix, "-1-"),
                        compartment %in% unique(sim_results$tbl_transitions$new_comp))) +
    geom_line(aes(x = date, y = count, group = ix),
              size = 0.1) +
    
    facet_wrap(~compartment, scales = "free_y", ncol = 1) +
    forecast_date_lines +
    
    theme_minimal() +
    ggtitle("Count in compartment") + xlab("")
  
  plot_p12 <- cowplot::plot_grid(p1, p2, ncol = 2)
  ggsave(paste0(simulation_options$dirs$plots, "/trace_plots.png"),
         height = 12, width = 8, bg = 'white')
  
  
  ggplot(sim_results$tbl_count_grouped %>%
           filter(str_detect(ix, "-1-"),
                  !is.na(group))) +
    geom_line(aes(x = date, y = count, group = ix),
              size = 0.1) +
    
    facet_wrap(~group, scales = "free_y", ncol = 3) +
    forecast_date_lines +
    
    theme_minimal() +
    
    ggtitle("Trace counts (by group)",
            simulation_options$run_name)
  
  ggsave(paste0(simulation_options$dirs$plots, "/trace_count_grouped.png"),
         height = 6, width = 8, bg = 'white')
  
  
  ggplot(sim_results$tbl_transitions %>%
           filter(new_comp == "symptomatic_clinical",
                  str_detect(ix, "-1-"))) +
    geom_line(aes(x = date, y = n, group = ix),
              size = 0.5) +
    forecast_date_lines +
    
    coord_cartesian(xlim = c(simulation_options$dates$last_onset_50 - 30,
                             simulation_options$dates$last_onset_50 + 28)) +
    
    theme_minimal() +
    ggtitle("Trace onset counts",
            simulation_options$run_name)
  
  ggsave(paste0(simulation_options$dirs$plots, "/trace_onset.png"),
         height = 6, width = 8, bg = 'white')
  
}