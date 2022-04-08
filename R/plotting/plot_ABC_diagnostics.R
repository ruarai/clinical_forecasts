
plot_ABC_diagnostics <- function(
  all_state_ABC_diagnostics,
  all_state_ABC_parameters,
  
  plot_dir
) {
  
  cowplot::plot_grid(
    
    ggplot(all_state_ABC_diagnostics %>% filter(state %in% c("ACT", "NT", "SA", "VIC"))) +
      geom_col(aes(x = factor(thresholds), y = accepted)) +
      
      facet_wrap(~state, ncol = 1) +
      
      xlab("Threshold") + ylab("Number outputs accepted") +
      ggtitle(NULL, "ABC acceptance rate") +
      theme_minimal() +
      theme(legend.position = "none"),
    
    
    ggplot() +
      geom_point(aes(x = los_scale, y = pr_hosp_scale, color = source),
                 all_state_ABC_parameters %>% filter(source == "prior"),
                 size = 0.1) +
      geom_point(aes(x = los_scale, y = pr_hosp_scale, color = source),
                 all_state_ABC_parameters %>% filter(source == "posterior"),
                 size = 0.1) +
      ggtitle(NULL, "Scale parameters prior/posterior") +
      
      facet_wrap(~state, ncol = 2)  +
      
      xlab("L (sigma_los)") + ylab("H (sigma_pr_hosp)") +
      
      coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
      
      scale_color_manual(values = c("prior" = "grey50",
                                    "posterior" = ggokabeito::palette_okabe_ito(3))) +
      
      scale_y_continuous(sec.axis = sec_axis(trans = identity, name = "H (sigma_pr_hosp)")) +
      
      theme_minimal() +
      theme(legend.position = "none"),
    
    
    ggplot(all_state_ABC_diagnostics %>% filter(state %in% c("NSW", "QLD", "TAS", "WA"))) +
      geom_col(aes(x = factor(thresholds), y = accepted)) +
      
      facet_wrap(~state, ncol = 1) +
      scale_y_continuous(position = "right") +
      
      xlab("Threshold") + ylab("Number outputs accepted") +
      ggtitle(NULL, "ABC acceptance rate") +
      theme_minimal() +
      theme(legend.position = "none"),
    
    ncol = 3, rel_widths = c(1, 2, 1)
  )
  
  ggsave(
    paste0(plot_dir, "/_ABC_diagnostics.png"),
    bg = "white",
    width = 10, height = 7
  )
}

