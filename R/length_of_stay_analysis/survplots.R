

library(survminer)

plot_additions <- list(
  scale_x_continuous(breaks = seq(0, 40, by = 5)),
  coord_cartesian(xlim = c(0, 40))
)

p1 <- ggflexsurvplot(ward_LoS_discharged, xlim = c(0,30))$plot + ggtitle("ward_to_discharge") +
  geom_vline(xintercept = 14, alpha = 0.5, linetype = 'dashed') +
  plot_additions


p2 <- ggflexsurvplot(ward_LoS_died, xlim = c(0,30))$plot + ggtitle("ward_to_death") +
  plot_additions
p3 <- ggflexsurvplot(ward_LoS_ICU, xlim = c(0,30))$plot + ggtitle("ward_to_ICU") +
  plot_additions



p4 <- ggflexsurvplot(ICU_LoS_death, xlim = c(0,40))$plot + ggtitle("ICU_to_death") +
  plot_additions
p5 <- ggflexsurvplot(ICU_LoS_postICU, xlim = c(0,40))$plot + ggtitle("ICU_to_postICU") +
  plot_additions
p6 <- ggflexsurvplot(ICU_LoS_discharge, xlim = c(0,40))$plot + ggtitle("ICU_to_discharge") +
  plot_additions


p7 <- ggflexsurvplot(postICU_LoS_death, xlim = c(0,40))$plot + ggtitle("postICU_to_death") +
  plot_additions
p8 <- ggflexsurvplot(postICU_LoS_discharge, xlim = c(0,40))$plot + ggtitle("postICU_to_discharge") +
  plot_additions


p_all <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,
                   
                   nrow = 2, ncol = 4)


ggsave(paste0(output_dir, "/survplots.png"), p_all,
       width = 24,
       height = 12)
