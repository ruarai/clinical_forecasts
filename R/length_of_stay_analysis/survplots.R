

library(survminer)

age_color_scale <- scale_color_manual(
  "Age grouping ",
  values = c("age_class=0-69" = "#E69F00",
             "age_class=70+" = "#009E73",
             "age_class=0-39" = "#0072B2",
             "age_class=40-69" = "#CC79A7"),
  labels = c("age_class=0-69" = "0-69",
             "age_class=0-39" = "0-39",
             "age_class=40-69" = "40-69",
             "age_class=70+" = "70+")
  )

plot_additions <- list(
  scale_x_continuous(breaks = seq(0, 40, by = 5)),
  coord_cartesian(xlim = c(0, 40)),
  age_color_scale,
  ylab("Surv. prob."),
  xlab("Days"),
  theme(legend.position = 'none')
)

p0 <- ggflexsurvplot(LoS_onset_to_ward, xlim = c(0,30))$plot + ggtitle("symptomatic_to_ED") +
  plot_additions

p1 <- ggflexsurvplot(LoS_ward_to_discharge, xlim = c(0,30))$plot + ggtitle("ward_to_discharge") +
  plot_additions


p2 <- ggflexsurvplot(LoS_ward_to_death, xlim = c(0,30))$plot + ggtitle("ward_to_death") +
  plot_additions
p3 <- ggflexsurvplot(LoS_ward_to_ICU, xlim = c(0,30))$plot + ggtitle("ward_to_ICU") +
  plot_additions



p4 <- ggflexsurvplot(LoS_ICU_to_death, xlim = c(0,40))$plot + ggtitle("ICU_to_death") +
  plot_additions
p5 <- ggflexsurvplot(LoS_ICU_to_postICU, xlim = c(0,40))$plot + ggtitle("ICU_to_postICU") +
  plot_additions
p6 <- ggflexsurvplot(LoS_ICU_to_discharge, xlim = c(0,40))$plot + ggtitle("ICU_to_discharge") +
  plot_additions


p7 <- ggflexsurvplot(LoS_postICU_to_death, xlim = c(0,40))$plot + ggtitle("postICU_to_death") +
  plot_additions
p8 <- ggflexsurvplot(LoS_postICU_to_discharge, xlim = c(0,40))$plot + ggtitle("postICU_to_discharge") +
  plot_additions


fake_plot <- tibble(
  age_class = c("0-39", "40-69", "70+", "0-69"),
  x = 1:4, y = 5:8
) %>% ggplot() + 
  geom_line(aes(x = x, y = y, color = age_class, group = age_class)) +
  age_color_scale +
  theme_minimal() +
  theme(legend.position = 'bottom')




p_tall <- cowplot::plot_grid(p0,
                             p1,p2,p3,p5,
                             p6,p4,p7,p8,
                            
                            nrow = 5, ncol = 2)

p_tall_legend <- cowplot::plot_grid(p_tall, 
                                    cowplot::get_legend(fake_plot),
                                    nrow = 2,
                                    rel_heights = c(0.97, 0.03))

ggsave(paste0(output_dir, "/survplots_tall.png"), p_tall_legend,
       bg = 'white',
       width = 11,
       height = 15)
