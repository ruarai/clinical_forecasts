
obs_data <- list(
  "onset_to_ward" = data_LoS_onset_to_ward %>% mutate(onset_coding = "-"),
  
  "ward_to_discharge" = data_LoS_ward_to_discharge,
  "ward_to_death" = data_LoS_ward_to_death,
  "ward_to_ICU" = data_LoS_ward_to_ICU,
  
  "ICU_to_discharge" = data_LoS_ICU_to_discharge, 
  "ICU_to_death" = data_LoS_ICU_to_death,
  "ICU_to_postICU" = data_LoS_ICU_to_postICU,
  
  "postICU_to_death" = data_LoS_postICU_to_death,
  "postICU_to_discharge" = data_LoS_postICU_to_discharge)


wide_prob_table_full <- bind_rows(
  wide_prob_table %>%
    select(compartment, age_class, pr = prob),
  
  data_LoS_onset_to_ward %>%
    mutate(compartment = "onset_to_ward", pr = 1) %>%
    distinct(compartment, age_class, pr),
  
  make_prob_table(ward_modelling,
                  "ward_coding", death_age_breaks, death_age_groups) %>%
    filter(ward_coding != "ward_to_death") %>% 
    rename(compartment = ward_coding) %>% 
    group_by(age_class) %>% 
    summarise(pr = 1 - sum(prob)) %>% 
    mutate(compartment = "ward_to_death") %>%
    select(compartment, age_class, pr),
  
  make_prob_table(ICU_modelling,
                  "ICU_coding", death_age_breaks, death_age_groups) %>%
    filter(ICU_coding != "ICU_to_death") %>% 
    rename(compartment = ICU_coding) %>% 
    group_by(age_class) %>% 
    summarise(pr = 1 - sum(prob)) %>% 
    mutate(compartment = "ICU_to_death") %>%
    select(compartment, age_class, pr),
  
  make_prob_table(postICU_modelling,
                  "postICU_coding", ward_age_breaks, ward_age_groups) %>%
    filter(postICU_coding != "postICU_to_discharge") %>% 
    rename(compartment = postICU_coding) %>% 
    group_by(age_class) %>% 
    summarise(pr = 1 - sum(prob)) %>% 
    mutate(compartment = "postICU_to_discharge") %>%
    select(compartment, age_class, pr),
)

observed_data <- 1:length(obs_data) %>%
  map_dfr(function(i) {
    data <- obs_data[[i]]
    name <- names(obs_data)[i]
    
    name_part <- str_extract(name, ".+?(?=_)")
    
    LoS_column <- str_c(name_part, "_LoS")
    coding_column <- str_c(name_part, "_coding")
    
    print(LoS_column)
    print(coding_column)
    
    data  %>%
      rename(LoS = !!sym(LoS_column),
             compartment = !!sym(coding_column)) %>%
      mutate(censored = compartment == "censored",
             compartment = name) %>%
      select(LoS, compartment, censored, age_class)
  })

obs_data_ecdf <- observed_data %>%
  ungroup() %>%
  
  group_by(compartment, age_class) %>%
  mutate(y = ecdf(LoS[!str_detect(compartment, "censored")])(LoS)) %>%
  # Adding fake endpoints
  bind_rows(
    observed_data %>% distinct(compartment, age_class) %>% mutate(y = 0, LoS = 0),
    observed_data %>% distinct(compartment, age_class) %>% mutate(y = 1, LoS = 1000)
  )# %>%
  
  # left_join(wide_prob_table_full) %>%
  # mutate(y = pr * y)

LoS_fits <- list(
  "onset_to_ward" = LoS_onset_to_ward,
  
  "ward_to_discharge" = LoS_ward_to_discharge,
  "ward_to_death" = LoS_ward_to_death,
  "ward_to_ICU" = LoS_ward_to_ICU,
  
  "ICU_to_discharge" = LoS_ICU_to_discharge, 
  "ICU_to_death" = LoS_ICU_to_death,
  "ICU_to_postICU" = LoS_ICU_to_postICU,
  
  "postICU_to_death" =LoS_postICU_to_death,
  "postICU_to_discharge" = LoS_postICU_to_discharge
)

fit_plots <- 1:length(LoS_fits) %>% map_dfr(function(i) {
  LoS_fit <- LoS_fits[[i]]
  
  age_classes <- observed_data %>%
    filter(compartment == names(LoS_fits)[i]) %>%
    pull(age_class) %>% unique()
  
  
  fit_data <- tibble(age_class = age_classes)
  
  fit_summary <- summary(LoS_fit, type = 'survival',
                         newdata = fit_data,
                         t = seq(0, 40, by = 0.25))
  
  fit_summary_tbl <- fit_summary %>%
    map_dfr(identity, .id = "age_class") %>%
    
    mutate(age_class = str_remove(age_class, "age_class="),
           compartment = names(LoS_fits)[i])
  
  fit_summary_tbl
}) #%>%
  
  # left_join(wide_prob_table_full) %>%
  # mutate(lcl = 1 - (1 - lcl) * pr,
  #        ucl = 1 - (1 - ucl) * pr,
  #        est = 1 - (1 - est) * pr)




comp_names <- names(obs_data)

p_list <- map(comp_names, function(i_comp) {
  
  filter_comp <- . %>% filter(compartment == i_comp)
  
  ggplot() +
    # 
    # geom_hline(aes(yintercept = 1 - pr),
    #            wide_prob_table_full %>% filter_comp,
    #            color = '#009E73',
    #            linetype = 'dotted') +
    
    geom_line(aes(x = time, y =  est),
              color = '#009E73',
              data = fit_plots %>% filter_comp) +
    
    geom_ribbon(aes(x = time, ymin = lcl, ymax = ucl),
                fill = "#009E73",
                alpha = 0.25,
                data = fit_plots %>% filter_comp) +
    
    geom_step(aes(x = LoS, y = 1 - y),
              obs_data_ecdf %>% filter_comp) +
    
    geom_point(aes(x = LoS, y = 1 - y),
               pch = 4,
               obs_data_ecdf %>% filter(censored) %>% filter_comp) +
    
    geom_blank(aes(y = pmin(1,(pr * 1.2))),
               wide_prob_table_full %>% filter_comp) +
    
    coord_cartesian(xlim = c(0, 40)) +
    
    xlab("Time (days)") + ylab("Probability") +
    
    facet_wrap(~age_class, scale = if_else(i_comp == "ward_to_death" , "free_y",
                                           "fixed")) +
    
    ggtitle(i_comp) +
    
    theme_minimal() +
    theme(plot.title = element_text(face = 'plain',
                                    size = 12),
          axis.title.x = element_blank())
  
})

plot_legend <- (ggplot(tibble(b = 1:2,
              c = 3:4)) +
  geom_rect(aes(xmin = 0, xmax = b, ymin = 0, ymax = c, fill = 'est')) +
  geom_line(aes(x = b, y = c, color = 'data')) +
  geom_point(aes(x = b, y = c, pch = 'cens')) +
  
  scale_fill_manual("",
                    values = c("est" = "#bfe7dc"),
                    labels = c("est" = "Estimate median and 95% CI")) +
  
  scale_color_manual("",
                     values = c("data" = "black"),
                     labels = c("data" = "Observed data")) +
  
  scale_shape_manual("",
                     values = c("cens" = 4),
                     labels = c("cens" = "Observed data (censored)")) +
  
  theme_minimal() +
  theme(legend.position = 'bottom')) %>%
  cowplot::get_legend()


cowplot::plot_grid(
  cowplot::plot_grid(plotlist = p_list[1:5], ncol = 1),
  plot_legend,
  rel_heights = c(20, 1), ncol = 1
)


ggsave(paste0(output_dir, "/survplots_upd_1.png"),
       bg = 'white',
       width = 8,
       height = 12)

cowplot::plot_grid(
  cowplot::plot_grid(plotlist = p_list[6:9], ncol = 1),
  plot_legend,
  rel_heights = c(20, 1), ncol = 1
)


ggsave(paste0(output_dir, "/survplots_upd_2.png"),
       bg = 'white',
       width = 8,
       height = 10)



p_list[1]
