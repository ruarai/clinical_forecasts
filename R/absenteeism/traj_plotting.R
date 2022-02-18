plot_traj <- function(traj, do_sum = FALSE) {
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  plot_data <- tibble(
    count = traj,
    age_group = rep(age_groups, length.out = length(traj)),
    t = rep(1:(length(traj) / 9), each = 9) - 1
  ) %>%
    mutate(date = t + forecast_dates$simulation_start)
  
  if(!do_sum) {
    
    p <- ggplot(plot_data) +
      geom_line(aes(x = date, y = count, color = age_group)) +
      
      theme_minimal()
  }
  
  if(do_sum) {
    plot_data <- plot_data %>%
      group_by(date, t) %>%
      summarise(count = sum(count))
    
    p <- ggplot(plot_data) +
      geom_line(aes(x = date, y = count)) +
      
      theme_minimal()
  }
  
  return (p)
  
}


make_results_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .) %>%
    slice(rep(1:nrow(.), each = 5))
  
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    bind_cols(medians) %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}

plot_trajs <-  function(trajs, do_sum = FALSE) {
  
  
  
  age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  p_common <- list(
    scale_y_continuous(
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    )
  )
  
  if(!do_sum) {
    
    
    plot_data <- trajs %>%
      as_tibble() %>%
      rename_with(~ str_c("sim_", .)) %>%
      mutate(age_group =  rep(age_groups, length.out = nrow(trajs)),
             t = rep(1:(nrow(trajs) / 9), each = 9) - 1,
             date = t + forecast_dates$simulation_start) %>%
      
      make_results_quants()
    
    p <- ggplot(plot_data) +
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
      
      geom_line(aes(x = date, y = median), color = '#023858') +
      
      scale_fill_brewer(palette = "PuBu") +
      
      facet_wrap(~age_group) +
      
      p_common +
      
      theme_minimal()
  }
  
  if(do_sum) {
    plot_data <- trajs %>%
      as_tibble() %>%
      rename_with(~ str_c("sim_", .)) %>%
      mutate(age_group =  rep(age_groups, length.out = nrow(trajs)),
             t = rep(1:(nrow(trajs) / 9), each = 9) - 1,
             date = t + forecast_dates$simulation_start) %>%
      
      group_by(date) %>%
      summarise(across(starts_with("sim_"), ~ sum(., na.rm = TRUE))) %>%
      
      make_results_quants()
    
    p <- ggplot(plot_data) +
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
      
      scale_fill_brewer(palette = "PuBu") +
      
      #geom_line(aes(x = date, y = median), color = '#023858') +
      
      p_common +
      
      theme_minimal()
  }
  
  return (p)
  
}


plot_trajs_no_age <-  function(trajs) {
  
  p_common <- list(
    scale_y_continuous(
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    )
  )
  

  plot_data <- trajs %>%
    as_tibble(.name_repair = "universal") %>%
    rename_with(~ str_c("sim_", .)) %>%
    mutate(t = row_number(),
           date = t + forecast_dates$simulation_start) %>%
    
    make_results_quants()
  
  p <- ggplot(plot_data) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    scale_fill_brewer(palette = "PuBu") +
    
    p_common +
    
    theme_minimal()
  
  return (p)
  
}