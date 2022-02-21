
library(targets)
library(tidyverse)
library(lubridate)

past_forecasts <- c(
  "fc_2022-01-05_retro",
  "fc_2022-01-13_retro",
  "fc_2022-01-21_retro",
  "fc_2022-01-28_retro",
  "fc_2022-02-04_retro",
  "fc_2022-02-11_final"
)



past_forecast_dirs <- str_c(
  "results/", past_forecasts, "/"
)

past_forecast_trajs <- str_c(
  past_forecast_dirs, "trajectories.fst"
) %>%
  `names<-`(past_forecasts)

past_forecast_datefiles <- str_c(
  past_forecast_dirs, "forecast_dates.csv"
) %>%
  `names<-`(past_forecasts)


make_results_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
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
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}

parse_forecast <- function(x) {
  x %>%
    select(c(state, date, group, sample, count)) %>%
    pivot_wider(names_from = "sample",
                names_prefix = "sim_",
                values_from = "count") %>%
    make_results_quants()
}


traj_data <- map_dfr(
  past_forecast_trajs,
  function(x){ 
    print(x)
    parse_forecast(fst::read_fst(x))
  }, .id = "source"
)

fc_dates <- map_dfr(
  past_forecast_datefiles, ~ read_csv(., show_col_types = FALSE), .id = "source"
)


plot_data <- traj_data %>%
  left_join(fc_dates %>% select(source, forecast_start)) %>%
  
  filter(date >= forecast_start - ddays(7)) %>%
  
  mutate(lower = pmin(lower, 10000),
         upper = pmin(upper, 10000))



known_occupancy <- tar_read(all_state_known_occupancy_ts) %>%
  filter(date >= min(plot_data$date)) %>%
  rename(ts_source = source)


states <- unique(plot_data$state)
states <- states[order(states)]

state_plots <- map(
  states,
  function(i_state) {
    
    filt_plot <- . %>%
      filter(state == i_state) %>%
      mutate(group = factor(group, levels = c("ward", "ICU")))
    
    ggplot(plot_data %>% filt_plot) +
      
      geom_line(aes(x = date, y = count, group = ts_source),
                known_occupancy %>% filt_plot %>% filter(ts_source == "c19")) +
      
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = group), 
                  alpha = 0.2) +
      
      geom_vline(aes(xintercept = forecast_start), fc_dates) +
      
      facet_wrap(~source * group, ncol = 2,
                 scales = "free_y") +
      
      scale_fill_manual(values = c("ward" = "green4", "ICU" = "purple2")) +
      
      ggtitle(paste0(i_state, " - Retrospective clinical forecast performance")) +
      xlab(NULL) + ylab(NULL) +
      
      scale_y_continuous(breaks = scales::breaks_extended(), labels = scales::label_comma()) +
      
      theme_minimal() +
      
      theme(legend.position = "none")
  }
)




pdf("results/retrospectives/retro_2022-02-21.pdf",
    width = 8, height = 10)
for (i in 1:length(state_plots)){
  plot(state_plots[[i]])
}
dev.off()




