
library(tidyverse)
library(lubridate)
library(targets)

source("R/_situational_awareness_functions.R")

## --------------- CHANGE THIS (if you want to) ---------------------
# These may be different from what is defined in _targets.R

# Paths of data and results to plot
results_dir <- "results/fc_2023-03-03_final/"
local_cases_path <- "data/local_cases_input_2023-03-02.csv"
ensemble_path <- "data/combined_samples_varasc2023-02-24.csv"
date_reporting_line <- ymd("2023-03-03")


# When our plots go back to
date_plot_start <- ymd("2022-09-01")

# Are we plotting long or short-term forecasts?
is_longterm <- FALSE


days_horizon <- if_else(is_longterm, 30 * 6 , 7 * 4)
days_before_fit <- 0
show_capacity <- TRUE


capacity_limits_tbl <- get_current_capacity_tbl(multipliers = 1:2)
public_occupancy_data <- tar_read(occupancy_data) %>%
  filter(date >= date_plot_start - ddays(14))

clinical_trajectories <- get_trajectories(results_dir)


local_cases <- read_csv(local_cases_path, show_col_types = FALSE) %>% 
  rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))


if(is_longterm) {
  plot_quant_widths <- c(0.5, 0.8, 0.9)
  ensemble_models_included <- c("moss", "dst")
} else{
  plot_quant_widths <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  ensemble_models_included <- c("gar", "moss_varasc_unsmoothed", "moss_varasc", "dst_new")
}


# Define our nice colour gradients (via David Price magic)
plot_cols <- get_plot_colors(length(plot_quant_widths), is_longterm)


# If a state is excluded from plotting, can change this
states <- get_states(clinical_trajectories)

plots <- list()
ward_plots <- list()
ICU_plots <- list()

for(i_state in states) {
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
  
  
  na_cols <- map_lgl(1:ncol(case_ensemble_wide), ~ all(is.na(case_ensemble_wide[,.])))
  
  print(paste0("Dropping ", sum(na_cols), " columns for being entirely NA"))
  
  
  case_ensemble_wide <- case_ensemble_wide[, !na_cols]
  
  
  ensemble_quants <- case_ensemble_wide %>%
    rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
    make_results_quants(plot_quant_widths) %>%
    drop_na(lower) %>%
    
    filter(date <= forecast_start_date + ddays(days_horizon))
  
  clinical_quants_state <- clinical_trajectories_wide_state %>%
    make_results_quants(plot_quant_widths) %>%
    filter(date > forecast_start_date - ddays(days_before_fit), date <= forecast_start_date + ddays(days_horizon))
  
  
  cases_known <- process_local_cases(local_cases_state) %>%
    filter(date >= date_plot_start)
  
  ward_quants <- clinical_quants_state %>%
    filter(group == "ward")
  ward_known <- public_occupancy_data %>%
    filter(group == "ward", state == i_state) %>%
    filter(date >= date_plot_start)
  
  ICU_quants <- clinical_quants_state %>%
    filter(group == "ICU")
  ICU_known <- public_occupancy_data %>%
    filter(group == "ICU", state == i_state) %>%
    filter(date >= date_plot_start)
  
  
  
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
           case_when(group == "ward" ~ capacity <= max(c(ward_known$count * 1.5, ward_quants$upper * 1.5), na.rm = TRUE),
                     group == "ICU" ~ capacity <= max(c(ICU_known$count * 1.5, ICU_quants$upper * 1.5), na.rm = TRUE))) %>%
    
    mutate(#padded_capacity = str_pad(capacity, max(str_length(as.character(capacity))), side = "r"),
           #label = padded_capacity,
           wave = "BA.4/5",
           label = if_else(multiplier == 1, str_c(wave, " peak"), str_c(multiplier, "x ", wave, " peak")),
           date = date_plot_start + days(7))
  
  
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
    
    coord_cartesian(xlim = c(date_plot_start, forecast_start_date + ddays(days_horizon) - ddays(4)))
  )
  
  plot_params <- list(
    "rep_colour" = "grey17",
    "rep_thinness" = 0.1,
    "reporting_line_colour" = "grey60",
    "point_size" = 0.5,
    "point_stroke" = 0.15,
    "point_colour" = "grey20"
  )
  
  rep_colour <- "grey17"
  rep_thinness <- 0.1
  
  p_cases <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = plot_params$reporting_line_colour) +
    
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
               color = plot_params$point_colour,
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
    geom_point(aes(x = date, y = count / detection_probability),
               cases_known %>% filter(detection_probability <= 0.99),
               color = 'black',
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
    # Currently smaller than the points themselves!
    geom_linerange(aes(x = date, ymin = lower90, ymax = upper90),
                   cases_known %>% filter(detection_probability <= 0.99)) +
    
    scale_fill_manual(values = plot_cols$case_or_inf) +
    
    
    geom_vline(xintercept = forecast_start_date + ddays(1),
               colour = "grey40", linetype = "dashed") +
    
    ggtitle(str_c(i_state, " \u2013 ", if_else(is_longterm, "Infection", "Case") , " incidence by onset date")) +
    
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
               color = plot_params$point_colour,
               ward_known,
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
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
        geom_label(aes(x = date, y = capacity, label = label),
                   hjust = 0, vjust = 0.5, label.r = unit(0, "cm"), label.size = 0,
                   state_capacity_limits %>% filter(group == "ward"))
    } +
    
    ggtitle(str_c(i_state, " \u2013 Ward bed occupancy")) +
    
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
               color = plot_params$point_colour,
               ICU_known,
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
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
        geom_label(aes(x = date, y = capacity, label = label),
                   hjust = 0, vjust = 0.5, label.r = unit(0, "cm"), label.size = 0,
                   state_capacity_limits %>% filter(group == "ICU"))
    } +
    
    ggtitle(str_c(i_state, " \u2013 ICU bed occupancy")) +
    
    plots_common
  
  
  p <- cowplot::plot_grid(
    p_cases, p_ward, p_ICU,
    ncol = 1, align = "v", axis = "lr"
  )
  
  p
  
  plots <- c(plots, list(p))
  ward_plots <- c(ward_plots, list(p_ward))
  ICU_plots <- c(ICU_plots, list(p_ICU))
  
  ggsave(str_c(results_dir, "_sitawareness_state_plot_", i_state, ".png" ),
         plot = p,
         height = 9, width = 8.5, bg = "white")
  
}


zip::zip(zipfile = str_c(results_dir, "_sitawareness_plots_", date_reporting_line, ".zip"),
         files = str_c(results_dir, "_sitawareness_state_plot_", states, ".png"),
         mode = "cherry-pick")

cairo_pdf(str_c(results_dir, "_sitawareness_combined.pdf"),
          width = 8.5, height = 9, onefile = TRUE)
for (i in 1:length(plots)) {
  plot(plots[[i]])
}
dev.off()



cowplot::plot_grid(
  plotlist = ward_plots,
  ncol = 2
)

ggsave(str_c(results_dir, "_sitawareness_ward.png" ),
       height = 11, width = 10, bg = "white")

cowplot::plot_grid(
  plotlist = ICU_plots,
  ncol = 2
)

ggsave(str_c(results_dir, "_sitawareness_ICU.png" ),
       height = 11, width = 10, bg = "white")



