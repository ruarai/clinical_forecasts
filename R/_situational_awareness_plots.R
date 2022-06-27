
library(tidyverse)
library(lubridate)
library(targets)

## --------------- CHANGE THIS (if you want to) ---------------------
# If models to be included in the ensemble for the case forecast plot needs to change
# This can be different from what is used for the clinical forecasting (which is defined in _targets.R)
ensemble_models_included <- c("gar", "moss", "dst")

# When our plots go back to
date_plot_start <- ymd("2021-12-01")

is_longterm <- TRUE
days_horizon <- if_else(is_longterm, 30 * 6, 7 * 4)
days_before_fit <- 60


# Load clinical forecasting data
all_state_quants <- tar_read(all_state_quants)
forecast_starts <- tar_read(forecast_starts)
forecast_dates <- tar_read(forecast_dates)
all_state_known_occupancy_ts <- tar_read(all_state_known_occupancy_ts)
plot_dir <- tar_read(plot_dir)
ensemble_path <- tar_read(raw_ensemble)
date_reporting_line <- tar_read(date_reporting_line)
date_forecasting <- tar_read(date_forecasting)

source("R/make_result_quants.R")
source("R/ensemble.R")


# Load in the capacity tables
source("R/capacity_table.R")
capacity_limits_tbl <- capacity_limits %>%
  map_dfr(function(x) tibble_row(capacity_ward = x$ward, capacity_ICU = x$ICU),
          .id = "state") %>%
  pivot_longer(cols = c(capacity_ward, capacity_ICU),
               names_prefix = "capacity_",
               names_to = "group", values_to = "capacity")

# NegBin observation model to account for reporting delays (for NG's linelist)
true_count_quantile <- function(observed_count, probability, quantiles = c(0.05, 0.95)) {
  observed_count + qnbinom(quantiles, observed_count + 1, probability)
}

# Define our nice colour gradients (via David Price magic)

alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))

case_base_colour <- "#006699"
ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"


case_cols <- shades::opacity(case_base_colour, alpha_vals)
ward_cols <- shades::opacity(ward_base_colour, alpha_vals)
ICU_cols <- shades::opacity(ICU_base_colour, alpha_vals)


# If a state is excluded from plotting, can change this
states <- unique(forecast_starts$state)
states <- states[order(states)]

plots <- list()

for(i_state in states) {
  
  local_cases <- tar_read_raw(str_c("local_cases_state_", i_state))
  
  ensemble_wide <- read_ensemble_state(ensemble_path, i_state, ensemble_models_included) %>%
    select(date, .model, starts_with("sim")) %>%
    
    pivot_wider(names_from = ".model",
                values_from = starts_with("sim"))
  
  
  cases_known <- local_cases %>%
    select(date = date_onset, count, detection_probability) %>%
    filter(detection_probability > 0.95) %>%
    
    mutate(lower90 = true_count_quantile(count, detection_probability, quantiles = 0.05),
           upper90 = true_count_quantile(count, detection_probability, quantiles = 0.95))
  
  forecast_start_date <- forecast_starts %>%
    filter(state == i_state) %>% pull(date)
  
  
  ensemble_quants <- ensemble_wide %>%
    rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
    make_results_quants() %>%
    drop_na(lower) %>%
    
    filter(date <= forecast_start_date + ddays(days_horizon))
  

  ward_quants <- all_state_quants %>%
    filter(group == "ward", state == i_state, date > forecast_start_date - ddays(days_before_fit))
  
  ward_known <- all_state_known_occupancy_ts %>%
    filter(group == "ward", state == i_state, date >= date_plot_start - ddays(30)) %>%
    
    mutate(do_match = date > forecast_start_date & date <= forecast_start_date + ddays(7))
  
  
  ICU_quants <- all_state_quants %>%
    filter(group == "ICU", state == i_state, date > forecast_start_date - ddays(days_before_fit))
  
  ICU_known <- all_state_known_occupancy_ts %>%
    filter(group == "ICU", state == i_state, date >= date_plot_start - ddays(30)) %>%
    
    mutate(do_match = date > forecast_start_date& date <= forecast_start_date + ddays(7))
  
  # Important - don't plot capacity limits where they're well out of range of what's observed or predicted
  state_capacity_limits <- capacity_limits_tbl %>%
    filter(state == i_state,
           case_when(group == "ward" ~ capacity <= max(c(ward_known$count * 1.5, ward_quants$upper * 1.5)),
                     group == "ICU" ~ capacity <= max(c(ICU_known$count * 1.5, ICU_quants$upper * 1.5))) )
  
  
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
    
    coord_cartesian(xlim = c(date_plot_start, forecast_start_date + ddays(days_horizon)))
  )
  
  
  
  
  p <- cowplot::plot_grid(
    
    
    ggplot() +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey60") +
    
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                  
                  data = ensemble_quants) +
      
      geom_point(aes(x = date, y = count),
                 cases_known %>% filter(detection_probability >= 0.95),
                 color = "grey20",
                 
                 size = 0.6, stroke = 0.2) +
      
      geom_point(aes(x = date, y = count / detection_probability),
                 cases_known %>% filter(detection_probability <= 0.99),
                 color = 'black',
                 
                 size = 0.6, stroke = 0.2) +
      
      # Currently smaller than the points themselves!
      geom_linerange(aes(x = date, ymin = lower90, ymax = upper90),
                     cases_known %>% filter(detection_probability <= 0.99),
                     ) +
      
      scale_fill_manual(values = case_cols) +
      
      
      geom_vline(xintercept = forecast_start_date + ddays(1),
                 colour = "grey40", linetype = "dashed") +
      
      ggtitle(str_c(i_state, " \u2013 Case incidence by onset date")) +
      
      plots_common,
    
    ggplot() +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                  
                  data = ward_quants) +
      
      geom_point(aes(x = date, y = count),
                 color = "grey20",
                 ward_known,
                 
                 size = 0.6, stroke = 0.2) +
      
      scale_fill_manual(values = ward_cols) +
      
      geom_vline(xintercept = forecast_start_date + ddays(7),
                 colour = "grey40", linetype = "dashed") +
      
      geom_hline(aes(yintercept = capacity),
                 state_capacity_limits %>% filter(group == "ward"),
                 linetype = 'dashed', size = 0.3) +
      
      ggtitle(str_c(i_state, " \u2013 Ward bed occupancy")) +
      
      plots_common,
    
    
    
    ggplot() +
      
      geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
      
      geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                  
                  data = ICU_quants) +
      
      
      geom_point(aes(x = date, y = count),
                 color = "grey20",
                 ICU_known,
                 
                 size = 0.6, stroke = 0.2) +
      
      scale_fill_manual(values = ICU_cols) +
      
      geom_vline(xintercept = forecast_start_date + ddays(7),
                 colour = "grey40", linetype = "dashed") +
      
      geom_hline(aes(yintercept = capacity),
                 state_capacity_limits %>% filter(group == "ICU"),
                 linetype = 'dashed', size = 0.3) +
      
      ggtitle(str_c(i_state, " \u2013 ICU bed occupancy")) +
      
      plots_common,
    
    ncol = 1, align = "v", axis = "lr"
  )
  
  p
  
  plots <- c(plots, list(p))
  
  ggsave(str_c(plot_dir, "_sitawareness_state_plot_", i_state, ".png" ),
         plot = p,
         height = 9, width = 8.5, bg = "white")
  
}


zip::zip(zipfile = str_c(plot_dir, "_sitawareness_plots_", date_forecasting, ".zip"),
         files = str_c(plot_dir, "_sitawareness_state_plot_", states, ".png"),
         mode = "cherry-pick")

cairo_pdf(str_c(plot_dir, "_sitawareness_combined.pdf"),
          width = 8.5, height = 9, onefile = TRUE)
for (i in 1:length(plots)){
  plot(plots[[i]])
}
dev.off()

