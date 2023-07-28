
library(tidyverse)
library(lubridate)
library(targets)

source("R/_situational_awareness_functions.R")

## --------------- CHANGE THIS (if you want to) ---------------------
# These may be different from what is defined in _targets.R

# Paths of data and results to plot
results_dir <- "results/fc_2023-07-28_final/"
local_cases_path <- "~/mfluxunimelb/local_cases_input/local_cases_input_2023-07-27.csv"
ensemble_path <- "~/mfluxshared/forecast-outputs/combined_samples_varasc2023-07-21.csv"
date_reporting_line <- ymd("2023-07-28")


# When our plots go back to
date_plot_start <- ymd("2023-02-01")
ensemble_models_included <- c("gar", "moss_varasc_unsmoothed", "moss_varasc", "dst_new", "dst_behave")


days_horizon <- 7 * 4
days_before_fit <- 0


capacity_limits_tbl <- get_current_capacity_tbl(multipliers = 1:2)
occupancy_data <- tar_read(occupancy_data) %>%
  filter(date >= date_plot_start - ddays(14))

clinical_trajectories_wide <- get_trajectories(results_dir) %>%
  get_clinical_trajectories_wide()

case_ensemble_wide <- read_ensemble_all_states(ensemble_path, ensemble_models_included) %>%
  get_ensemble_wide()


local_cases <- read_csv(local_cases_path, show_col_types = FALSE) %>% 
  rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))





# Define our nice colour gradients (via David Price magic)
plot_cols <- get_plot_colors(length(plot_quant_widths), FALSE)


# If a state is excluded from plotting, can change this
states <- get_states(clinical_trajectories_wide)

results_long_plots <- list()
results_short_plots <- list()

max_forecast_start_date <- get_forecast_start_date(local_cases, pr_detect = 0.95)

for(i_state in states) {
  clinical_trajectories_wide_state <- clinical_trajectories_wide %>%
    filter(state == i_state)
  
  state_forecast_start_date <- get_forecast_start_date(local_cases %>% filter(state == i_state), pr_detect = 0.95)
  
  
  case_ensemble_wide_state <- case_ensemble_wide %>%
    filter(state == i_state) %>% 
    filter(date <= state_forecast_start_date + ddays(days_horizon))
  
  na_cols <- map_lgl(1:ncol(case_ensemble_wide_state), ~ all(is.na(case_ensemble_wide_state[,.])))
  print(paste0("Dropping ", sum(na_cols), " columns for being entirely NA"))
  case_ensemble_wide_state <- case_ensemble_wide_state[, !na_cols]
  
  
  long_plots <- make_plots(
    i_state = i_state,  
    
    case_ensemble_wide_state = case_ensemble_wide_state,
    clinical_trajectories_wide_state = clinical_trajectories_wide_state,
    local_cases = local_cases,
    occupancy_data = occupancy_data,
    
    
    state_forecast_start_date = state_forecast_start_date,
    date_plot_start = date_plot_start,
    date_plot_end = max_forecast_start_date + days(28),
    
    plots_common = plots_common_long,
    
    days_before_fit = days_before_fit,
    days_horizon = days_horizon,
    plot_quant_widths = plot_quant_widths
  )
  
  results_long_plots <- c(results_long_plots, list(long_plots))
  
  ggsave(str_c(results_dir, "_sitawareness_state_plot_", i_state, ".png" ),
         plot = long_plots$p,
         height = 9, width = 8.5, bg = "white")
  
  
  short_plots <- make_plots(
    i_state = i_state,  
    
    case_ensemble_wide_state = case_ensemble_wide_state,
    clinical_trajectories_wide_state = clinical_trajectories_wide_state,
    local_cases = local_cases,
    occupancy_data = occupancy_data,
    
    
    state_forecast_start_date = state_forecast_start_date,
    date_plot_start = max_forecast_start_date - days(7 * 8),
    date_plot_end = max_forecast_start_date + days(28),
    
    plots_common = plots_common_short,
    
    days_before_fit = days_before_fit,
    days_horizon = days_horizon,
    plot_quant_widths = plot_quant_widths
  )
  results_short_plots <- c(results_short_plots, list(short_plots))
  
}

p_short_ward <- make_short_plot_grid(results_short_plots, "p_ward")
ggsave(str_c(results_dir, "_sitawareness_combined_ward.png"),
       p_short_ward,
       height = 9, width = 8.5, bg = "white")



p_short_ICU <- make_short_plot_grid(results_short_plots, "p_ICU")
ggsave(str_c(results_dir, "_sitawareness_combined_ICU.png" ),
       p_short_ICU,
       height = 9, width = 8.5, bg = "white")


cairo_pdf(str_c(results_dir, "_sitawareness_combined.pdf"),
          width = 8.5, height = 9, onefile = TRUE)
plot(p_short_ward)
plot(p_short_ICU)
for (i in 1:length(results_long_plots)) {
  plot(results_long_plots[[i]]$p)
}
dev.off()

zip::zip(
  zipfile = str_c(results_dir, "_sitawareness_plots_", date_reporting_line, ".zip"),
  files = c(
    str_c(results_dir, "_sitawareness_state_plot_", states, ".png"),
    str_c(results_dir, "_sitawareness_combined_ward.png"),
    str_c(results_dir, "_sitawareness_combined_ICU.png")
  ),
  mode = "cherry-pick"
)


