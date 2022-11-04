
library(tidyverse)
library(lubridate)
library(targets)

source("R/_situational_awareness_functions.R")

## --------------- CHANGE THIS (if you want to) ---------------------
# These may be different from what is defined in _targets.R

# Paths of data and results to plot
results_dir <- "results/fc_2022-10-28_final/"
local_cases_path <- "~/mfluxunimelb/local_cases_input/local_cases_input_2022-07-13.csv"

date_reporting_line <- NA_Date_

i_state <- "NSW"

# When our plots go back to
date_plot_start <- ymd("2022-06-01")

# Are we plotting long or short-term forecasts?
is_longterm <- TRUE
plot_quant_widths <- c(0.2, 0.5)
ensemble_models_included <- c("moss")


days_horizon <- if_else(is_longterm, 30 * 6 , 7 * 4)
days_before_fit <- 0
show_capacity <- TRUE


capacity_limits_tbl <- get_current_capacity_tbl(multipliers = 1:2)
ascertainment_ts <- get_ascertainment_ts()
public_occupancy_data <- tar_read(c19data) %>%
  filter(date >= date_plot_start - ddays(14))

clinical_trajectories <- get_trajectories(results_dir)


local_cases <- read_csv(local_cases_path, show_col_types = FALSE) %>% 
  rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))




# Define our nice colour gradients (via David Price magic)
plot_cols <- get_plot_colors(length(plot_quant_widths), is_longterm)


# If a state is excluded from plotting, can change this
states <- get_states(clinical_trajectories)

plots <- list()
ward_plots <- list()
ICU_plots <- list()



case_ensemble_state <- read_ensemble_state("~/mfluxshared/forecast-outputs/combined_samples_50asc2022-07-05.csv",
                                           i_state, ensemble_models_included) %>%
  mutate(asc = "50%") %>% 
  
  bind_rows(
    read_ensemble_state("~/mfluxshared/forecast-outputs/combined_samples_75asc2022-07-05.csv",
                        i_state, ensemble_models_included) %>%
      mutate(asc = "75%")
  )
local_cases_state <- local_cases %>%
  filter(state == i_state)

forecast_start_date <- get_forecast_start_date(local_cases_state, pr_detect = 0.95)
case_ensemble_state <- case_ensemble_state %>%
  filter(date <= forecast_start_date + ddays(days_horizon))

case_ensemble_wide <- case_ensemble_state %>%
  select(date, asc, .model, starts_with("sim")) %>%
  
  pivot_wider(names_from = ".model",
              values_from = starts_with("sim"))




ensemble_quants <- case_ensemble_wide %>%
  rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
  make_results_quants(plot_quant_widths) %>%
  drop_na(lower) %>%
  
  filter(date <= forecast_start_date + ddays(days_horizon),
         date <= ymd("2022-12-01")) %>%
  
  left_join(ascertainment_ts, by = "date")

clinical_quants_state <- clinical_trajectories_wide_state %>%
  make_results_quants(plot_quant_widths) %>%
  filter(date > forecast_start_date - ddays(days_before_fit), date <= forecast_start_date + ddays(days_horizon))

ascertainment_ts$time_varying_75 <- 1
ascertainment_ts$bau_50 <- 1
ascertainment_ts$constant_75 <- 1

cases_known <- process_local_cases(local_cases_state, ascertainment_ts)




# Important - don't plot capacity limits where they're well out of range of what's observed or predicted
state_capacity_limits <- capacity_limits_tbl %>%
  filter(state == i_state,
         case_when(group == "ward" ~ capacity <= max(c(ward_known$count * 1.5, ward_quants$upper * 1.5)),
                   group == "ICU" ~ capacity <= max(c(ICU_known$count * 1.5, ICU_quants$upper * 1.5)))) %>%
  
  mutate(padded_capacity = str_pad(capacity, max(str_length(as.character(capacity))), side = "r"),
         label = padded_capacity)


plots_common <- list(
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%b")),
               expand = expansion(mult = c(0.01, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0.1))),
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
  
  coord_cartesian(xlim = c(date_plot_start, ymd("2022-12-01")),
                  ylim = c(0, 60000))
)

p_cases <- ggplot() +
  
  geom_vline(xintercept = date_reporting_line, colour = "grey60") +
  
  geom_ribbon(aes(x = date, ymin = lower / 0.5, ymax = upper / 0.5, group = quant, fill = quant),
              
              fill = paletteer::paletteer_d("LaCroixColoR::PeachPear")[1], alpha = 0.5,
              
              data = ensemble_quants %>% filter(asc == "50%")) +
  
  geom_ribbon(aes(x = date, ymin = lower / 0.75, ymax = upper / 0.75, group = quant, fill = quant),
              
              fill = paletteer::paletteer_d("LaCroixColoR::PeachPear")[5], alpha = 0.5,
              
              data = ensemble_quants %>% filter(asc == "75%")) +
  
  
  geom_point(aes(x = date, y = count / 0.5),
             cases_known %>% filter(detection_probability >= 0.95),
             colour =  paletteer::paletteer_d("LaCroixColoR::PeachPear")[1],
             
             size = 1, stroke = 0.2) +
  
  
  geom_point(aes(x = date, y = count / 0.75),
             cases_known %>% filter(detection_probability >= 0.95),
             colour =  paletteer::paletteer_d("LaCroixColoR::PeachPear")[5],
             
             size = 1, stroke = 0.2) +
  
  # Currently smaller than the points themselves!
  geom_linerange(aes(x = date, ymin = lower90, ymax = upper90),
                 cases_known %>% filter(detection_probability <= 0.99)) +
  
  
  geom_vline(xintercept = forecast_start_date + ddays(1),
             colour = "grey40", linetype = "dashed") +
  
  plots_common

p_cases

