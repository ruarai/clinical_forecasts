

library(tidyverse)
library(lubridate)

source("R/ensemble.R")
source("R/read_occupancy_data.R")
source("R/_situational_awareness_functions.R")
source("R/make_result_quants.R")



latest_occupancy_data <- read_occupancy_data("data/occupancy/NAT_2023-10-12_Data for Uni of Melbourne.xlsx")
latest_case_data <- read_csv("data/local_cases_input_2023-10-12.csv") %>% 
  rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))



date_plot_start <- ymd("2023-06-01")

weeks_back <- 3


all_forecasts <- tibble(
  forecast_name = list.files(
    "results", pattern = "\\d\\d_final$",
    include.dirs = TRUE
  )
) %>%
  mutate(forecast_dir = str_c("results/", forecast_name, "/"),
         forecast_trajectories_file = str_c(forecast_dir, "trajectories.fst"),
         local_cases_file = str_c(forecast_dir, "archive/local_cases.csv"),
         ensemble_file = str_c(forecast_dir, "archive/ensemble.csv"),
         
         forecast_run_date = ymd(str_extract(forecast_name, "\\d{4}-\\d{2}-\\d{2}")))

recent_forecasts <- all_forecasts %>%
  slice_max(forecast_run_date, n = weeks_back) %>%
  mutate(forecast_ix = row_number()) %>% 
  rowwise() %>%
  mutate(forecast_trajectories = list(fst::read_fst(forecast_trajectories_file)),
         local_cases = list(read_csv(local_cases_file, show_col_types = FALSE))) %>%
  ungroup()


recent_forecast_trajectories <- recent_forecasts %>%
  select(forecast_name, forecast_run_date, forecast_ix, forecast_trajectories) %>%
  unnest(forecast_trajectories)

recent_forecast_local_cases <- recent_forecasts %>%
  select(forecast_name, forecast_ix, local_cases) %>%
  unnest(local_cases) %>% 
  rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))

recent_forecast_start_dates <-recent_forecast_local_cases  %>%
  group_by(forecast_name, state) %>%
  do(case_forecast_start_date = get_forecast_start_date(.)) %>%
  unnest(case_forecast_start_date)


recent_forecast_case_forecasts <- recent_forecasts %>%
  select(forecast_name, forecast_run_date, forecast_ix, ensemble_file) %>%
  rowwise() %>%
  mutate(ensemble = list(vroom::vroom(ensemble_file, show_col_types = FALSE))) %>%
  unnest(ensemble)
  


occupancy_data <- tibble(
  file = list.files("data/occupancy/", pattern = "^NAT_", full.names = TRUE)
) %>%
  mutate(file_date = ymd(str_extract(file, "\\d{4}-\\d{2}-\\d{2}"))) %>%
  filter(file_date <= max(recent_forecasts$forecast_run_date) + days(1)) %>% 
  slice_max(file_date, n = weeks_back) %>%
  mutate(forecast_ix = row_number()) %>%
  
  rowwise() %>%
  mutate(data = list(read_occupancy_data(file))) %>%
  ungroup() %>%
  unnest(data) %>%
  select(forecast_ix, state, group, date, count)

days_horizon <- 28

interval_widths <- seq(0.2, 0.9, by = 0.1)
plot_cols <- get_plot_colors(length(interval_widths), is_longterm = FALSE)
plot_cols$case <- plot_cols$case_or_inf

recent_forecast_quants <- recent_forecast_trajectories %>%
  select(forecast_name, forecast_run_date, forecast_ix, state, group, sample, date, count) %>% 
  pivot_wider(names_from = sample,
              names_prefix = "sim_",
              values_from = count) %>%
  
  make_results_quants(interval_widths)


recent_ensemble_quants <- recent_forecast_case_forecasts %>%
  select(-forecast_origin) %>% 
  pivot_wider(names_from = ".model", values_from = starts_with("sim")) %>% 
  rename_with(function(x) str_replace(x, "sim", "sim_")) %>%
  make_results_quants(na.rm = TRUE) %>%
  select(forecast_name, forecast_run_date, forecast_ix, state, date, median, quant, lower, upper) %>%
  mutate(group = "case")


recent_forecasts_plot_data <- bind_rows(recent_forecast_quants, recent_ensemble_quants) %>%
  left_join(recent_forecast_start_dates, by = c("forecast_name", "state")) %>%
  filter(date >= case_forecast_start_date, date <= case_forecast_start_date + ddays(days_horizon))

latest_occupancy_plot_data <- bind_rows(
  latest_occupancy_data,
  latest_case_data %>% filter(detection_probability >= 0.95) %>% select(state, date = date_onset, count) %>% mutate(group = "case")
) %>%
  filter(date >= date_plot_start)

occupancy_plot_data <- bind_rows(
  occupancy_data,
  recent_forecast_local_cases %>% filter(detection_probability >= 0.95) %>%  select(state, forecast_ix, date = date_onset, count) %>% mutate(group = "case")
)%>%
  filter(date >= date_plot_start)

recent_forecast_start_dates_plot_data <- forecast_start_dates <- recent_forecasts_plot_data %>%
  distinct(state, forecast_ix, case_forecast_start_date)


date_plot_end <- max(recent_forecasts_plot_data$date)

state_maxima_plot_data <- bind_rows(
  recent_forecasts_plot_data %>% group_by(state, group) %>% summarise(max_count = max(upper), .groups = "drop"),
  occupancy_plot_data %>% group_by(state, group) %>% summarise(max_count = max(count), .groups = "drop")
) %>% 
  group_by(state, group) %>% summarise(max_count = max(max_count), .groups = "drop")


state_groups <- list(c("ACT", "NSW", "NT", "QLD"), c("SA", "TAS", "VIC", "WA"))

plots <- expand_grid(
  i_state_group = 1:2,
  i_forecast_ix = weeks_back:1,
  i_group = c("case", "ward", "ICU")
) %>%
  pmap(
    function(i_forecast_ix, i_group, i_state_group) {
      i_states <- state_groups[[i_state_group]]
      
      i_forecasts_plot_data <- recent_forecasts_plot_data %>%
        filter(group == i_group, forecast_ix == i_forecast_ix, state %in% i_states)
      
      
      i_forecasts_plot_data %>% 
        ggplot() +
        
        geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
        
        geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
        
        geom_point(aes(x = date, y = count),
                   colour = "black", size = 0.5,
                   latest_occupancy_plot_data %>% filter(group == i_group, state %in% i_states)) +
        
        geom_point(aes(x = date, y = count),
                   colour = "grey60", size = 0.5,
                   occupancy_plot_data %>% filter(group == i_group, forecast_ix == i_forecast_ix, state %in% i_states)) +
        
        geom_vline(aes(xintercept = case_forecast_start_date + days(if_else(i_group == "case", 1, 8) )),
                   recent_forecast_start_dates_plot_data %>% filter(forecast_ix == i_forecast_ix, state %in% i_states),
                   linetype = "44") +
        
        geom_blank(aes(y = max_count),
                   state_maxima_plot_data %>% filter(group == i_group, state %in% i_states)) +
        
        facet_wrap(~state, ncol = 1, scales = "free_y") +
        
        scale_fill_manual(values = plot_cols[[i_group]]) +
        
        plots_common_short +
        
        ggtitle(if_else(i_group == "case",
                        str_c(i_forecast_ix, " week retrospective\nForecasts for ", i_group),
                        str_c("\nForecasts for ", i_group)
        ), if_else(i_group == "case", str_c("Forecasts produced ", i_forecasts_plot_data$forecast_run_date[1]), "")) +
        
        coord_cartesian(clip = "off", xlim = c(date_plot_start, date_plot_end))
    }
  )



cairo_pdf(str_c("results_retrospective/", "clinical_forecasts_evaluation_", max(recent_forecasts$forecast_run_date), ".pdf"),
          width = 13, height = 8, onefile = TRUE)
for (i in seq(1, length(plots), by = 3)) {
  plot(cowplot::plot_grid(plots[[i]], plots[[i + 1]], plots[[i + 2]], ncol = 3, align = "h", axis = "tb"))
}
dev.off()






