
library(tidyverse)
library(lubridate)
library(targets)

## --------------- CHANGE THIS (if you want to) ---------------------
# If models to be included in the ensemble for the case forecast plot needs to change
# This can be different from what is used for the clinical forecasting (which is defined in _targets.R)

ensemble_models_included <- c("gar", "moss", "dst")

forecast_dir <- "results/fc_2022-07-01_final/"
ensemble_path <- "data/mflux/downloads_raw/combined_samples_75asc2022-06-21.csv"

# When our plots go back to
date_plot_start <- ymd("2021-12-01")

# Are we plotting long or short-term forecasts?
is_longterm <- FALSE


days_horizon <- if_else(is_longterm, 30 * 6, 7 * 4)
days_before_fit <- 0

# Add states to these to hide their plots
censor_cases <- c()
censor_ward <- c()
censor_ICU <- c()



if(is_longterm) {
  Sys.setenv(TAR_PROJECT = "long")
  quants <- c(0.5, 0.8, 0.9)
} else{
  Sys.setenv(TAR_PROJECT = NA)
  quants <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
}


# Load clinical forecasting data

date_reporting_line <- tar_read(date_reporting_line)
trajectories <- fst::read_fst(str_c(forecast_dir, "/trajectories.fst"))
forecast_dates <- read_csv(str_c(forecast_dir, "/forecast_dates.csv"), show_col_types = FALSE)
all_state_known_occupancy_ts <- tar_read(all_state_known_occupancy_ts)

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

ascertainment_ts <- read_csv("data/CAR scenarios 2022-06-09.csv") %>%
  mutate(date = dmy(DATE))

if(!is_longterm) {
  ascertainment_ts$time_varying_75 <- 1
}

get_quants <- function(dat, fn = "max", probs = c(0.25, 0.5, 0.75)) {
  long.dat <- dat %>% pivot_longer(cols = starts_with("sim"))
  states <- unique(dat$state)
  nsims <- length(unique(long.dat$name))
  nmods <- length(unique(long.dat$.model))
  totals <- long.dat %>%
    group_by(state, .model, name) %>%
    summarise(value = do.call(fn, list(value)))
  quants <- totals %>%
    arrange(state, value) %>%
    group_by(state) %>%
    mutate(idx = row_number()) %>%
    ungroup() %>%
    filter(idx %in% round(probs*nsims*nmods)) %>%
    mutate(rr = paste0(state,.model,name),
           ps = idx/(nsims*nmods)) %>%
    select(rr,ps)
  out <- long.dat %>%
    mutate(rr = paste0(state,.model,name)) %>%
    left_join(., quants, by = "rr") %>%
    filter(!is.na(ps)) %>%
    select(state, date, ps, value) %>%
    mutate(ps = paste0(ps*100,"%")) %>%
    pivot_wider(names_from =  "ps", values_from = "value")
  return(out)
}

# Define our nice colour gradients (via David Price magic)


alpha_vals <- scales::rescale(rev(1/1.7^(1:length(quants))), to = c(0.05, 0.99))

if(is_longterm) {
  case_cols <- shades::opacity("#FCBA2D", alpha_vals)
} else {
  case_cols <- shades::opacity("#006699", alpha_vals)
}


ward_cols <- shades::opacity("#b53aa0", alpha_vals)
ICU_cols <- shades::opacity("#008200", alpha_vals)

na_plot <- cowplot::ggdraw() + cowplot::draw_label('Not provided')

# If a state is excluded from plotting, can change this
states <- unique(trajectories$state)
states <- states[order(states)]
plots <- list()

for(i_state in states) {
  ensemble_raw <- read_ensemble_state(ensemble_path, i_state, ensemble_models_included)
  
  
  ensemble_wide <- ensemble_raw %>%
    select(date, .model, starts_with("sim")) %>%
    
    pivot_wider(names_from = ".model",
                values_from = starts_with("sim"))
  
  local_cases_state <- local_cases %>%
    filter(state == i_state)
  
  
  cases_known <- local_cases_state %>%
    select(date = date_onset, count, detection_probability) %>%
    filter(detection_probability > 0.95) %>%
    
    mutate(lower90 = true_count_quantile(count, detection_probability, quantiles = 0.05),
           upper90 = true_count_quantile(count, detection_probability, quantiles = 0.95)) %>%
    
    left_join(ascertainment_ts, by = "date") %>%
    mutate(count = count / time_varying_75,
           lower90 = lower90 / time_varying_75,
           upper90 = upper90 / time_varying_75)
  
  forecast_start_date <- local_cases_state %>%
    filter(detection_probability >= 0.95) %>%
    pull(date_onset) %>%
    max()
  
  
  ensemble_quants <- ensemble_wide %>%
    rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
    make_results_quants(quants) %>%
    drop_na(lower) %>%
    
    filter(date <= forecast_start_date + ddays(days_horizon)) %>%
    
    left_join(ascertainment_ts, by = "date") %>%
    mutate(lower = lower / time_varying_75,
           upper = upper / time_varying_75)
  
  model_trajs_wide <- trajectories %>%
    filter(state == i_state) %>%
    select(group, date, sample, count) %>%
    pivot_wider(names_from = sample, values_from = count, names_prefix = "sim_")
  
  state_quants <- model_trajs_wide %>%
    make_results_quants(quants)
  

  ward_quants <- state_quants %>%
    filter(group == "ward", date > forecast_start_date - ddays(days_before_fit),
           date <= forecast_start_date + ddays(days_horizon))
  
  ward_known <- all_state_known_occupancy_ts %>%
    filter(group == "ward", state == i_state, date >= date_plot_start - ddays(30)) %>%
    
    mutate(do_match = date > forecast_start_date & date <= forecast_start_date + ddays(7))
  
  
  
  ICU_quants <- state_quants %>%
    filter(group == "ICU", date > forecast_start_date - ddays(days_before_fit),
           date <= forecast_start_date + ddays(days_horizon),
           quant %in% as.character(quants * 100))
  
  ICU_known <- all_state_known_occupancy_ts %>%
    filter(group == "ICU", state == i_state, date >= date_plot_start - ddays(30)) %>%
    
    mutate(do_match = date > forecast_start_date& date <= forecast_start_date + ddays(7))
  
  
  
  if(is_longterm) {
    ward_rep <- model_trajs_wide %>% 
      filter(group == "ward") %>% mutate(state = 1, .model = 1) %>%
      get_quants("max", probs = seq(0.1, 0.9, by = 0.05)) %>%
      filter(date >= forecast_start_date + ddays(7))
    ICU_rep <- model_trajs_wide %>% 
      filter(group == "ICU") %>% mutate(state = 1, .model = 1) %>%
      get_quants("max", probs = seq(0.1, 0.9, by = 0.05)) %>%
      filter(date >= forecast_start_date + ddays(7))
    
    ensemble_smooth <- bind_rows(
      ensemble_raw %>% 
        filter(.model != "moss"),
      ensemble_raw %>% 
        filter(.model == "moss") %>%
        mutate(across(starts_with("sim"), ~ slider::slide_index_dbl(., .i = date, .f = mean, .before = 7, .after =0)))
    )
    
    ensemble_rep <- get_quants(ensemble_smooth, "max", probs = seq(0.1, 0.9, by = 0.05))
  } else{
    ward_rep <- tibble(); ICU_rep <- tibble(); ensemble_rep <- tibble();
  }
  
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
  
  color <- "grey17"
  thinnes <- 0.1
  
  p_cases <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = "grey60") +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ensemble_quants) +

    geom_line(aes(x = date, y = value / 0.75, group = name),
              color = color, size = thinnes,
              ensemble_rep %>% pivot_longer(-c(state, date))) +
    
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
                   cases_known %>% filter(detection_probability <= 0.99)) +
    
    scale_fill_manual(values = case_cols) +
    
    
    geom_vline(xintercept = forecast_start_date + ddays(1),
               colour = "grey40", linetype = "dashed") +
    
    ggtitle(str_c(i_state, " \u2013 ", if_else(is_longterm, "Infection", "Case") , " incidence by onset date")) +
    
    plots_common
  
  p_ward <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ward_quants) +
    
    geom_line(aes(x = date, y = value, group = name),
              color = color, size = thinnes,
              ward_rep %>% pivot_longer(-c(state, date))) +
    
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
    
    plots_common
  
  
  p_ICU <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ICU_quants) +
    
    geom_line(aes(x = date, y = value, group = name),
              color = color, size = thinnes,
              ICU_rep %>% pivot_longer(-c(state, date))) +
    
    
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
    
    plots_common
  
  if(i_state %in% censor_cases) 
    p_cases <- na_plot
  if(i_state %in% censor_ward)
    p_ward <- na_plot
  if(i_state %in% censor_ICU)
    p_ICU <- na_plot
  
  
  p <- cowplot::plot_grid(
    p_cases, p_ward, p_ICU,
    ncol = 1, align = "v", axis = "lr"
  )
  
  plots <- c(plots, list(p))
  
  ggsave(str_c(forecast_dir, "_sitawareness_state_plot_", i_state, ".png" ),
         plot = p,
         height = 9, width = 8.5, bg = "white")
  
}


zip::zip(zipfile = str_c(forecast_dir, "_sitawareness_plots_", date_reporting_line, ".zip"),
         files = str_c(forecast_dir, "_sitawareness_state_plot_", states, ".png"),
         mode = "cherry-pick")

cairo_pdf(str_c(forecast_dir, "_sitawareness_combined.pdf"),
          width = 8.5, height = 9, onefile = TRUE)
for (i in 1:length(plots)){
  plot(plots[[i]])
}
dev.off()

