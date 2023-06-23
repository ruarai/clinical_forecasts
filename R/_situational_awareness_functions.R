

label_date_short_keep_month <- function(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n") {
  scales:::force_all(format, sep)
  
  function(x) {
    dt <- unclass(as.POSIXlt(x))
    
    changes <- cbind(
      year = scales:::changed(dt$year),
      month = scales:::changed(dt$mon),
      day = scales:::changed(dt$mday)
    )
    # Ensure large unit changes implies that small units change too
    # Would be more elegant with cumany() but cumsum() does the job
    changes <- t(apply(changes, 1, cumsum)) >= 1
    
    # Trim out "firsts" from smallest to largest - only want to trim (e.g.)
    # January if all dates are the first of the month.
    if (inherits(x, "Date") || all(dt$hour == 0 & dt$min == 0, na.rm = TRUE)) {
      format[[4]] <- NA
      
      if (all(dt$mday == 1, na.rm = TRUE)) {
        format[[3]] <- NA
        
        if (all(dt$mon == 0, na.rm = TRUE)) {
          format[[2]] <- NA
        }
      }
    }
    
    for_mat <- cbind(
      ifelse(changes[, 1], format[[1]], NA),
      ifelse(TRUE, format[[2]], NA),
      ifelse(changes[, 3], format[[3]], NA),
      format[[4]]
    )
    
    format <- apply(for_mat, 1, function(x) paste(rev(x[!is.na(x)]), collapse = sep))
    format(x, format)
  }
}

plot_params <- list(
  "rep_colour" = "grey17",
  "rep_thinness" = 0.1,
  "reporting_line_colour" = "grey60",
  "point_size" = 0.8,
  "point_stroke" = 0.2,
  "point_colour" = "grey20"
)

source("R/make_result_quants.R")
source("R/ensemble.R")
source("R/capacity_table.R")

# NegBin observation model to account for reporting delays (for NG's linelist)
true_count_quantile <- function(observed_count, probability, quantiles = c(0.05, 0.95)) {
  observed_count + qnbinom(quantiles, observed_count + 1, probability)
}

plot_quant_widths = seq(0.2, 0.9, by = 0.1)

plots_common_long <- list(
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
  scale_x_date(date_breaks = "month",
               labels = scales::label_date_short(format = c("%Y", "%B")),
               expand = expansion(mult = c(0.01, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0.02, 0.1)),
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
        text = element_text(family = "Helvetica"),
        axis.line.x = element_blank())
)

plots_common_short <- list(
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)),
  scale_x_date(date_breaks = "2 weeks",
               labels = label_date_short_keep_month(),
               expand = expansion(mult = c(0.01, 0.05))),
  scale_y_continuous(breaks = scales::extended_breaks(),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0.02, 0.1)),
                     position = "right"),
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
        text = element_text(family = "Helvetica"),
        axis.line.x = element_blank())
)


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


get_current_capacity_tbl <- function(multipliers = 1:2) {
  
  # Load in the capacity tables
  source("R/capacity_table.R")
  capacity_limits %>%
    map_dfr(function(x) tibble_row(capacity_ward = x$ward, capacity_ICU = x$ICU),
            .id = "state") %>%
    pivot_longer(cols = c(capacity_ward, capacity_ICU),
                 names_prefix = "capacity_",
                 names_to = "group", values_to = "capacity") %>%
    
    expand_grid(multiplier = multipliers) %>%
    mutate(capacity = capacity * multiplier)
  
}


get_ascertainment_ts <- function() {
  read_csv("data/CAR scenarios 2022-06-09.csv", show_col_types = FALSE) %>%
    mutate(date = dmy(DATE))
}


get_plot_colors <- function(n_plot_quants, is_longterm) {
  
  alpha_vals <- scales::rescale(rev(1/1.7^(1:n_plot_quants)), to = c(0.05, 0.99))
  
  if(is_longterm) {
    case_cols <- shades::opacity("#FCBA2D", alpha_vals)
  } else {
    case_cols <- shades::opacity("#006699", alpha_vals)
  }
  
  
  ward_cols <- shades::opacity("#b53aa0", alpha_vals)
  ICU_cols <- shades::opacity("#008200", alpha_vals)
  
  
  list(
    "case_or_inf" = case_cols,
    "ward" = ward_cols,
    "ICU" = ICU_cols
  )
}

get_trajectories <- function(results_dir) {
  str_c(results_dir, "/trajectories.fst") %>%
    fst::read_fst()
}

get_states <- function(trajectories) {
  
  states <- unique(trajectories$state)
  states <- states[order(states)]
  states
}

get_clinical_trajectories_wide <- function(trajectories) {
  trajectories %>%
    select(state, group, date, sample, count) %>%
    pivot_wider(names_from = sample, values_from = count, names_prefix = "sim_")
}

get_ensemble_wide <- function(case_ensemble) {
  case_ensemble %>%
    select(state, date, .model, starts_with("sim")) %>%
    
    pivot_wider(names_from = ".model",
                values_from = starts_with("sim"))
}


process_local_cases <- function(local_cases_state, ascertainment_ts) {
  local_cases_state %>%
    select(date = date_onset, count, detection_probability) %>%
    filter(detection_probability > 0.95) %>%
    
    mutate(lower90 = true_count_quantile(count, detection_probability, quantiles = 0.05),
           upper90 = true_count_quantile(count, detection_probability, quantiles = 0.95))
}



get_forecast_start_date <- function(local_cases_state, pr_detect = 0.95) {
  local_cases_state %>%
    filter(detection_probability >= pr_detect) %>%
    pull(date_onset) %>%
    max()
}


make_plots <- function(
  i_state,  

  case_ensemble_wide_state,
  clinical_trajectories_wide_state,
  local_cases,
  occupancy_data,
  
  
  state_forecast_start_date,
  date_plot_start,
  date_plot_end,
  
  plots_common,
  
  days_before_fit,
  days_horizon,
  plot_quant_widths
) {
  
  ensemble_quants <- case_ensemble_wide_state %>%
    rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>%
    make_results_quants(plot_quant_widths) %>%
    drop_na(lower) %>%
    
    filter(date <= state_forecast_start_date + ddays(days_horizon))
  
  clinical_quants_state <- clinical_trajectories_wide_state %>%
    make_results_quants(plot_quant_widths) %>%
    filter(date > state_forecast_start_date - ddays(days_before_fit), date <= state_forecast_start_date + ddays(days_horizon))
  
  cases_known <- local_cases %>%
    filter(state == i_state) %>%
    process_local_cases() %>%
    filter(date >= date_plot_start)
  
  ward_quants <- clinical_quants_state %>%
    filter(group == "ward")
  ward_known <- occupancy_data %>%
    filter(group == "ward", state == i_state) %>%
    filter(date >= date_plot_start)
  
  ICU_quants <- clinical_quants_state %>%
    filter(group == "ICU")
  ICU_known <- occupancy_data %>%
    filter(group == "ICU", state == i_state) %>%
    filter(date >= date_plot_start)
  
  p_cases <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = plot_params$reporting_line_colour) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ensemble_quants) +
    
    geom_hline(yintercept = 0, colour = "grey40", size = 0.5) +
    
    
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
    
    
    geom_vline(xintercept = state_forecast_start_date + ddays(1),
               colour = "grey40", linetype = "dashed") +
    
    ggtitle(str_c(i_state, " \u2013 Case incidence by onset date")) +
    
    plots_common +
    coord_cartesian(xlim = c(date_plot_start, date_plot_end), clip = "off")
  
  p_ward <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ward_quants) +
    
    geom_hline(yintercept = 0, colour = "grey40", size = 0.5) +
    
    geom_point(aes(x = date, y = count),
               color = plot_params$point_colour,
               ward_known,
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
    scale_fill_manual(values = plot_cols$ward) +
    
    geom_vline(xintercept = state_forecast_start_date + ddays(7),
               colour = "grey40", linetype = "dashed") +
    
    ggtitle(str_c(i_state, " \u2013 Ward bed occupancy")) +
    
    plots_common + 
    coord_cartesian(xlim = c(date_plot_start, date_plot_end), clip = "off")
  
  p_ICU <- ggplot() +
    
    geom_vline(xintercept = date_reporting_line, colour = "grey60")  +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant),
                
                data = ICU_quants) +
    
    geom_hline(yintercept = 0, colour = "grey40", size = 0.5) +
    
    
    geom_point(aes(x = date, y = count),
               color = plot_params$point_colour,
               ICU_known,
               
               size = plot_params$point_size, stroke = plot_params$point_stroke) +
    
    scale_fill_manual(values = plot_cols$ICU) +
    
    geom_vline(xintercept = state_forecast_start_date + ddays(7),
               colour = "grey40", linetype = "dashed") +
    
    ggtitle(str_c(i_state, " \u2013 ICU bed occupancy")) +
    
    plots_common + 
    coord_cartesian(xlim = c(date_plot_start, date_plot_end), clip = "off")
  
  
  p <- cowplot::plot_grid(
    p_cases, p_ward, p_ICU,
    ncol = 1, align = "v", axis = "lr"
  )
  
  list(
    "p" = p,
    "p_cases" = p_cases,
    "p_ward" = p_ward,
    "p_ICU" = p_ICU
  )
}

make_short_plot_grid <- function(short_results, plot_name) {
  cowplot::plot_grid(
    plotlist = imap(
      short_results,
      function(plots, i) {
        if(i <= 6) {
          plots[[plot_name]] + ggtitle(states[i]) + theme(axis.text.x = element_blank())
        } else{
          plots[[plot_name]] + ggtitle(states[i])
        }
      }
    ),
    ncol = 2,
    align = "v", axis = "lrtb",
    rel_heights = c(1, 1, 1, 1.3)
  )
}



