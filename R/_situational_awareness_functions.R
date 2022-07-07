
source("R/make_result_quants.R")
source("R/ensemble.R")
source("R/capacity_table.R")

# NegBin observation model to account for reporting delays (for NG's linelist)
true_count_quantile <- function(observed_count, probability, quantiles = c(0.05, 0.95)) {
  observed_count + qnbinom(quantiles, observed_count + 1, probability)
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


get_current_capacity_tbl <- function() {
  
  # Load in the capacity tables
  source("R/capacity_table.R")
  capacity_limits %>%
    map_dfr(function(x) tibble_row(capacity_ward = x$ward, capacity_ICU = x$ICU),
            .id = "state") %>%
    pivot_longer(cols = c(capacity_ward, capacity_ICU),
                 names_prefix = "capacity_",
                 names_to = "group", values_to = "capacity")
  
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

get_state_clinical_trajectories_wide <- function(trajectories, i_state) {
  trajectories %>%
    filter(state == i_state) %>%
    select(group, date, sample, count) %>%
    pivot_wider(names_from = sample, values_from = count, names_prefix = "sim_")
}


process_local_cases <- function(local_cases_state, ascertainment_ts) {
  local_cases_state %>%
    select(date = date_onset, count, detection_probability) %>%
    filter(detection_probability > 0.95) %>%
    
    mutate(lower90 = true_count_quantile(count, detection_probability, quantiles = 0.05),
           upper90 = true_count_quantile(count, detection_probability, quantiles = 0.95)) %>%
    
    left_join(ascertainment_ts, by = "date") %>%
    mutate(count = count / time_varying_75,
           lower90 = lower90 / time_varying_75,
           upper90 = upper90 / time_varying_75)
}



get_forecast_start_date <- function(local_cases_state, pr_detect = 0.95) {
  local_cases_state %>%
    filter(detection_probability >= pr_detect) %>%
    pull(date_onset) %>%
    max()
}

get_public_occupancy_data <- function() {
  source("R/data_various.R")
  c19data <- get_c19data()
  
  source("R/occupancy_timeseries.R")
  
  make_occupancy_timeseries(c19data, NULL)
}
