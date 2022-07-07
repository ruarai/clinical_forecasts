fit_ascertainment <- function(
  prediction_dates,
  date_case_data,
  vaccination_tables_state,
  vaccination_effects_state,
  population_state,
  nindss_state,
  ngm
) {
  n_preds <- length(prediction_dates)
  
  
  case_counts_mat <-  nindss_state %>%
    select(age, date_onset) %>%
    
    drop_na(age) %>%
    mutate(age_band = assign_age_band(age)) %>%
    
    count(date_onset, age_band) %>%
    filter(date_onset >= prediction_dates[1]) %>%
    rowwise() %>%
    mutate(date_nearest = prediction_dates[which.min(abs(prediction_dates - date_onset))]) %>%
    
    group_by(date = date_nearest, age_band) %>%
    summarise(n = sum(n), .groups = "drop") %>% 
    
    complete(date = prediction_dates,
             age_band = age_band_order,
             fill = list(n = 0)) %>%
    
    filter(date <= date_case_data)  %>%
    
    mutate(age_band = factor(age_band, levels = age_band_order)) %>%
    arrange(age_band) %>%
    
    pivot_wider(names_from = age_band,
                values_from = n) %>%
    
    select(-date) %>%
    as.matrix()
  
  vec_population <- population_state$population
  
  
  
  vaccination_acquisition_effect <- do.call(rbind, vaccination_effects_state$acquisition_effect)
  vaccination_transmission_effect <- do.call(rbind, vaccination_effects_state$transmission_effect)
  vaccination_coverage <- do.call(rbind, vaccination_effects_state$prop_coverage)
  
  
  ascertainment_adj <- normal(0, 0.5, dim = 17)
  
  ascertainment_ts <- get_ascertainment_timeseries(prediction_dates)$ascertainment
  ascertainment_mat <- matrix(ascertainment_ts, ncol = 17, nrow = length(ascertainment_ts))
  
  ilogit <- function(x) exp(x) / (1 + exp(x))
  
  ascertainment_mult <- (1 / ilogit(
    sweep(qlogis(ascertainment_mat), 2, ascertainment_adj, "+")
  ))
  
  days_ago <- 1:1000
  
  log10_neut_over_time_infection <- reff_env$log10_neut_over_time(
    seq(1, 1000, by = 4),
    params$neut$log10_mean_neut_infection,
    params$neut$neut_decay
  )
  
  
  normal_density <- function(x, mean, sd) {
    op <- greta::.internals$nodes$constructors$op
    op(
      "normal_density",
      x, mean, sd,
      tf_operation = "tf_normal_density"
    )
  }
  
  tf_normal_density <- function(x, mean, sd) {
    dist <- greta:::tfp$distributions$Normal(loc = mean, scale = sd)
    dist$prob(x)
  }
  
  reff_env$tf_normal_density <- tf_normal_density
  reff_env$normal_density <- normal_density
  
  get_neuts_effect <- function(mean_neuts, c50_vec) {
    reff_env$ve_from_mean_log10_neut(
      mean_log10_neut_vec = mean_neuts,
      sd_log10_neut = params$ve$sd_log10_neut_titres,
      log_k = params$ve$log_k,
      c50_vec = as.matrix(c50_vec),
      method = "gaussian"
    )
  }
  
  log10_neut_over_time_infection_mat <- matrix(log10_neut_over_time_infection,
                                               nrow = length(log10_neut_over_time_infection),
                                               ncol = 17, byrow = FALSE)
  
  ix_start <- nrow(case_counts_mat) - 3
  ix_end <- ix_start + 1
  
  
  previous_infections <- sweep(case_counts_mat[1:ix_start, ], 2, vec_population, FUN = "/") *
    ascertainment_mult[1:ix_start, ]
  
  infection_coverage <- apply(
    cbind(colSums(previous_infections), rep(1, times = 17)),
    MARGIN = 1,
    FUN = "min"
  )
  
  
  mean_neuts_infection <- colSums(log10_neut_over_time_infection_mat[ix_start:1, ] * previous_infections) / 
    infection_coverage
  
  infection_acquisition_effect <- get_neuts_effect(mean_neuts_infection, params$ve$c50_acquisition)
  infection_transmission_effect <- get_neuts_effect(mean_neuts_infection, params$ve$c50_transmission)
  
  
  
  pretable_mat <- vaccination_tables_state[[ix_start]] %>%
    select(-days_i) %>%
    as.matrix()
  
  
  
  mean_neuts_infection_vacc <- colSums(pretable_mat * previous_infections) / 
    infection_coverage
  
  infection_vacc_acquisition_effect <- get_neuts_effect(mean_neuts_infection_vacc, params$ve$c50_acquisition)
  infection_vacc_transmission_effect <- get_neuts_effect(mean_neuts_infection_vacc, params$ve$c50_transmission)
  
  
  
  
  p_infected_only <- infection_coverage * (1 - vaccination_coverage[ix_start,])
  p_vaccinated_only <- vaccination_coverage[ix_start, ] * (1 - infection_coverage)
  
  
  p_infandvax <- infection_coverage * vaccination_coverage[ix_start, ]
  
  weighted_acquisition <- p_infected_only * infection_acquisition_effect + 
    p_vaccinated_only * vaccination_acquisition_effect[ix_start] +
    p_infandvax * infection_vacc_acquisition_effect
  
  
  weighted_transmission <- p_infected_only * infection_transmission_effect + 
    p_vaccinated_only * vaccination_transmission_effect[ix_start, ] + 
    p_infandvax * infection_vacc_transmission_effect
  
  acquisition_multiplier <- 1 - weighted_acquisition
  transmission_multiplier <- 1 - weighted_transmission
  
  
  
  ngm_mult <- acquisition_multiplier %*% t(transmission_multiplier)
  
  start_age_dist <- case_counts_mat[ix_start, ] / sum(case_counts_mat[ix_start, ])
  
  pred_age_dist <- (ngm * ngm_mult) %*% as_data(start_age_dist)
  
  pred_case_age_dist <- pred_age_dist / t(ascertainment_mult[ix_end,])
  
  prop_pred_case_age_dist <- t(pred_case_age_dist / sum(pred_case_age_dist))
  
  next_cases <- t(case_counts_mat[ix_end,])
  n_next_cases <- sum(next_cases)
  
  distribution(next_cases) <- multinomial(n_next_cases, prop_pred_case_age_dist)
  
  m <- model(ascertainment_adj)
  
  
  draws <- mcmc(m, n_samples = 1000, n_cores = 1)
  
  print("Finished MCMC")
  
  
  
  final_ascertainment <- ilogit(
    sweep(qlogis(ascertainment_mat), 2, summary(draws)$statistics[,"Mean"], "+")
  )
  
  
  list(
    "ascertainment_mat" = final_ascertainment
  )
}