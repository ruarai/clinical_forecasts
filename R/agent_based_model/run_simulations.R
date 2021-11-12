
run_single_simulation <- function(case_linelist,
                                  simulation_options,
                                  model_params) {
  
  Rcpp::sourceCpp("cpp/abm_loop.cpp")
  
  delay_compartment_names <- colnames(model_params$delay_params$compartment_LoS_shape)
  
  case_delay_shapes <- model_params$delay_params$
    compartment_LoS_shape[case_linelist$age_class,]
  
  case_delay_means <- model_params$delay_params$
    compartment_LoS_mean[case_linelist$age_class,]
  
  compartment_probs <- model_params$morbidity_params$
    prob_table[case_linelist$age_class,] %>%
    `colnames<-`(paste0("pr_", colnames(model_params$morbidity_params$prob_table)))
  
  
  pr_ICU_moving <- case_linelist$pr_ICU
  pr_ICU_constant <- compartment_probs[,"pr_ward_to_ICU"]
  
  # Calculate our non-ward-to-ICU path probabilities conditional upon our moving
  # ICU probability. This is P(ward_to_discharge|not ward_to_ICU (constant))*P(not ward_to_ICU (moving))
  compartment_probs[, "pr_ward_to_discharge"] <- (compartment_probs[,"pr_ward_to_discharge"] /
    (1 - pr_ICU_constant)) * (1 - pr_ICU_moving)
  
  compartment_probs[, "pr_ward_to_ICU"] <- pr_ICU_moving
  
  run_sim_sample <- function(i) {
    
    
    case_delay_samples <- rgamma(length(case_delay_shapes),
                                 shape = case_delay_shapes, rate = case_delay_shapes / case_delay_means) %>%
      matrix(ncol = ncol(case_delay_shapes)) %>%
      `colnames<-`(paste0("LoS_", delay_compartment_names))
    
    
    case_linelist$t_onset <- as.numeric(case_linelist$date_onset - min(case_linelist$date_onset))
    case_parameter_samples = cbind(
      time_of_infection = case_linelist$t_onset,
      LoS_symptomatic_to_ED = rgamma(nrow(case_linelist), 2, 2 / 14),
      case_delay_samples,
      
      pr_hosp = case_linelist$pr_hosp,
      pr_ICU = case_linelist$pr_ICU,
      
      compartment_probs
    )
    
    t_forecast_horizon <- as.numeric(simulation_options$dates$forecast_horizon - 
                                       simulation_options$dates$simulation_start)
    
    results <- process_loop(case_parameter_samples, t_forecast_horizon,
                            dt = 0.25,
                            ED_queue_capacity = simulation_options$ED_daily_queue_capacity)
    
    
    compartment_names_true <- c("susceptible", "symptomatic_clinical", "symptomatic_nonclinical",
                                "ED_queue", "ward",
                                "ward_died", "ward_discharged", "ICU", "ICU_died", "ICU_discharged",
                                "postICU", "postICU_died",
                                "postICU_discharged")
    
    summary_groups <- list("ward" = c("ward", "postICU"), 
                           "queue" = c("ED_queue"),
                           "ICU" = c("ICU"),
                           "died" = c("ward_died", "ICU_died", "postICU_died"),
                           "discharged" = c("ward_discharged", "postICU_discharged", "ICU_discharged"),
                           "symptomatic (clinical)" = c("symptomatic_clinical")) %>%
      map_dfr(~ tibble(compartment = .), .id="group")  %>%
      select(compartment, group) %>% deframe()
    
    tbl_transitions <- do.call(rbind, results$transitions) %>%
      `colnames<-`(c("case_index", "t", "new_comp")) %>%
      as_tibble() %>%
      mutate(new_comp = compartment_names_true[new_comp + 1],
             t = floor(t),
             date = simulation_options$dates$simulation_start + t) %>%
      group_by(date, new_comp) %>%
      summarise(n = n(), .groups = "drop")
    
    
    tbl_transitions_grouped <- tbl_transitions %>%
      mutate(group = summary_groups[new_comp]) %>%
      
      group_by(date, group) %>%
      summarise(n = sum(n), .groups = "drop")
    
    
    tbl_count <- apply(results$compartment_counts, MARGIN = 2, cumsum)[,1:length(compartment_names_true)] %>%
      `colnames<-`(compartment_names_true) %>%
      as_tibble() %>%
      
      mutate(t = row_number(),
             date = simulation_options$dates$simulation_start + t) %>%
      
      pivot_longer(cols = -c(date, t), names_to = "compartment", values_to = "count")
    
    
    
    tbl_count_grouped <- tbl_count %>%
      mutate(group = summary_groups[compartment]) %>%
      
      group_by(date, group) %>%
      summarise(count = sum(count), .groups = "drop")
    
    
    list(tbl_count = tbl_count,
         tbl_count_grouped = tbl_count_grouped,
         
         tbl_transitions = tbl_transitions,
         tbl_transitions_grouped = tbl_transitions_grouped)
  }
  
  results_samples <- map(1:simulation_options$n_samples_per_trajectory,
                         run_sim_sample)
  
  
  tbl_count <- map_dfr(results_samples, function(r) r$tbl_count, .id = "sample_ix")
  tbl_count_grouped <- map_dfr(results_samples, function(r) r$tbl_count_grouped, .id = "sample_ix")
  tbl_transitions <- map_dfr(results_samples, function(r) r$tbl_transitions, .id = "sample_ix")
  tbl_transitions_grouped <- map_dfr(results_samples, function(r) r$tbl_transitions_grouped, .id = "sample_ix")
  
  
  list(tbl_count = tbl_count,
       tbl_count_grouped = tbl_count_grouped,
       
       tbl_transitions = tbl_transitions,
       tbl_transitions_grouped = tbl_transitions_grouped)
}

run_worker_jobs <-  function(linelists) {
  
  require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(purrr, quietly = TRUE, warn.conflicts = FALSE)
  require(tibble, quietly = TRUE, warn.conflicts = FALSE)
  
  results_worker <- map(linelists, function(linelist) {
    run_single_simulation(linelist, simulation_options, model_params)
  })
  
  tbl_count <- map_dfr(results_worker, function(r) r$tbl_count, .id = "worker_ix")
  tbl_count_grouped <- map_dfr(results_worker, function(r) r$tbl_count_grouped, .id = "worker_ix")
  tbl_transitions <- map_dfr(results_worker, function(r) r$tbl_transitions, .id = "worker_ix")
  tbl_transitions_grouped <- map_dfr(results_worker, function(r) r$tbl_transitions_grouped, .id = "worker_ix")
  
  list(tbl_count = tbl_count,
       tbl_count_grouped = tbl_count_grouped,
       
       tbl_transitions = tbl_transitions,
       tbl_transitions_grouped = tbl_transitions_grouped)
}


run_simulations <- function(input_trajectories,
                            simulation_options,
                            model_params) {
  stopifnot(simulation_options$n_trajectories == length(input_trajectories))
  
  sim_t0 <- Sys.time()
  
  worker_trajectories <- split(input_trajectories,
                               rep(1:16, length.out = length(input_trajectories)))
  
  require(future)
  require(furrr)
  require(future.callr)
  plan(callr)

  results_all <- future_map(worker_trajectories, run_worker_jobs,
    .options = furrr_options(seed = TRUE,
                             globals = c("simulation_options", 
                                         "model_params",
                                         "run_single_simulation")))
  
  
  print(paste0("Ran simulation loop in ", 
               round(Sys.time() - sim_t0, 4), " ",
                     units(Sys.time() - sim_t0) ))
  
  tbl_count <- map_dfr(results_all, function(r) r$tbl_count, .id = "job_ix") %>%
    mutate(ix = str_c(job_ix, "-", worker_ix ,"-",  sample_ix)) %>% select(-c(job_ix, worker_ix, sample_ix))
  tbl_count_grouped <- map_dfr(results_all, function(r) r$tbl_count_grouped, .id = "job_ix") %>%
    mutate(ix = str_c(job_ix, "-", worker_ix ,"-",  sample_ix)) %>% select(-c(job_ix, worker_ix, sample_ix))
  
  tbl_transitions <- map_dfr(results_all, function(r) r$tbl_transitions, .id = "job_ix") %>%
    mutate(ix = str_c(job_ix, "-", worker_ix ,"-",  sample_ix)) %>% select(-c(job_ix, worker_ix, sample_ix))
  tbl_transitions_grouped <- map_dfr(results_all, function(r) r$tbl_transitions_grouped, .id = "job_ix") %>%
    mutate(ix = str_c(job_ix, "-", worker_ix ,"-",  sample_ix)) %>% select(-c(job_ix, worker_ix, sample_ix))
  
  
  
  make_quants <- function(tbl) {
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
  
  
  
  
  tbl_count_grouped_quants <- tbl_count_grouped %>%
    pivot_wider(names_from = "ix", names_prefix = "sim_",
                values_from = "count") %>%
    mutate(group = replace_na(group, "other")) %>%
    make_quants()
  
  tbl_transitions_grouped_quants <- tbl_transitions_grouped %>%
    pivot_wider(names_from = ix,
                values_from = n,
                names_prefix = "sim_") %>%
    mutate(across(starts_with("sim_"), ~ replace_na(., 0))) %>%
    
    make_quants()
  
  list(tbl_count = tbl_count,
       tbl_count_grouped = tbl_count_grouped,
       tbl_count_grouped_quants = tbl_count_grouped_quants,
       
       tbl_transitions = tbl_transitions,
       tbl_transitions_grouped = tbl_transitions_grouped,
       tbl_transitions_grouped_quants = tbl_transitions_grouped_quants)
}
