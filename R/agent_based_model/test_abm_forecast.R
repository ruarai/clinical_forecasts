delay_compartment_names <- colnames(model_params$delay_params$compartment_LoS_shape)

samples_per_sim <- 4

run_single_simulation <- function(case_linelist) {
  
  case_delay_shapes <- model_params$delay_params$
    compartment_LoS_shape[case_linelist$age_class,]
  
  case_delay_means <- model_params$delay_params$
    compartment_LoS_mean[case_linelist$age_class,]
  
  case_pr_death_ward <- model_params$morbidity_params$
    prob_death_ward_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]
  
  case_pr_death_ICU <- model_params$morbidity_params$
    prob_death_ICU_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]
  
  case_pr_death_postICU <- model_params$morbidity_params$
    prob_death_post_ICU_lookup[cbind(case_linelist$age_class, case_linelist$vaccine)]
  
  
  
  case_delay_samples <- rgamma(length(case_delay_shapes),
                               shape = case_delay_shapes, rate = case_delay_shapes / case_delay_means) %>%
    matrix(ncol = ncol(case_delay_shapes)) %>%
    `colnames<-`(str_c("LoS_", delay_compartment_names))
  
  
  case_linelist$t_onset <- as.numeric(case_linelist$date_onset - min(case_linelist$date_onset))
  case_parameter_samples = cbind(
    time_of_infection = case_linelist$t_onset,
    case_delay_samples,
    
    pr_hosp = case_linelist$pr_hosp,
    pr_ICU = case_linelist$pr_ICU,
    
    pr_death_ward = case_pr_death_ward,
    pr_death_ICU = case_pr_death_ICU,
    pr_death_postICU = case_pr_death_postICU
  )
  
  run_sim_sample <- function(i) {
    Rcpp::sourceCpp("cpp/abm_loop.cpp")
    
    t_forecast_horizon <- as.numeric(forecast_dates$date_forecast_horizon - date_0)
    
    results <- process_loop(case_parameter_samples, t_forecast_horizon,
                            dt = 0.25,
                            ED_queue_capacity = 5000)
    
    
    compartment_names_true <- c("susceptible", "symptomatic", "symptomatic_nonclinical",
                                "ED_queue", "ward",
                                "ward_died", "ward_discharged", "ICU", "ICU_died",
                                "postICU_to_death", "postICU_to_discharge", "postICU_died",
                                "postICU_discharged")
    
    tbl_transitions <- do.call(rbind, results$transitions) %>%
      `colnames<-`(c("case_index", "t", "new_comp")) %>%
      as_tibble() %>%
      mutate(new_comp = compartment_names_true[new_comp + 1],
             t = floor(t),
             date = date_0 + t) %>%
      group_by(date, new_comp) %>%
      summarise(n = n(), .groups = "drop")
    
    
    tbl_count <- apply(results$compartment_counts, MARGIN = 2, cumsum)[,1:length(compartment_names_true)] %>%
      `colnames<-`(compartment_names_true) %>%
      as_tibble() %>%
      
      mutate(t = row_number(),
             date = date_0 + t) %>%
      
      pivot_longer(cols = -c(date, t), names_to = "compartment", values_to = "count")
    
    
    summary_groups <- list("ward" = c("ward", "postICU_to_death", "postICU_to_discharge"), 
                           "queue" = c("ED_queue"),
                           "ICU" = c("ICU"),
                           "died" = c("ward_died", "ICU_died", "postICU_died"),
                           "discharged" = c("ward_discharged", "postICU_discharged"),
                           "symptomatic" = c("symptomatic")) %>%
      map_dfr(~ tibble(compartment = .), .id="group")  %>%
      select(compartment, group) %>% deframe()
    
    tbl_count_grouped <- tbl_count %>%
      mutate(group = summary_groups[compartment]) %>%
      
      group_by(date, group) %>%
      summarise(count = sum(count), .groups = "drop")
    
    
    list(tbl_count = tbl_count,
         tbl_count_grouped = tbl_count_grouped,
         
         tbl_transitions = tbl_transitions)
  }
  
  results_samples <- map(1:samples_per_sim, run_sim_sample)
  
  
  tbl_count <- map_dfr(results_samples, function(r) r$tbl_count, .id = "sample_ix")
  tbl_count_grouped <- map_dfr(results_samples, function(r) r$tbl_count_grouped, .id = "sample_ix")
  tbl_transitions <- map_dfr(results_samples, function(r) r$tbl_transitions, .id = "sample_ix")

  
  list(tbl_count = tbl_count,
       tbl_count_grouped = tbl_count_grouped,
       
       tbl_transitions = tbl_transitions)
}

library(future.callr)
library(furrr)
library(progressr)
plan(callr)


a <- Sys.time()

with_progress({
  p <- progressor(n_simulation)
  if(n_simulation * samples_per_sim > 100) {
    results_all <- future_map(assembled_linelists, function(linelist) {
      p()
      run_single_simulation(linelist)
    })
  } else {
    results_all <- map(assembled_linelists, function(linelist) {
      p()
      run_single_simulation(linelist)
    })
  }
})

print(Sys.time() - a)

tbl_count <- map_dfr(results_all, function(r) r$tbl_count, .id = "sim_ix") %>%
  mutate(sim_ix = str_c(sim_ix, "-", sample_ix)) %>% select(-sample_ix)
tbl_count_grouped <- map_dfr(results_all, function(r) r$tbl_count_grouped, .id = "sim_ix") %>%
  mutate(sim_ix = str_c(sim_ix, "-", sample_ix)) %>% select(-sample_ix)
tbl_transitions <- map_dfr(results_all, function(r) r$tbl_transitions, .id = "sim_ix") %>%
  mutate(sim_ix = str_c(sim_ix, "-", sample_ix)) %>% select(-sample_ix)


forecast_date_lines <- list(
  geom_vline(xintercept = forecast_dates$date_last_infection_50 - 5, linetype = 'dashed'),
  geom_vline(xintercept = forecast_dates$date_last_onset_50, linetype = 'dashed'),
  geom_vline(xintercept = forecast_dates$date_last_infection_50, linetype = 'dashed')
)

ggplot(tbl_transitions %>%
         filter(sim_ix <= 10)) +
  geom_line(aes(x = date, y = n, group = sim_ix),
            size = 0.1) +
  
  facet_wrap(~new_comp, scales = "free_y") +
  forecast_date_lines +
  
  theme_minimal()


ggplot(tbl_count %>%
         filter(sim_ix <= 10)) +
  geom_line(aes(x = date, y = count, group = sim_ix),
            size = 0.1) +
  
  facet_wrap(~compartment, scales = "free_y") +
  forecast_date_lines +
  
  theme_minimal()


ggplot(tbl_count_grouped %>%
         filter(sim_ix <= 100)) +
  geom_line(aes(x = date, y = count, group = sim_ix),
            size = 0.1) +
  
  facet_wrap(~group, scales = "free_y") +
  forecast_date_lines +
  
  theme_minimal()


ggplot(tbl_transitions %>%
         filter(new_comp == "symptomatic",
                sim_ix <= 100)) +
  geom_point(aes(x = date, y = n, group = sim_ix),
             size = 0.5) +
  forecast_date_lines +
  
  coord_cartesian(xlim = c(forecast_dates$date_last_onset_50 - 30,
                           forecast_dates$date_last_onset_50 + 28)) +
  
  theme_minimal()


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
    pivot_longer(cols = -c(date,group),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}


quant_count_grouped <- tbl_count_grouped %>%
  pivot_wider(names_from = "sim_ix", names_prefix = "sim_",
              values_from = "count") %>%
  mutate(group = replace_na(group, "other")) %>%
  make_quants()

c19data <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == state_modelled) %>% select(-c(state, state_abbrev)) %>%
  
  mutate(ward_cum = hosp_cum - icu_cum) %>%
  select(date, ward = ward_cum, ICU = icu_cum) %>%
  
  pivot_longer(cols = -c(date),
               values_to = "count", names_to = "group")

ggplot(quant_count_grouped) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_line(aes(x = date, y = count),
            c19data) +
  
  facet_wrap(~group, scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5) +
  
  theme_minimal()


ggplot(quant_count_grouped) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_line(aes(x = date, y = count),
            c19data) +
  
  facet_wrap(~group, scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5) +
  forecast_date_lines +
  
  coord_cartesian(xlim = c(max(quant_count_grouped$date) - 60,
                           max(quant_count_grouped$date))) +
  
  theme_minimal()

filter_grp <- . %>% filter(group %in% c("ICU", "ward"))
ggplot(quant_count_grouped %>% filter_grp) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  geom_line(aes(x = date, y = count),
            c19data %>% filter_grp) +
  
  facet_wrap(~group, scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5) +
  forecast_date_lines +
  
  coord_cartesian(xlim = c(max(quant_count_grouped$date) - 60,
                           max(quant_count_grouped$date))) +
  
  theme_minimal()
