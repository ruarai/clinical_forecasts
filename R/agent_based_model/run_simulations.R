


n_simulation <- 10




setwd("/usr/local/forecasting/source/covid19_aus_clinical_forecasting/")
source("R/model_parameters.R")
model_params <- get_model_parameters()

state_modelled <- "NSW"

date_0 <- lubridate::ymd("2021-06-01")

days_sim <- as.numeric(lubridate::today() - date_0) + 60

vaccination_prob_table <- read_rds("data/processed/vaccination_probability_table.rds")

vaccination_prob_table <- vaccination_prob_table %>%
  filter(state == state_modelled) %>%
  group_by(state, date, age_class) %>%
  mutate(proportion = case_when(name == "none" ~ 0.9,
                                TRUE ~ 0.1 * proportion),
         proportion = proportion / sum(proportion))



clinical_prob_table <- read_rds("data/processed/clinical_probabilities.rds")

clinical_linelist <- read_rds("data/processed/clinical_linelist_NSW.rds") %>%
  mutate(dt_onset = dt_hosp_admission - ddays(6),
         date_onset = date(dt_onset)) # No onset date in NSW data

case_linelist_with_vacc_prob <- clinical_linelist %>%
  mutate(state = state_modelled) %>%
  mutate(t_onset = (dt_onset - as.POSIXct(date_0)) / ddays(1),
         
         delay_onset_to_hospital = (dt_hosp_admission - dt_onset) / ddays(1),
         delay_ward_to_ICU = (dt_first_icu - dt_hosp_admission) / ddays(1),
         
         ix = row_number()) %>%
  filter(t_onset >= 0) %>%
  
  left_join(vaccination_prob_table, by = c("state", "age_class", "date_onset" = "date")) %>%
  left_join(clinical_prob_table) %>%
  
  mutate(pr_ICU = if_else(ever_in_icu, 1, 0))


# Could be repeated
case_linelist <- case_linelist_with_vacc_prob %>%
  group_by(ix) %>%
  
  slice_sample(n = 1, weight_by = proportion) %>%
  ungroup() %>%
  select(-c(ix, proportion)) %>% rename(vaccine = name)


delay_compartment_names <- colnames(model_params$delay_params$compartment_LoS_shape)

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

run_single_simulation <- function(i){
  
  case_delay_samples <- rgamma(length(case_delay_shapes),
                               shape = case_delay_shapes, rate = case_delay_shapes / case_delay_means) %>%
    matrix(ncol = ncol(case_delay_shapes)) %>%
    `colnames<-`(str_c("LoS_", delay_compartment_names))
  
  case_delay_samples[, "LoS_symptomatic_to_ED"] <- case_linelist$delay_onset_to_hospital
  case_delay_samples[, "LoS_ward_to_ICU"] <- case_linelist$delay_ward_to_ICU
  
  
  
  case_parameter_samples = cbind(
    time_of_infection = case_linelist$t_onset,
    case_delay_samples,
    pr_ICU = case_linelist$pr_ICU,
    
    pr_death_ward = case_pr_death_ward,
    pr_death_ICU = case_pr_death_ICU,
    pr_death_postICU = case_pr_death_postICU
  )
  
  
  Rcpp::sourceCpp("cpp/abm_loop.cpp")
  
  results <- process_loop(case_parameter_samples, days_sim,
                          dt = 0.1,
                          ED_queue_capacity = 5000)
  
  
  compartment_names_true <- c("susceptible", "symptomatic", "ED_queue", "ward",
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
                         "discharged" = c("ward_discharged", "postICU_discharged")) %>%
    map_dfr(~ tibble(compartment = .), .id="group")  %>%
    select(compartment, group) %>% deframe()
  
  tbl_count_grouped <- tbl_count %>%
    mutate(group = summary_groups[compartment]) %>%
    
    group_by(date, group) %>%
    summarise(count = sum(count), .groups = "drop")
  
  list(tbl_count = tbl_count %>% mutate(sim_ix = i),
       tbl_count_grouped = tbl_count_grouped %>% mutate(sim_ix = i),
       
       tbl_transitions = tbl_transitions %>% mutate(sim_ix = i))
}

library(future.callr)
library(furrr)
plan(callr)

a <- Sys.time()
if(n_simulation > 100) {
  results_all <- future_map(1:n_simulation - 1, run_single_simulation)
} else{
  results_all <- map(1:n_simulation - 1, run_single_simulation)
}

print(Sys.time() - a)


tbl_count <- map_dfr(results_all, function(r) r$tbl_count)
tbl_count_grouped <- map_dfr(results_all, function(r) r$tbl_count_grouped)
tbl_transitions <- map_dfr(results_all, function(r) r$tbl_transitions)


ggplot(tbl_transitions %>%
         filter(sim_ix <= 10)) +
  geom_line(aes(x = date, y = n),
            size = 0.1) +
  
  facet_wrap(~new_comp, scales = "free_y") +
  
  theme_minimal()


ggplot(tbl_count %>%
         filter(sim_ix <= 10)) +
  geom_line(aes(x = date, y = count),
            size = 0.1) +
  
  facet_wrap(~compartment, scales = "free_y") +
  
  theme_minimal()

plot_c19data_cumulative <- read_rds("data/covid19data.rds") %>%
  filter(state_abbrev == state_modelled,
         date >= date_0) %>%
  
  mutate(ward = hosp_cum - icu_cum,
         deaths_cum = deaths_cum - min(deaths_cum)) %>%
  select(date, ward, ICU = icu_cum, died = deaths_cum) %>%
  pivot_longer(cols = c(ward, ICU, died),
               names_to = "group", values_to = "count")

ggplot(tbl_count_grouped %>%
         filter(sim_ix <= 10)) +
  geom_line(aes(x = date, y = count),
            size = 0.1) +
  
  geom_line(aes(x = date, y = count),
            color = 'orangered',
            plot_c19data_cumulative ,
            size = 0.5) +
  
  facet_wrap(~group, scales = "free_y") +
  
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


ggplot(quant_count_grouped) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  # 
  # geom_line(aes(x = date, y = median),
  #           color = '#2ca25f',
  #           size = 0.5) +
  # 
  geom_line(aes(x = date, y = count),
            color = 'black',
            plot_c19data_cumulative ,
            size = 0.5) +
  
  facet_wrap(~group, scales = "free_y") +
  
  scale_fill_brewer(type = 'seq',
                    palette = 5) +
  
  theme_minimal()


quant_count_grouped %>%
  write_csv(paste0("results/test_1_", state_modelled, ".csv"))

