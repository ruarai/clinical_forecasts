

# Handles maximum-likelihood estimation of P_hosp and P_ICU
# Calculating these estimates over a moving window

source("R/model_parameters.R")
model_params <- get_model_parameters()

window_size <- 14

fn_score <- function(x, A, days_since_onset, delay_shape, delay_mean) {
  prob_already_observed <- pgamma(days_since_onset, shape = delay_shape, scale = delay_mean / delay_shape)
  
  
  A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
}

# TODO Filter for state here too
date_last_onset_50 <- read_csv("data/input/local_cases_input.csv") %>%
  filter(detection_probability > 0.5) %>% pull(date_onset) %>% max()


clinical_linelist <- read_rds("data/processed/clinical_linelist.rds") %>%
  filter(state == "VIC",
         date_onset <= date_last_onset_50,
         date_onset >= ymd("2021-Jun-01"))






# Put more thought into this
data_date <- read_rds("data/processed/clinical_linelist.rds") %>%
  pull(date_onset) %>% max() - 1

get_linelist_subset <- function(date_start, n_days_forward, age_class) {
  if(is.null(age_class)) {
    return(clinical_linelist %>%
             filter(date_onset >= date_start, date_onset <= date_start + n_days_forward))
  }
  else{
    return(clinical_linelist %>%
             filter(date_onset >= date_start, date_onset <= date_start + n_days_forward,
                    age_class == !!age_class))
  }
}

moving_window_hospitalisation <- function(date_start) {
  
  n_days_forward <- 14
  linelist_subset <- get_linelist_subset(date_start, n_days_forward)
  
  while(nrow(linelist_subset) <= 50 | n_days_forward >= 120) {
    n_days_forward <- n_days_forward + 1
    linelist_subset <- get_linelist_subset(date_start, n_days_forward)
  }
  
  num_hospitalised <- linelist_subset %>%
    filter(status_hospital == 1) %>%
    nrow()
  
  cases_not_hospitalised <- linelist_subset %>%
    filter(status_hospital != 1)
  
  days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
  
  delay_shape <- model_params$delay_params$compartment_LoS_mean[cases_not_hospitalised$age_class, "symptomatic_to_ED"]
  delay_mean <- model_params$delay_params$compartment_LoS_shape[cases_not_hospitalised$age_class, "symptomatic_to_ED"]
  
  if(nrow(linelist_subset) <= 50 | num_hospitalised == 0 | nrow(cases_not_hospitalised) == 0){
    return(tibble_row(prob_naive = -1, prob_MLE = - 1))
  } 
  
  prob_MLE <- tryCatch(pracma::fzero(
    function(x) {fn_score(x, num_hospitalised, days_since_onset, delay_shape, delay_mean)},
    
    x = c(0+.Machine$double.eps,1-.Machine$double.eps),
  )$x,
  error = function(c) { return(-1) }) # fzero didn't find anything. return -1
  
  tibble_row(
    prob_naive = num_hospitalised / nrow(linelist_subset),
    prob_MLE = prob_MLE
  )
}

calculation_jobs <- expand_grid(
  date_start = seq(min(clinical_linelist$date_onset), max(clinical_linelist$date_onset), by = 'days')
)

results <- pmap_dfr(calculation_jobs, moving_window_hospitalisation) %>%
  bind_cols(calculation_jobs, .)  %>%
  mutate(date_onset = date_start + window_size / 2)


cutoff_date <- max(results$date_onset) - 35


results_forecast <- results %>%
  
  filter(prob_MLE != -1,
         date_onset <= cutoff_date) %>%
  
  select(date_onset, prob_hosp = prob_MLE) %>%
  right_join(
    tibble(date_onset = seq(min(results$date_onset), max(results$date_onset) + 28, by = 'days'))
    ) %>%
  arrange(date_onset) %>%
  mutate(prob_hosp = zoo::na.approx(prob_hosp, na.rm = FALSE))

ggplot() +
  geom_line(aes(x = date_onset, y = prob_hosp),
            results_forecast) +
  geom_line(aes(x = date_start + window_size / 2, y = prob_naive),
            results %>% filter(prob_naive != -1),
            linetype = 'dotted') +
  
  geom_vline(xintercept = cutoff_date) +
  
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.3))


results_forecast %>%
  write_rds("data/processed/probability_hospitalisation.rds")



