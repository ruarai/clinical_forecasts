# num_hosp_bigwindow = sum(strcmp(Health_Outcomes.STATE,state_modelled) & Health_Outcomes.HOSPITALISED==1 & Health_Outcomes.TRUE_ONSET_DATE>=date_l_window & Health_Outcomes.TRUE_ONSET_DATE<=date_r_window);
# not_hosp_bigwindow = Health_Outcomes.TRUE_ONSET_DATE(strcmp(Health_Outcomes.STATE,state_modelled) & Health_Outcomes.HOSPITALISED~=1 & Health_Outcomes.TRUE_ONSET_DATE>=date_l_window & Health_Outcomes.TRUE_ONSET_DATE<=date_r_window);
# 
# %case ages
# not_hosp_bigwindow_ages=Health_Outcomes.AGE_AT_ONSET(strcmp(Health_Outcomes.STATE,state_modelled) & Health_Outcomes.HOSPITALISED~=1 & Health_Outcomes.TRUE_ONSET_DATE>=date_l_window & Health_Outcomes.TRUE_ONSET_DATE<=date_r_window);
# 
# %cases into age brackets
# not_hosp_bigwindow_strata = not_hosp_bigwindow_ages;
# not_hosp_bigwindow_strata( not_hosp_bigwindow_strata==0) = 1;
# not_hosp_bigwindow_strata=min(ceil( not_hosp_bigwindow_strata/5),num_ages);

# dLLH_hosp_prob=@(x) num_hosp_bigwindow/x - sum(prob_observed_before_now./(1-x*prob_observed_before_now));



source("R/model_parameters.R")
model_params <- get_model_parameters()

window_size <- 14

fn_score <- function(x, A, days_since_onset, delay_shape, delay_mean) {
  prob_already_observed <- pgamma(days_since_onset, shape = delay_shape, scale = delay_mean / delay_shape)
  
  
  A / x - sum(prob_already_observed / (1 - x * prob_already_observed))
}


clinical_linelist <- read_rds("data/processed/clinical_linelist.rds") %>%
  filter(state == "VIC")


calculation_jobs <- expand_grid(
  date_end = seq(min(clinical_linelist$date_onset), max(clinical_linelist$date_onset), by = 'days'),
  age_class = unique(clinical_linelist$age_class)
) %>%
  mutate(date_start = date_end - window_size)

data_date <- today()


moving_window_hospitalisation <- function(date_start, date_end, age_class) {
  linelist_subset <- clinical_linelist %>%
    filter(date_onset >= date_start, date_onset <= date_end, age_class == !!age_class)
  
  num_hospitalised <- linelist_subset %>%
    filter(status_hospital == 1) %>%
    nrow()
  
  cases_not_hospitalised <- linelist_subset %>%
    filter(status_hospital != 1)
  
  days_since_onset <- as.numeric(data_date - cases_not_hospitalised$date_onset)
  
  delay_shape <- model_params$delay_params$mean_delay_by_age[age_class]
  delay_mean <- model_params$delay_params$shape_delay_by_age[age_class]
  
  if(nrow(linelist_subset) == 1 | num_hospitalised == 0 | nrow(cases_not_hospitalised) == 0){
    return(0)
  } 
  
  pracma::fzero(
    function(x) {fn_score(x, num_hospitalised, days_since_onset, delay_shape, delay_mean)},
    
    x = c(0+.Machine$double.eps,1-.Machine$double.eps),
  )$x
}

library(future.callr)
library(furrr)
plan(callr)

prob_values <- future_pmap_dbl(calculation_jobs, moving_window_hospitalisation)

results <- tibble(calculation_jobs, probability_hosp = prob_values)



ggplot(results) +
  geom_line(aes(x = date_start, y = probability_hosp, group = age_class, color = age_class))


