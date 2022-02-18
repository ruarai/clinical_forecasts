
do_smush <- function(
  absentee_case_trajectories,
  forecast_dates
) {
    
  
  source("R/absenteeism/traj_plotting.R")
  
  # 
  # plot_trajs(absentee_case_trajectories) +
  #   ggtitle("Total cases by age group (backcast and forecast)")
  # 
  # plot_trajs(absentee_case_trajectories, do_sum = TRUE) +
  #   ggtitle("Total cases (backcast and forecast)")
  # 
  source("R/demography.R")
  
  contact_mat <- get_contact_matrix()
  
  contacts_by_age <- rep(1.5, times = 9)
  pr_contact <- apply(contact_mat, 2, FUN = function(x) x / sum(x))
  
  
  Rcpp::sourceCpp("cpp/sample_contacts.cpp")
  secondary_iso_contacts_age_sampled <- apply(
    absentee_case_trajectories,
    2,
    function(x) sample_contacts(x, n_ages = 9, contacts_by_age, pr_contact)
  )
  
  # 
  # plot_trajs(secondary_iso_contacts_age_sampled) +
  #   ggtitle("Primary contacts by age group (backcast and forecast)")
  # 
  # plot_trajs(secondary_iso_contacts_age_sampled, do_sum = TRUE) +
  #   ggtitle("Total contacts (backcast and forecast)")
  # 
  
  age_demography <- get_age_distribution_by_state() %>%
    filter(state == "NSW") %>%
    mutate(workforce_participation = c(0, 0.27006, 0.83684, 0.862148, 0.863779, 0.799347, 0.46397, 0.115252, 0.02064),
           working_population = workforce_participation * population)
  
  
  pr_wfh <- 0.4
  pr_asymptomatic <- 0.1147541
  pr_asymp_and_wfh <- pr_wfh * 0.1147541
  direct_absentee_case_trajectories <- apply(
    absentee_case_trajectories,
    2,
    function(x) rbinom(length(x), x, age_demography$workforce_participation * (1 - pr_asymp_and_wfh)) 
  )
  
  contact_absentee_case_trajectories <- apply(
    secondary_iso_contacts_age_sampled,
    2,
    function(x) rbinom(length(x), x, age_demography$workforce_participation * (1 - pr_wfh)) # 40% of contacts are WFH, not included
  )
  
  
  worker_absentee_case_trajectories <- direct_absentee_case_trajectories + contact_absentee_case_trajectories
  
  # 
  # plot_trajs(worker_absentee_case_trajectories) +
  #   ggtitle("Total workforce beginning isolation by age group (backcast and forecast)")
  # 
  # 
  Rcpp::sourceCpp("cpp/roll_sum_aged.cpp")
  worker_absentee_n_isolated <- apply(
    worker_absentee_case_trajectories,
    2,
    function(x) rev(roll_sum_aged(rev(x), 9, 7))
  )
  # 
  # 
  # plot_trajs(worker_absentee_n_isolated) +
  #   
  #   ggtitle("Isolated workers by age group (backcast and forecast), 7 day isolation period")
  # 
  # plot_trajs(worker_absentee_n_isolated, do_sum = TRUE) +
  #   
  #   ggtitle("Total isolated workers (backcast and forecast), 7 day isolation period")
  # 
  
  worker_absentee_n_isolated
}