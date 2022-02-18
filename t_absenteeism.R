

source("R/absenteeism/absentee_case_trajectories.R")
source("R/absenteeism/smush.R")
source("R/absenteeism/full_plots.R")

t_state_absenteeism <- list(
  tar_target(
    absentee_case_trajectories,
    
    make_absentee_case_trajectories(
      ensemble_state,
      local_cases_state,
      nindss_state,
      
      forecast_dates
    )
  ),
  
  tar_target(
    n_isolated,
    
    do_smush(
      absentee_case_trajectories,
      forecast_dates
    )
  ),
  
  tar_target(
    absentee_trajs,
    
    {
      age_groups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
      
      
      n_isolated %>%
        as_tibble(.name_repair = "universal") %>%
        mutate(age_group =  rep(age_groups, length.out = nrow(n_isolated)),
               t = rep(1:(nrow(n_isolated) / 9), each = 9) - 1,
               date = t + forecast_dates$simulation_start,
               state = state_modelled)
    }
    
  )
)
