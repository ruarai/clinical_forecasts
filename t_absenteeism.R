

source("R/absenteeism/absentee_case_trajectories.R")
source("R/absenteeism/smush.R")

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
  )
)