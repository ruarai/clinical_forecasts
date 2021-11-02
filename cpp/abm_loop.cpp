#include <Rcpp.h>
#include "CovidCase.h"
#include "ClinicalQueue.h"

using namespace Rcpp;


void validate_case_param_matrix(NumericMatrix &case_param_matrix){
  CharacterVector col_names = colnames(case_param_matrix);
  
  CharacterVector expect_col_names = CharacterVector(
    {"time_of_infection", "LoS_symptomatic_to_ED", "LoS_ward_to_discharge", 
     "LoS_ward_to_death", "LoS_ward_to_ICU", "LoS_ICU_to_death", "LoS_ICU_to_postICU_death", 
     "LoS_ICU_to_postICU_discharge", "LoS_postICU_to_death", "LoS_postICU_to_discharge", 
     "pr_ICU", "pr_death_ward", "pr_death_ICU", "pr_death_postICU"}
  );
  
  bool is_correct = is_true(all(col_names == expect_col_names));
  
  if(!is_correct) {
    Rcout << "invalid case_param_matrix input\n";
  }
}


std::array<double, 3> create_plot_datapoint(int case_index, double t, int new_compartment) {
  std::array<double, 3> result;
  result[0] = case_index;
  result[1] = t;
  result[2] = new_compartment;
  
  return result;
}

void call_transition(CovidCase &covid_case, int case_ix,
                     double time_in_compartment[],
                     double next_compartment_trigger_time[],
                     
                     std::vector<std::array<double, 3>> &transitions,
                     NumericMatrix &compartment_counts,
                     
                     double t, double dt, int steps_per_day) {
  
  int old_compartment = static_cast<int>(covid_case.GetCurrentCompartment());
  
  covid_case.TriggerNextCompartment();
  
  int new_compartment = static_cast<int>(covid_case.GetCurrentCompartment());
  
  time_in_compartment[case_ix] = 0;
  next_compartment_trigger_time[case_ix] = covid_case.GetNextCompartmentTriggerTime();
  
  
  compartment_counts(t / steps_per_day, old_compartment) -= 1;
  compartment_counts(t / steps_per_day, new_compartment) += 1;
  
  std::array<double, 3> datapoint = create_plot_datapoint(case_ix, (double)t * dt, new_compartment);
  
  transitions.push_back(datapoint);
}

CaseParameterSamples create_case_params(NumericMatrix case_parameter_samples, int i) {
  CaseParameterSamples case_param_samples = CaseParameterSamples();
  case_param_samples.time_of_infection = case_parameter_samples(i, 0);
  
  case_param_samples.LoS_symptomatic_to_ED = case_parameter_samples(i, 1);
  case_param_samples.LoS_ward_to_discharge = case_parameter_samples(i, 2);
  case_param_samples.LoS_ward_to_death = case_parameter_samples(i, 3);
  case_param_samples.LoS_ward_to_ICU = case_parameter_samples(i, 4);
  case_param_samples.LoS_ICU_to_death = case_parameter_samples(i, 5);
  case_param_samples.LoS_ICU_to_postICU_death = case_parameter_samples(i, 6);
  case_param_samples.LoS_ICU_to_postICU_discharge = case_parameter_samples(i, 7);
  case_param_samples.LoS_postICU_to_death = case_parameter_samples(i, 8);
  case_param_samples.LoS_postICU_to_discharge = case_parameter_samples(i, 9);
  
  case_param_samples.pr_ICU = case_parameter_samples(i, 10);
  case_param_samples.pr_death_ward = case_parameter_samples(i, 11);
  case_param_samples.pr_death_ICU = case_parameter_samples(i, 12);
  case_param_samples.pr_death_postICU = case_parameter_samples(i, 13);
  
  return case_param_samples;
}

// [[Rcpp::export]]
List process_loop(NumericMatrix case_param_matrix,
                  int n_days, double dt, int ED_queue_capacity) {
  validate_case_param_matrix(case_param_matrix);
  
  
  int n_cases = case_param_matrix.nrow();
  CovidCase* case_array[n_cases];
  
  double time_in_compartment[n_cases];
  double next_compartment_trigger_time[n_cases];
  
  NumericMatrix compartment_counts(n_days, 12);
  ClinicalQueue* ED_queue = new ClinicalQueue(ED_queue_capacity, CaseCompartment::Ward);
  
  for(int i = 0; i < n_cases; i++) {
    CaseParameterSamples case_param_samples = create_case_params(case_param_matrix, i);
    
    case_array[i] = new CovidCase(i, "0-4", "none", case_param_samples, ED_queue);
    
    time_in_compartment[i] = 0;
    next_compartment_trigger_time[i] = case_array[i]->GetNextCompartmentTriggerTime();
    
    compartment_counts(0, 0) += 1;
  }
  
  
  std::vector<std::array<double, 3>> transitions(0);
  
  int steps_per_day = (int)(1 / dt);
  
  for(int t = 0; t < n_days * steps_per_day; t++) {
    
    for(int i = 0; i < n_cases; i++) {
      time_in_compartment[i] += dt;
      
      if(time_in_compartment[i] >= next_compartment_trigger_time[i]) {
        CovidCase& i_case = *case_array[i];
        
        call_transition(i_case, i,
                        time_in_compartment, next_compartment_trigger_time,
                        transitions, compartment_counts,
                        t, dt, steps_per_day);
      }
    }
    
    std::vector<CovidCase*> updated_ED_queue_cases = ED_queue->ProcessQueue(t, steps_per_day);
    
    for (auto & i_case : updated_ED_queue_cases) {
      int i = i_case->GetIndex();
      
      call_transition(*i_case, i,
                      time_in_compartment, next_compartment_trigger_time,
                      transitions, compartment_counts,
                      t, dt, steps_per_day);
    }
  }
  
  return List::create(
    _["transitions"] = transitions, 
    _["compartment_counts"] = compartment_counts
  );
}

