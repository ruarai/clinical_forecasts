#include <Rcpp.h>
#include "modules/CovidCase.h"

using namespace Rcpp;


std::array<double, 3> create_plot_datapoint(int case_index, double t, int new_compartment) {
  std::array<double, 3> result;
  result[0] = case_index;
  result[1] = t;
  result[2] = new_compartment;
  
  return result;
}

// [[Rcpp::export]]
List process_loop(NumericMatrix case_parameter_samples,
                  int n_days, double dt) {
  int n_cases = case_parameter_samples.nrow();
  CovidCase* case_array[n_cases];
  
  double time_in_compartment[n_cases];
  double next_compartment_trigger_time[n_cases];
  
  NumericMatrix compartment_counts(n_days, 11);
  
  for(int i = 0; i < n_cases; i++) {
    CaseParameterSamples case_param_samples = CaseParameterSamples();
    case_param_samples.time_of_infection = case_parameter_samples(i, 0);
    case_param_samples.LoS_symptomatic_to_ED = case_parameter_samples(i, 1);
    case_param_samples.LoS_ward_to_discharge = case_parameter_samples(i, 2);
    
    case_array[i] = new CovidCase("0-4", "none", case_param_samples);
    
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
        CovidCase* i_case = case_array[i];
        
        int old_compartment = static_cast<int>(i_case->GetCurrentCompartment());
        
        i_case->TriggerNextCompartment();
        
        int new_compartment = static_cast<int>(i_case->GetCurrentCompartment());
        
        time_in_compartment[i] = 0;
        next_compartment_trigger_time[i] = i_case->GetNextCompartmentTriggerTime();
        
        
        compartment_counts(t / steps_per_day, old_compartment) -= 1;
        compartment_counts(t / steps_per_day, new_compartment) += 1;
        transitions.push_back(create_plot_datapoint(i, (double)t * dt, new_compartment));
      }
    }
  }
  
  return List::create(
    _["transitions"] = transitions, 
    _["compartment_counts"] = compartment_counts
  );
}

