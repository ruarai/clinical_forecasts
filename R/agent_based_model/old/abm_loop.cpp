
#include <Rcpp.h>
using namespace Rcpp;


std::array<double, 3> create_plot_datapoint(int case_index, double t, int new_compartment) {
  std::array<double, 3> result;
  result[0] = case_index;
  result[1] = t;
  result[2] = new_compartment;
  
  return result;
}

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
List process_loop(List case_list,
                  int days_sim,
                  double dt) {
  int n_cases = case_list.size();
  
  std::vector<double> time_in_compartment(n_cases);
  std::vector<double> trigger_time(n_cases);
  NumericMatrix compartment_counts(days_sim, 11);
  
  for(int i = 0; i < n_cases; i++) {
    time_in_compartment[i] = 0;
    
    
    Environment case_i = case_list[i];
    
    Function get_current_compartment_fn = case_i["get_current_compartment"];
    int start_compartment = as<int>(get_current_compartment_fn());
    compartment_counts(0, start_compartment - 1) += 1;
    
    Function get_trigger_time_fn = case_i["get_compartment_threshold_time"];
    
    trigger_time[i] = as<double>(get_trigger_time_fn());
  }
  
  std::vector<std::array<double, 3>> transitions(0);
  
  int steps_per_day = (int)(1 / dt);
  
  for(int t = 0; t < days_sim * steps_per_day; t++) {
    for(int i = 0; i < n_cases; i++) {
      time_in_compartment[i] += dt;
      
      if(time_in_compartment[i] >= trigger_time[i]) {
        Environment case_i = case_list[i];
        
        Function get_current_compartment_fn = case_i["get_current_compartment"];
        int old_compartment = as<int>(get_current_compartment_fn());
        
        Function trigger_transition_fn = case_i["trigger_transition"];
        int new_compartment = as<int>(trigger_transition_fn());
        
        // Reset the timer and determine the new trigger time
        time_in_compartment[i] = 0;
        Function get_compartment_threshold_time_fn = case_i["get_compartment_threshold_time"];
        trigger_time[i] = as<double>(get_compartment_threshold_time_fn());
        
        transitions.push_back(create_plot_datapoint(i, t, new_compartment));
        
        compartment_counts(t / steps_per_day, old_compartment - 1) -= 1;
        compartment_counts(t / steps_per_day, new_compartment - 1) += 1;
      }
    }
  }
  
  
  return List::create(
    _["transitions"] = transitions, 
    _["compartment_counts"] = compartment_counts
  );
}


