
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
std::vector<std::array<double, 3>> process_loop(List case_list, NumericVector initial_trigger_times) {
  int n_cases = case_list.size();
  
  std::vector<double> time_in_compartment(n_cases);
  std::vector<double> trigger_time(n_cases);
  
  for(int i = 0; i < n_cases; i++) {
    time_in_compartment[i] = 0;
    trigger_time[i] = initial_trigger_times[i];
  }
  
  std::vector<std::array<double, 3>> transitions(0);
  
  double dt = 0.1;
  
  for(int t = 0; t < 60*10; t++) {
    for(int i = 0; i < n_cases; i++) {
      time_in_compartment[i] += dt;
      
      if(time_in_compartment[i] >= trigger_time[i]) {
        Environment case_i = case_list[i];
        
        Function trigger_transition_fn = case_i["trigger_transition"];
        int new_compartment = as<int>(trigger_transition_fn());
        
        // Reset the timer and determine the new trigger time
        time_in_compartment[i] = 0;
        Function get_compartment_threshold_time_fn = case_i["get_compartment_threshold_time"];
        trigger_time[i] = as<double>(get_compartment_threshold_time_fn());
        
        transitions.push_back(create_plot_datapoint(i, t, new_compartment));
      }
    }
  }
  
  
  
  return transitions;
}


