#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
std::vector<double> process_loop(List case_list, NumericVector initial_trigger_times) {
  int n_cases = case_list.size();
  
  std::vector<double> time_in_compartment(n_cases);
  std::vector<double> trigger_time(n_cases);
  
  for(int i = 0; i < n_cases; i++) {
    time_in_compartment[i] = 0;
    trigger_time[i] = initial_trigger_times[i];
  }
  
  
  double dt = 0.1;
  
  for(int t = 0; t < 365*10; t++) {
    for(int i = 0; i < n_cases; i++) {
      time_in_compartment[i] += dt;
      
      if(time_in_compartment[i] >= trigger_time[i]) {
        Environment case_i = case_list[i];
        
        Function trigger_transition_fn = case_i["trigger_transition"];
        trigger_transition_fn();
        
        // Reset the timer and determine the new trigger time
        time_in_compartment[i] = 0;
        Function get_compartment_threshold_time_fn = case_i["get_compartment_threshold_time"];
        trigger_time[i] = as<double>(get_compartment_threshold_time_fn());
      }
    }
  }
  
  
  
  return trigger_time;
}