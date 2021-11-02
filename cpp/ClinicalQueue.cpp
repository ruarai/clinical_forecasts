#include "CovidCase.h"
#include "ClinicalQueue.h"
#include <Rcpp.h>
using namespace Rcpp;





ClinicalQueue::ClinicalQueue(int daily_capacity, CaseCompartment next_compartment) {
  this->daily_capacity = daily_capacity;
  this->next_compartment = next_compartment;
}


void ClinicalQueue::EnterQueue(CovidCase *covid_case) {
  this->case_queue.push(covid_case);
}

std::vector<CovidCase*> ClinicalQueue::ProcessQueue(int t, int steps_per_day) {
  
  std::vector<CovidCase*> updated_cases;
  
  int capacity_step = this->daily_capacity / steps_per_day;

  for(int i = 0; i < capacity_step && i < this->case_queue.size(); i++) {
    CovidCase *covid_case = this->case_queue.front();
    this->case_queue.pop();
    
    updated_cases.push_back(covid_case);
  }
    
  
  return updated_cases;
}