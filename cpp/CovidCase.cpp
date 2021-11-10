#include "CovidCase.h"
#include <Rcpp.h>
using namespace Rcpp;

// Constructor

CovidCase::CovidCase() { }
CovidCase::CovidCase(int ix,
                     std::string age_class, std::string vaccine_status,
                     CaseParameterSamples case_parameter_samples,
                     ClinicalQueue *ED_queue){
  this->ix = ix;
  
  this->age_class = age_class;
  this->vaccine_status = vaccine_status;
  
  this->compartment = CaseCompartment::Susceptible;
  
  this->case_parameter_samples = case_parameter_samples;
  this->next_compartment_trigger_time = case_parameter_samples.time_of_infection;
  
  
  this->ED_queue = ED_queue;
}

int CovidCase::GetIndex() { return(ix); }

double CovidCase::GetNextCompartmentTriggerTime() {
  return(next_compartment_trigger_time);
}
CaseCompartment CovidCase::GetCurrentCompartment() {
  return(compartment);
}

void CovidCase::TriggerNextCompartment() {
  switch(compartment) {
  case CaseCompartment::Susceptible:
    transitionSusceptibleSymptomatic();
    break;
    
  case CaseCompartment::Symptomatic:
    transitionSymptomaticWardQueue();
    break;
    
  case CaseCompartment::Symptomatic_WardQueue:
    transitionWardQueueWard();
    break;
    
  case CaseCompartment::Ward:
    transitionWardNext();
    break;
    
  case CaseCompartment::ICU:
    transitionICUNext();
    break;
    
  case CaseCompartment::PostICU:
    transitionPostICUNext();
    break;
    
  default:
    Rcout << "TriggerNextCompartment was called with no valid transition available\n";
    break;
  }
}



void CovidCase::transitionSusceptibleSymptomatic() {
  
  if(case_parameter_samples.pr_hosp == 1 ||
     R::runif(0, 1) <= case_parameter_samples.pr_hosp) {
    compartment = CaseCompartment::Symptomatic;
    next_compartment_trigger_time = case_parameter_samples.LoS_symptomatic_to_ED;
  } else{
    compartment = CaseCompartment::Symptomatic_NonClinical;
    next_compartment_trigger_time = std::numeric_limits<double>::infinity();
  }
  
  
}

void CovidCase::transitionSymptomaticWardQueue() {
  compartment = CaseCompartment::Symptomatic_WardQueue;
  
  ED_queue->EnterQueue(this);
  next_compartment_trigger_time = std::numeric_limits<double>::infinity();
}

void CovidCase::transitionWardQueueWard() {
  compartment = CaseCompartment::Ward;
  
  double pr_ward_to_ICU = case_parameter_samples.pr_ward_to_ICU;
  double pr_ward_to_discharge = case_parameter_samples.pr_ward_to_discharge;
  
  double pr_ward_to_death = 1 - pr_ward_to_ICU - pr_ward_to_discharge;
  
  // Ignoring the case where pr_ward_to_ICU is 1 as this means
  // we've manually passed in the value here
  if(pr_ward_to_death < 0 & pr_ward_to_ICU != 1) {
    Rcout << "invalid pr_ward_to_death\n"; 
  }
  
  double prob_sample = R::runif(0, 1);
  
  if(prob_sample <= pr_ward_to_ICU) {
    next_compartment = CaseCompartment::ICU;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_ICU;
    
  } else if(prob_sample <= pr_ward_to_ICU + pr_ward_to_discharge) {
    
    next_compartment = CaseCompartment::Ward_Discharged;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_discharge;
    
  } else {
    
    next_compartment = CaseCompartment::Ward_Died;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_death;
  }
  
}

void CovidCase::transitionWardNext() {
  
  switch(next_compartment) {
  case CaseCompartment::Ward_Died:
  case CaseCompartment::Ward_Discharged:
    compartment = next_compartment;
    next_compartment_trigger_time = std::numeric_limits<double>::infinity();
    
    break;
    
  case CaseCompartment::ICU:
    transitionWardICU();
    break;
    
  default:
    Rcout << "invalid next_compartment from Ward\n"; 
    break;
    
  }
}

void CovidCase::transitionWardICU() {
  compartment = CaseCompartment::ICU;
  
  double pr_ICU_to_discharge = case_parameter_samples.pr_ICU_to_discharge;
  double pr_ICU_to_postICU = case_parameter_samples.pr_ICU_to_postICU;
  double pr_ICU_to_death = 1 - pr_ICU_to_discharge - pr_ICU_to_postICU;
  
  if(pr_ICU_to_death < 0) {
    Rcout << "invalid pr_ICU_to_death\n"; 
  }
  
  
  double prob_sample = R::runif(0, 1);
  
  if(prob_sample <= pr_ICU_to_death) {
    
    next_compartment = CaseCompartment::ICU_Died;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_death;
    
  } else if(prob_sample <= pr_ICU_to_death + pr_ICU_to_postICU) {
    
    next_compartment = CaseCompartment::PostICU;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_postICU;
    
    
  } else {
    
    next_compartment = CaseCompartment::ICU_Discharged;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_discharge;
    
  }
}


void CovidCase::transitionICUNext() {
  
  switch(next_compartment) {
  case CaseCompartment::ICU_Died:
  case CaseCompartment::ICU_Discharged:
    compartment = next_compartment;
    next_compartment_trigger_time = std::numeric_limits<double>::infinity();
    
    break;
    
  case CaseCompartment::PostICU:
    transitionICUPostICU();
    break;
    
  default:
    Rcout << "invalid next_compartment from ICU\n"; 
  break;
  
  }
}

void CovidCase::transitionICUPostICU() {
  compartment = CaseCompartment::PostICU;
  
  double pr_postICU_to_death = case_parameter_samples.pr_postICU_to_death;
  
  
  if(R::runif(0, 1) < pr_postICU_to_death) {
    next_compartment = CaseCompartment::PostICU_Died;
    next_compartment_trigger_time = case_parameter_samples.LoS_postICU_to_death;
  } else{
    next_compartment = CaseCompartment::PostICU_Discharged;
    next_compartment_trigger_time = case_parameter_samples.LoS_postICU_to_discharge;
  }
}

void CovidCase::transitionPostICUNext() {
  compartment = next_compartment;
  next_compartment_trigger_time = std::numeric_limits<double>::infinity();
}
