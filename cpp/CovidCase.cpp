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
    
  case CaseCompartment::PostICU_to_Death:
  case CaseCompartment::PostICU_to_Discharge:
    transitionPostICUNext();
    break;
    
  default:
    Rcout << "TriggerNextCompartment was called with no valid transition available\n";
    break;
  }
}



void CovidCase::transitionSusceptibleSymptomatic() {
  compartment = CaseCompartment::Symptomatic;
  next_compartment_trigger_time = case_parameter_samples.LoS_symptomatic_to_ED;
}

void CovidCase::transitionSymptomaticWardQueue() {
  compartment = CaseCompartment::Symptomatic_WardQueue;
  
  ED_queue->EnterQueue(this);
  next_compartment_trigger_time = std::numeric_limits<double>::infinity();
}

void CovidCase::transitionWardQueueWard() {
  compartment = CaseCompartment::Ward;
  
  double pr_ward_to_death = case_parameter_samples.pr_death_ward;
  double pr_ward_to_ICU = case_parameter_samples.pr_ICU;
  
  double pr_ward_to_discharge = 1 - pr_ward_to_death - pr_ward_to_ICU;
  
  if(pr_ward_to_discharge < 0) {
    Rcout << "invalid pr_ward_to_discharge\n"; 
  }
  
  double prob_sample = R::runif(0, 1);
  
  if(prob_sample <= pr_ward_to_death) {
    
    next_compartment = CaseCompartment::Ward_Died;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_death;
    
  } else if(prob_sample <= pr_ward_to_death + pr_ward_to_discharge) {
    
    next_compartment = CaseCompartment::Ward_Discharged;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_discharge;
    
  } else {
    
    next_compartment = CaseCompartment::ICU;
    next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_ICU;
    
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
  
  double pr_ICU_to_death = case_parameter_samples.pr_death_ICU;
  double pr_ICU_to_postICU_death = case_parameter_samples.pr_death_postICU;
  double pr_ICU_to_postICU_discharge = 1 - pr_ICU_to_death - pr_ICU_to_postICU_discharge;
  
  if(pr_ICU_to_postICU_discharge < 0) {
    Rcout << "invalid pr_ICU_to_postICU_discharge\n"; 
  }
  
  
  double prob_sample = R::runif(0, 1);
  
  if(prob_sample <= pr_ICU_to_death) {
    
    next_compartment = CaseCompartment::ICU_Died;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_death;
    
  } else if(prob_sample <= pr_ICU_to_death + pr_ICU_to_postICU_discharge) {
    
    next_compartment = CaseCompartment::PostICU_to_Discharge;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_postICU_discharge;
    
    
  } else {
    
    next_compartment = CaseCompartment::PostICU_to_Death;
    next_compartment_trigger_time = case_parameter_samples.LoS_ICU_to_postICU_death;
    
  }
}


void CovidCase::transitionICUNext() {
  
  switch(next_compartment) {
  case CaseCompartment::ICU_Died:
    compartment = CaseCompartment::ICU_Died;
    next_compartment_trigger_time = std::numeric_limits<double>::infinity();
    
    break;
    
  case CaseCompartment::PostICU_to_Death:
  case CaseCompartment::PostICU_to_Discharge:
    transitionICUPostICU();
    break;
    
  default:
    Rcout << "invalid next_compartment from ICU\n"; 
  break;
  
  }
}

void CovidCase::transitionICUPostICU() {
  switch(next_compartment) {
  
  case CaseCompartment::PostICU_to_Death:
    compartment = CaseCompartment::PostICU_to_Death;
    next_compartment = CaseCompartment::PostICU_Died;
    
    next_compartment_trigger_time = case_parameter_samples.LoS_postICU_to_death;
    
    break;
  case CaseCompartment::PostICU_to_Discharge:
    compartment = CaseCompartment::PostICU_to_Discharge;
    next_compartment = CaseCompartment::PostICU_Discharged;
    
    next_compartment_trigger_time = case_parameter_samples.LoS_postICU_to_discharge;
    
    break;
    
  default:
    Rcout << "invalid next_compartment within ICUPostICU transition\n";
  }
}

void CovidCase::transitionPostICUNext() {
  compartment = next_compartment;
  next_compartment_trigger_time = std::numeric_limits<double>::infinity();
}
