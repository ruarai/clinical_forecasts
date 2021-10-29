#include "CovidCase.h"
#include <Rcpp.h>
using namespace Rcpp;

// Constructor

CovidCase::CovidCase() { }
CovidCase::CovidCase(std::string age_class, std::string vaccine_status,
                     CaseParameterSamples case_parameter_samples){
  this->age_class = age_class;
  this->vaccine_status = vaccine_status;
  
  this->compartment = CaseCompartment::Susceptible;
  
  this->case_parameter_samples = case_parameter_samples;
  this->next_compartment_trigger_time = case_parameter_samples.time_of_infection;
}

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
    transitionSymptomaticWard();
    break;
    
  case CaseCompartment::Ward:
    next_compartment_trigger_time = 100000;
    break;
  }
}



void CovidCase::transitionSusceptibleSymptomatic() {
  compartment = CaseCompartment::Symptomatic;
  next_compartment_trigger_time = case_parameter_samples.LoS_symptomatic_to_ED;
}

void CovidCase::transitionSymptomaticWard() {
  compartment = CaseCompartment::Ward;
  
  //next_compartment_trigger_time = case_parameter_samples.LoS_ward_to_discharge;
  
  double pr_ward_to_death = 0.1;
  double pr_ward_to_ICU = 0.3;
  
  double pr_ward_to_discharge = 1 - pr_ward_to_death - pr_ward_to_ICU;
  
  
  
}



