
#ifndef CovidCase_H
#define CovidCase_H

#include <string>

enum class CaseCompartment {Susceptible, Symptomatic, Ward};

struct CaseParameterSamples {
  double time_of_infection;
  double LoS_symptomatic_to_ED;
  double LoS_ward_to_discharge;
  double LoS_ward_to_death;
  double LoS_ward_to_ICU;
};

class CovidCase
{
public:
  
  // Constructor
  CovidCase();
  CovidCase(std::string age_class, std::string vaccine_status, CaseParameterSamples caseSamples);
  
  double GetNextCompartmentTriggerTime();
  void TriggerNextCompartment();
  
  CaseCompartment GetCurrentCompartment();
  
private:
  
  
  std::string age_class;
  std::string vaccine_status;
  
  double next_compartment_trigger_time;
  
  CaseParameterSamples case_parameter_samples;
  
  CaseCompartment compartment;
  
  void transitionSusceptibleSymptomatic();
  void transitionSymptomaticWard();
};

#endif