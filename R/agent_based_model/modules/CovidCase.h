
#ifndef CovidCase_H
#define CovidCase_H

#include <string>

enum class CaseCompartment {Susceptible,
                            Symptomatic,
                            
                            Ward,
                            Ward_Died, Ward_Discharged,
                            
                            ICU,
                            ICU_Died,
                            
                            PostICU_to_Death,
                            PostICU_to_Discharge,
                            
                            PostICU_Died,
                            PostICU_Discharged};

struct CaseParameterSamples {
  double time_of_infection;
  
  double LoS_symptomatic_to_ED;
  
  double LoS_ward_to_discharge;
  double LoS_ward_to_death;
  double LoS_ward_to_ICU;
  
  double LoS_ICU_to_death;
  double LoS_ICU_to_postICU_death;
  double LoS_ICU_to_postICU_discharge;
  
  double LoS_postICU_to_death;
  double LoS_postICU_to_discharge;
  
  double pr_ICU;
  
  double pr_death_ward;
  double pr_death_ICU;
  double pr_death_postICU;
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
  CaseCompartment next_compartment;
  
  void transitionSusceptibleSymptomatic();
  
  void transitionSymptomaticWard();
  
  void transitionWardNext();
  void transitionWardICU();
  
  void transitionICUNext();
  void transitionICUPostICU();
  
  void transitionPostICUNext();
  
  
};

#endif