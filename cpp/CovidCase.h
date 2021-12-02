
#pragma once

#include <string>
#include <case_structures.h>
#include <ClinicalQueue.h>


class CovidCase
{
public:
  
  CovidCase();
  CovidCase(int ix,
            std::string age_class, std::string vaccine_status,
            CaseParameterSamples caseSamples,
            ClinicalQueue *ED_queue);
  
  int GetIndex();
  
  double GetNextCompartmentTriggerTime();
  void TriggerNextCompartment();
  
  void TriggerInactive();
  
  double GetActiveChangeTriggerTime();
  
  CaseCompartment GetCurrentCompartment();
  
  bool IsActiveCase();
  
private:
  
  int ix;
  
  std::string age_class;
  std::string vaccine_status;
  
  double next_compartment_trigger_time;
  double active_change_trigger_time;
  
  bool is_active_case;
  
  CaseParameterSamples case_parameter_samples;
  
  CaseCompartment compartment;
  CaseCompartment next_compartment;
  
  void transitionSusceptibleSymptomatic();
  
  void transitionSymptomaticWardQueue();
  void transitionWardQueueWard();
  
  void transitionWardNext();
  void transitionWardICU();
  
  void transitionICUNext();
  void transitionICUPostICU();
  
  void transitionPostICUNext();
  
  ClinicalQueue *ED_queue;
  
};
