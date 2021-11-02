
#pragma once

#include <queue>
#include <case_structures.h>

class CovidCase;

class ClinicalQueue
{
public:
  
  ClinicalQueue(int daily_capacity, CaseCompartment next_compartment);
  
  void EnterQueue(CovidCase *covid_case);
  
  std::vector<CovidCase*> ProcessQueue(int t, int steps_per_day);
  
private:
  
  std::queue<CovidCase*> case_queue;
  int daily_capacity;
  CaseCompartment next_compartment;
  
};
