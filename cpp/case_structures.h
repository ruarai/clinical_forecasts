#pragma once

enum class CaseCompartment {Susceptible,
                            Symptomatic,
                            
                            Symptomatic_WardQueue,
                            
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