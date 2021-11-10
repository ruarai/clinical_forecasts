#pragma once

enum class CaseCompartment {Susceptible,
                            Symptomatic,
                            
                            Symptomatic_NonClinical,
                            Symptomatic_WardQueue,
                            
                            Ward,
                            Ward_Died, Ward_Discharged,
                            
                            ICU,
                            ICU_Died,
                            ICU_Discharged,
                            
                            PostICU,
                            
                            PostICU_Died,
                            PostICU_Discharged};

struct CaseParameterSamples {
  double time_of_infection;
  
  double LoS_symptomatic_to_ED;
  
  double LoS_ward_to_discharge;
  double LoS_ward_to_death;
  double LoS_ward_to_ICU;
  
  double LoS_ICU_to_postICU;
  double LoS_ICU_to_discharge;
  double LoS_ICU_to_death;
  
  double LoS_postICU_to_death;
  double LoS_postICU_to_discharge;
  
  double pr_hosp;
  double pr_ICU;
  
  double pr_ward_to_discharge;
  double pr_ward_to_ICU;
  double pr_ICU_to_discharge;
  double pr_ICU_to_postICU;
  double pr_postICU_to_death;
};