#include <Rcpp.h>

#include "CovidCase.h"

// Expose (some of) the Student class
RCPP_MODULE(RcppStudentEx){
  Rcpp::class_<CovidCase>("CovidCase")
  .constructor<std::string, std::string, double>()
  .method("TriggerNextCompartment", &CovidCase::TriggerNextCompartment);
}