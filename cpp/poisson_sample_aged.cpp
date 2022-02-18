#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
std::vector<int> poisson_sample_aged(NumericVector x, int n_ages, double n_contacts) {
  std::vector<int> secondary_by_day(x.size() / n_ages);
  
  
  for(int i = 0; i < x.size(); i++) {
    secondary_by_day[i / n_ages] += R::rpois(x[i] * n_contacts); // probably just averages out lol
  }
  
  
  return secondary_by_day;
}