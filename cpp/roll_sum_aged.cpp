#include<Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
std::vector<int> roll_sum_aged(NumericVector x, int n_ages, int sum_width) {
  
  std::vector<int> results(x.length(), 0);
  for(int i = 0; i < x.length() / n_ages - sum_width + 1; i++) {
    for(int a = 0; a < n_ages; a++) {
      for(int s = 0; s < sum_width; s++) {
        
        results[i * n_ages + a] += x[i * n_ages + s * n_ages + a];
        
        
      }
    }
  }
  
  return results;
}