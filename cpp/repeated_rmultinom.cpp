#include <Rcpp.h>
using namespace Rcpp;

// Helper functions to use rmultinom (multinomial sampling)
// Size is the sample size, probs is the probability density across [0, 1, ..., probs.size()]
IntegerVector rmultinom_vec(unsigned int size, NumericVector &probs) {
  int N = probs.size();
  
  IntegerVector outcome(N);
  rmultinom(size, probs.begin(), N, outcome.begin());
  return outcome;
}

// [[Rcpp::export]]
std::vector<int> repeated_rmultinom(IntegerVector n, NumericVector probs) {
  std::vector<int> results(n.length() * probs.length());
  
  for(int i = 0; i < n.length(); i++) {
    std::vector<int> partial_results = as<std::vector<int>>(rmultinom_vec(n[i], probs));
    
    for(int j = 0; j < probs.size(); j++) {
      results[i * probs.size() + j] = partial_results[j];
    }
  }
  
  
  return results;
}
