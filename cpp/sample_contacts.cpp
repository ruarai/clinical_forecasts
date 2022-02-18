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
std::vector<int> sample_contacts(
    NumericVector x,
    int n_ages,
    
    NumericVector contact_rate,
    NumericMatrix pr_contact
) {
  std::vector<int> results(x.size(), 0);
  
  int max_t = x.size() / n_ages;
  
  for(int t = 0; t < max_t; t++) {
    for(int a = 0; a < n_ages; a++) {
      NumericVector age_pr_contact = pr_contact(_, a);
      double age_contact_rate = contact_rate(a);
      
      int n_contacts = R::rpois(x[t * n_ages + a] * age_contact_rate);
      
      IntegerVector contacts_made = rmultinom_vec(n_contacts, age_pr_contact);
      
      for(int j = 0; j < n_ages; j++)
        results[t * n_ages + j] += contacts_made[j];
    }
    
  }
  
  
  
  return results;
}
