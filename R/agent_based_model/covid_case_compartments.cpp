#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double timesTwo() {
  return R::runif(0.0, 1.0);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
a <- timesTwo()
*/
