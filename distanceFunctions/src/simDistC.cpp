#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector simDistC(NumericMatrix A) {
  int nr=A.nrow();
  NumericVector distance(nr);
  for (int i=0; i<nr; i++) {
    distance[i] = sqrt(pow(A(i,0) - A(i,2),2.0) + pow(A(i,1) - A(i,3),2.0));
  }
  return distance;
}
