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
double distC(NumericMatrix A) {
  int nr=A.nrow();
  NumericVector dist_temp(nr);
  NumericVector distance(nr);
  for (int i=0; i<nr; i++) {
    for (int j=0; j<nr; j++) {
  //     // if (result$`Centroid id`[i] == result$`Centroid id`[j]) {dist_temp[j] <- Inf}
      if (A(i,1) == A(j,1)) {
        dist_temp[j] = R_PosInf;
  //     // else dist_temp[j] <- euclid_norm(c(result$x[i],result$y[i])-c(result$x[j],result$y[j]))
      } else {
        dist_temp[j] = sqrt(pow(A(i,3) - A(j,3),2.0) + pow(A(i,4) - A(j,4),2.0));
      }
    }
    distance[i] = min(dist_temp);
  }
  return min(distance);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# distC(
#   result %>% data.matrix()
# )
*/
