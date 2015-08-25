#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix brLoop(NumericVector x) {
  int i, k;
  int n = x.size();
  float m = 0.0, sum = 0.0;
  
  for (i = 0; i < n; ++i) {
    sum += x[i];
  }
  
  NumericMatrix out(n, 2);
  for (i = 0; i < n; ++i) {
    for (k = i + 1; k < n + 1; ++k) {
      m = m + (1.0 / n) * (1.0 / k);
    }
    out(i, 0) = (x[i] / sum) * 100.0;
    out(i, 1) = m * 100.0;
    m = 0;
  }
  
  return out;
}

/*** R
summary(pc.cr <- princomp(USArrests, cor = TRUE))
system.time(brLoop(pc.cr$sdev ** 2))
*/
