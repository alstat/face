#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rbrLoop(NumericVector x) {
  int x_size = x.size();
  int i, j, k;
  
  NumericMatrix out(x_size, 2);
  for (i = 0; i < x_size; ++i) {
    double n = 0, m = 0;
    
    for (j = 0; j < (x_size - i); ++j) {
      m = m + (1.0 / (x_size - i)) * (1.0 / (j + 1)); 
    }
    for (k = i; k < x_size; ++k) {
      n = n + x[k];
    }
    
    out(i, 0) = x[i] / n;
    out(i, 1) = m;
  }
  
  return(out);
}

/***R
summary(pc.cr <- princomp(USArrests, cor = TRUE))
rbrStick(pc.cr$sdev ** 2)
rbrLoop(pc.cr$sdev ** 2)
*/