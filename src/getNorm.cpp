#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getNorm(NumericMatrix x) {
  int x_col = x.ncol();
  int x_row = x.nrow();
  int i, j;
  
  NumericVector out(x_col);
  out[0] = 0;
  
  for (j = 0; j < x_col; ++j) {
    double result = 0;
    for (i = 0; i < x_row; ++i) {
      result = result + (x(i, j) * x(i, j));
    }
    out[j] = out[j] + std::sqrt(result);
  }
   return out;
}
