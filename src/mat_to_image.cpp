#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

// x - matrix of face vectors
// dim - the dimension of the single face matrix
// [[Rcpp::export]]
arma::Cube<double> mat_to_image(NumericMatrix x, NumericVector dim) {
  int i, j, n;
  int slice = x.cols();
  arma::mat rawMat = as<arma::mat>(x);
  
  arma::cube rawImages = arma::zeros(dim[0], dim[1], slice);
  for (i = 0; i < dim[0]; ++i) {
    for (j = 0; j < dim[1]; ++j) {
      n = (i) * dim[1] + j;
      rawImages(span(i), arma::span(j), arma::span(0, slice - 1)) = rawMat(n, arma::span(0, slice - 1));
    }
  }
  
  return rawImages;
}

/*** R
raw1 <- matrix(c(1,2,3,4), 2, 2)
raw2 <- matrix(c(5,6,7,8), 2, 2)
raw <- cbind(c(raw1), c(raw2))
d <- dim(raw1)
raw
system.time(rawImages <- mat_to_image(raw, d))
*/
