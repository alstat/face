#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace arma;

// x - list of face vectors
// nrow - number of rows of face vectors
// ncol - number of cols of face vectors
// [[Rcpp::export]]
arma::Mat<double> mat_of_faces(List x, int nrow, int ncol) {
  int i;
  
  arma::mat faces = arma::zeros<arma::mat>(nrow, ncol);
  for (i = 0; i < ncol; ++i) {
    faces(arma::span(0, nrow - 1), arma::span(i)) = as<arma::mat>(x[i]);
  }
  return faces;
}

/*** R
raw1 <- matrix(c(1,2,3,4), 2, 2)
raw2 <- matrix(c(5,6,7,8), 2, 2)
rawlist <- list(as.matrix(c(raw1)), as.matrix(c(raw2)))
nrow <- nrow(rawlist[[1]])
ncol <- length(rawlist)
mat_of_faces(rawlist, nrow, ncol)


*/
