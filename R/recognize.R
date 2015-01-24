#' Object Recognition
#' @export
#' 
#' @description
#' This function uses PCA to compute the eigenfaces and recognizes the data.
#' 
#' @param x a data frame consisting the suitability scores of a given characteristics
#'          (terrain, soil, water and temperature) for a 
#'          given crop (e.g. coconut, cassava, etc.);
#' @param method the method for computing the overall suitability, which includes the
#'        \code{"minimum"}, \code{"maximum"}, \code{"sum"}, \code{"product"},
#'        \code{"average"}, \code{"exponential"} and \code{"gamma"}. If \code{NULL},
#'        \code{"minimum"} is used.
#' @param interval if \code{NULL}, the interval used are the following: 0-25% (Not
#'        suitable, N), 25%-50% (Marginally Suitable, S3), 50%-75% (Moderately Suitable, S2), and
#'        75%-100% (Highly Suitable, S1). But users can assign a custom intervals by specificying
#'        the values of the end points of the intervals.
#' @param output the output to be returned, either the scores or class. If \code{NULL},
#'        both are returned.
#' 
#' @examples
#' library(or)
#' 
#' rawData <- list()
#' for (i in 1:101) {
#'   rawData[[i]] <- melt(t(readImage(paste("~/Documents/Data/faces94/", i, ".jpg", sep = ""))[,,1]))[-c(1,2)]
#' }
#' 
#' face_mat <- matrix(unlist(rawData), ncol = 101)
#' recognize("~/Documents/Data/faces94/42.jpg", face_mat)
recognize <- function(x, y, k = 1) {
  in_img <- readImage(as.character(x))
  in_img1 <- melt(t(in_img[,,1]))[-c(1,2)]
  
  # Obtain the average face vector
  face_mu <- rowMeans(y)
  face_mu <- matrix(rep(face_mu, ncol(y)), ncol = ncol(y))
  face_unque <- y - face_mu
  pc <- princomp(face_unque)
  
  imgH <- face_mu
  for (i in 1:k) {
    imgH <- imgH + cbind(pc$scores[,i]) %*% rbind(loadings(pc)[,i])
  }
  
  err <- as.data.frame(y) - as.matrix(in_img1)
  out <- numeric()
  for (i in 1:ncol(y)) {
    out[i] <- norm(as.matrix(err[, i]), "f")
  }
  
  min_err <- which(out == min(out))
  img_mat <- array(0, c(dim(in_img)[1:2], 2))
  for (i in 1:dim(in_img)[1]) {
    for (j in 1:dim(in_img)[2]) {
      n = (i - 1) * dim(in_img)[2] + j
      img_mat[i, j, ] <- c(in_img[n], y[n, min_err])
    }
  }
  
  img_mat[,,1] <- t(img_mat[,,1])
  display(img_mat[,,1:2], all = T, method = "r", title = c("Input", "Recognized as"))
}