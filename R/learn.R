#' Supervised Learning
#' @export
#' 
#' @description
#' Supervised the algorithm to learn from the training data set.
#' 
#' @param x matrix input of class \code{images}; this object contains the imported images
#'          transformed into matrix by \code{\link{importImages}} function.
#' 
#' @examples
#' library(fr)
#' 
#' imgData <- importImages("~/Documents/Data/jaffe/", display = FALSE)
#' model <- learn(imgData)
#' 
#' # Display the Eigenfaces
#' showFace(criterion)
learn <- function(x) {
  if (class(x) != 'images') stop('x must be of class images.')

  # Obtain the mean of the faces of the images
  y <- x[[1L]] #
  face_mu <- rowMeans(y)
  face_mu <- matrix(rep(face_mu, ncol(y)), ncol = ncol(y))
  
  # Apply Principal Component Analysis
  pc <- princomp(y)
  out <- list(pc, x, face_mu)
  class(out) <- "learn"
  return(out)
}