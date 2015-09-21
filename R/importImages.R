#' Import Images
#' @export
#' 
#' @description
#' Import images from the folder.
#' 
#' @param path string input; directory of the folder, default is set to the working directory.
#' @param display boolean input; toggle display of the images.
#' 
#' @details 
#' The \code{path} of the folder should end with \code{/}, so that function will import all
#' filenames following that symbol.
#' 
#' @seealso 
#' \code{\link{EBImage::readImage}}
#' @examples
#' library(fr)
#' 
#' # Import all images in the directory "data/jaffe/"
#' imgData <- importImages("~/Documents/Data/jaffe/")
#' showFace(imgData) # display the images
importImages <- function (path = ".", display = FALSE) {
  
  if (!is.character(path))
    stop("path should be character vector.")
  
  x <- list.files(path)
  rawData <- list(); numData <- list()
  for (i in 1L:length(x)) {
    rawData[[i]] <- readImage(paste(path, x[i], sep = ""))  
    if (length(dim(rawData[[i]])) == 2L) {
      numData[[i]] <- as.matrix(melt(t(as.matrix(rawData[[i]])))[-c(1L, 2L)])
    } else if (length(dim(rawData[[i]])) == 3L) {
      numData[[i]] <- as.matrix(melt(t(as.matrix(rawData[[i]][,, 1L])))[-c(1L, 2L)])
    }
  }
  nrow <- nrow(numData[[1]])
  ncol <- length(numData)
  # Matrix of faces, each column correspond to face vector
  rawMat <- mat_of_faces(numData, nrow, ncol)
  
  d <- dim(rawData[[1L]])
  if (display == TRUE) {
    rawImges <- mat_to_image(rawMat, d)
    display(rawImges, all = T, method = "r")
  }
  out <- list(rawMat, d)
  class(out) <- "images"
  return(out)
}