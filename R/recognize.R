#' Recognize Image (Human Face)
#' @export
#' 
#' @description
#' Function for recognizing the image using the model trained from the data. 
#' 
#' @param x string input; image file input to be recognize.
#' @param y model supervised on learning from the training dataset.
#' @param rule string input; stopping rule for choosing number of principal components.
#'        Options are: \code{'simple f-share'} for simple fair-share; \code{'broken stick'}; and,
#'        \code{'rel-broken stick'} for relative broken stick stopping rule.
#' @param display displays the input image, and the corresponding output image --
#'        as recognized by the model.
#' 
#' @examples
#' library(fr)
#' 
#' # Import all images in the directory "data/jaffe/"
#' imgData <- importImges("~/Documents/Data/jaffe/", display = FALSE)
#' 
#' model <- learn(imgData)
#' a <- recognize("~/Documents/Data/jaffe/KA.FE3.47.tiff", criterion, display = TRUE, rule = "rel-broken stick")
recognize <- function(x, y, rule = "simple f-share", display = TRUE) {
  if (class(y) != 'learn') stop ("y is of class 'learn'.")
  if (class(x) != 'character') stop ("x should be string.")
  
  # Read the input image
  in_img <- readImage(as.character(x))
  ydat <- y[[2L]] # Extract the input data of all images
  # Conditions for the dimensions of the image
  if (length(dim(in_img)) == 3L) {
    if (dim(in_img)[1L] != ydat[[2L]][1L] ||
          dim(in_img)[2L] != ydat[[2L]][2L] ||
          dim(in_img)[3L] != ydat[[2L]][3L]) {
      stop("The dimensions of x and y are not compatible. x and y must have the same dimensions.")
    }
  } else if (length(dim(in_img)) == 2L) {
    if (dim(in_img)[1L] != ydat[[2L]][1L] ||
          dim(in_img)[2L] != ydat[[2L]][2L]) {
      stop("The dimensions of x and y are not compatible. x and y must have the same dimensions.")
    }
  }
  
  # Extract the column pixel values of the input image
  if (length(dim(in_img)) == 3L) {
    in_img1 <- melt(t(in_img[,, 1L]))[-c(1L, 2L)] 
  } else if (length(dim(in_img)) == 2L) {
    in_img1 <- melt(t(in_img))[-c(1L, 2L)]
  }
  
  # Generate the image using the first k images
  imgH <- face_mu <- y[[3L]]; pc <- y[[1L]]
  if (is.null(rule)) {
    imgH <- imgH + pc$scores %*% t(loadings(pc))
  } else if (!is.null(rule)) {
    # Extract the maximum number of PCs for the image
    if (rule == "rel-broken stick") {
      k <- max(rbrStick(pc$sdev ^ 2L)[["Use PC(s):"]]) 
    } else if (rule == "simple f-share") {
      k <- max(which((pc$sdev ^ 2L) > (sum(pc$sdev ^ 2L)) / length(pc$sdev)))
    } else if (rule == "broken stick") {
      k <- max(brStick(pc$sdev ^ 2L)[["Use PC(s):"]])
    }
    if (k == dim(ydat[[1L]])[2L]) {
      imgH <- imgH + pc$scores %*% loadings(pc)
    } else {
      for (i in 1L:k) {
        imgH <- imgH + cbind(pc$scores[,i]) %*% rbind(loadings(pc)[,i])
      } 
    }
  }
  
  # Extract the error by subtracting the fitted image to all image data
  err <- as.matrix(as.data.frame(imgH) - as.matrix(in_img1))
  
  # And for each error obtain the norm/length of the vector
  out <- getNorm(err)
  
  # The one with the smallest norm will tell us the closest image
  min_err <- which(out == min(out))
  img_mat <- array(0L, c(dim(in_img)[1L:2L], 2L))
  img_mat[,,] <- c(matrix(in_img1[,1L], ncol = dim(in_img)[2L], byrow = TRUE),
                   matrix(ydat[[1L]][, min_err], ncol = dim(in_img)[2L], byrow = TRUE))
  if (display == TRUE) {
    display(img_mat[,,1L:2L], all = T, method = "r", title = c("Input", "Recognized as"))
  } 
  
  out <- list("Average Face" = face_mu[,1L], "Image Dimension" = dim(in_img), "Image" = img_mat[,,1L:2L])
  class(out) <- "recognize"
  return(out)
}