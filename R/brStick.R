#' Broken Stick Stopping Rule
#' @export
#' 
#' @description
#' This function is a criterion for choosing the number of principal components.
#' 
#' @param x numeric, a vector of the eigenvalues of the principal components.
#' @param plot boolean default is FALSE, toggle plot.
#' 
#' @seealso \code{\link{rbrStick}}
#' 
#' @examples 
#' summary(pc.cr <- princomp(USArrests, cor = TRUE))
#' brStick(pc.cr$sdev ** 2)
#' 
#' # Show plot
#' brStick(pc.cr$sdev ** 2, plot = TRUE)
brStick <- function (x, plot = FALSE) {
  m <- 0
  out <- brLoop(x)
  colnames(out) <- c("% of Variability", "B-Stick Threshold")
  
  if (plot == TRUE) {
    br.df <- melt(out)
    p <- xyplot(
      value ~ Var1, group = Var2, data = br.df, type = c('g', 'l'),
      xlab = "Principal Components", ylab = "Percent of Variability",
      auto.key = list(corner = c(0.1, 0.9), points = FALSE , lines = TRUE))
    show(p)
  }
  
  return(list("Use PC(s):" = which(out[, 1] > out[, 2]), Table = out))
}