#' Scale a covariate matrix
#'
#' @param X The covariate matrix
#' @return A list with 2 elements: the scaled matrix and a dataframe with the
#'   mean and standard deviation used to scale the matrix.
get_scaled_covars <- function(X){
  if(is.matrix(X) == FALSE) stop("X must be a matrix.")
  mus <- apply(X, MARGIN = 2, FUN = mean)
  sigmas <- apply(X, MARGIN = 2, FUN = sd)

  Xout <- X
  Xout[,] <- NA
  for(k in 1:ncol(X)){
    Xout[,k] <- (X[,k] - mus[k]) / sigmas[k]
  }
  param_df <- data.frame(
    means = mus,
    sigmas = sigmas
  )
  return(list(Xout, param_df))
}

#' Scale an out-of-sample covariate vector
#' @param x A covariate vector (length k).
#' @param params A k-by-2 matrix containing the means of the in-sample covariate
#'   matrix in the first column and the sds of the in-sample covariate matrix
#'   in the second column.
#' @return The scaled covariate vector.
scale_oos_covars <- function(x, params){
  xout <- x
  xout <- NA
  for(k in 1:length(x)){
    xout[k] <- (x[k] - params[k,1]) / params[k,2]
  }
  return(xout)
}
