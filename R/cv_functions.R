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
#'
#' @param x A covariate matrix.
#' @param params A k-by-2 matrix containing the means of the in-sample covariate
#'   matrix in the first column and the sds of the in-sample covariate matrix
#'   in the second column.
#' @return The scaled covariate vector.
scale_oos_covars <- function(X, params){
  Xout <- X
  Xout[,] <- NA
  for(k in 1:ncol(X)){
    Xout[,k] <- (X[,k] - params[k,1]) / params[k,2]
  }
  return(Xout)
}

#' Calculate likelihood-based metrics for WAIC and LPD
#'
#' @param stanfit A formal class stanfit
#' @return A list of WAIC and LPD metrics
waic <- function(stanfit){
  log_lik <- rstan::extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- matrixStats::colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}
