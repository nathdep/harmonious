#' \loadmathjax{}
#' @title Generation of Data for the Person-by-Item Interaction Model
#' @description randomly samples a lower triangular, Cholesky-factorized matrix \mjeqn{L}{} from a Lewandowski-Kurowicka-Joe (LKJ) distribution given \mjeqn{\eta}{}.
#' @details Using the onion method, samples a Cholesky factorized lower-triangular matrix \mjeqn{L}{} from a Lewandowski-Kurowicka-Joe (LKJ) distribution given the concentration hyperparameter \mjeqn{\eta}{}, such that: \mjdeqn{A=LL^{\top}}{} where \mjeqn{A}{} is a \mjeqn{n \times n}{} matrix and is a member of the set of all symmetric, positive-definite matrices.
#' @param nDim desired dimension of the sampled \mjeqn{n \times n}{} matrix
#' @param eta concentration hyperparameter
#' @returns a Choleksy-factorized, lower triangular matrix
#' @export
rCorr <- function(nDim, eta=1){
  OmegaL <- matrix(data=0, nrow=nDim, ncol=nDim)
  for(n in 1:nDim){
    if(n==1){
      OmegaL[n,n] = 1
    }
    else{
      beta_hyper <- eta + (n-1)/2
      beta_cell <- rbeta(n=1, shape=beta_hyper,shape2=beta_hyper)
      scale <- sqrt(beta_cell)

      z <- rnorm(n=n-1, mean=0, sd=1)
      z <- z/sqrt(sum(z^2))
      OmegaL[n,1:(n-1)] <- scale*z
      OmegaL[n,n] <- sqrt(1-scale^2)
    }
  }
  return(OmegaL)
}
