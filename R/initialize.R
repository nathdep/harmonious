#' \loadmathjax{}
#' @title Run Fixed \mjeqn{\theta}{} Model
#' @description Runs the first step in the two-step estimation framework
#' @seealso \code{\link{CreateMod}}
#' @param ... arguments passed from parent frame
#' @details  The first step in the two-step estimation framework for the \mjeqn{p \times i}{} model
#' @returns Model results for the standardized \mjeqn{\theta}{} model added to the environment rendered by \code{\link{CreateMod}}
initialize <- function(...){

  initdata <- list(
    P=nrow(resps),
    I=ncol(resps),
    J=max(Z),
    K=max(X),
    X=X,
    Z=Z,
    Y=resps,
    coef_hyper=coef_hyper,
    sd_hyper=sd_hyper,
    true_theta=theta,
    true_lambda=lambda,
    true_tau=tau,
    true_beta_j_theta_est=beta_j_theta_est,
    true_beta_k_lambda_est=beta_k_lambda_est,
    true_beta_k_tau_est=beta_k_tau_est,
    true_beta_jk_eta_est=beta_jk_eta_est
  )

  if(isCorrI){
    initmod <- cmdstan_model(stan_file="stan/run_pi_corr.stan")
  }

  if(!isCorrI){
    initmod <- cmdstan_model(stan_file="stan/run_pi.stan")
  }

  initrun <- initmod$variational(
    seed=seed,
    data=initdata
  )

  initsum <- posterior::summarise_draws(initrun$draws())
  init_lambda=initsum[grepl("^lambda\\[", initsum$variable),]$mean
  init_tau=initsum[grepl("^tau\\[", initsum$variable),]$mean

  returnNames <- ls(envir=environment())
  return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

}
