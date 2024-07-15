#' \loadmathjax{}
#' @title Run Free \mjeqn{\theta}{} Model
#' @description Runs the second step of the two-step estiamtion framework
#' @seealso \code{\link{CreateMod}}
#' @param ... arguments passed from parent frame
#' @details  The second step treating latent trait measurements (\mjeqn{\theta_{p}}{}) as freely estimated parameters in the two-step estimation framework for the \mjeqn{p \times i}{} model
#' @returns Model results for the full model added to the environment rendered by \code{\link{CreateMod}}
sample <- function(...){

  moddata <- list(
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
    modstan <- cmdstsan_model(stan_file="stan/run_pi_corr.stan")
  }

  if(!isCorrI){
    modstan <- cmdstan_model(stan_file="stan/run_pi.stan")
  }

  modrun <- modstan$sample(
    iter_warmup=nWarmup_run,
    iter_sampling=nSamples_run,
    seed=seed,
    data=moddata,
    chains=4,
    parallel_chains=4,
    init=function()list(
      theta=init_theta,
      tau=init_tau,
      lambda=init_lambda
    )
  )

  modsum <- posterior::summarise_draws(modrun$draws())

  returnNames <- ls(envir=environment())
  return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

}
