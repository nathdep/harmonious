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
    true_tau=tau
  )

  modstan <- stan_package_model(name = modname, package = "harmonious")

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
