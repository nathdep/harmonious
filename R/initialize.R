#' \loadmathjax{}
#' @title Run Fixed \mjeqn{\theta}{} Model
#' @description Runs the first step in the two-step estimation framework
#' @seealso \code{\link{CreateMod}}
#' @param ... arguments passed from parent frame
#' @details  The first step in the two-step estimation framework for the \mjeqn{p \times i}{} model
#' @returns Model results for the standardized \mjeqn{\theta}{} model added to the environment rendered by \code{\link{CreateMod}}
initialize <- function(...){
  init_theta <- (rowSums(resps) - mean(rowSums(resps)))/sd(rowSums(resps))
  init_lambda <- vector(length=I, mode="numeric")
  init_tau <- vector(length=I, mode="numeric")

  for(i in 1:I){
    linmod <- glm(resps[,i] ~ init_theta, family=binomial(link="logit"))
    init_tau[i] <- linmod$coefficients[1]
    init_lambda[i] <- linmod$coefficients[2]
  }

  init_sigma_lambda <- sd(init_lambda)
  init_sigma_tau <- sd(init_tau)

  initstan <- cmdstan_model(initFile)

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
    sum_score=init_theta
  )

  initrun <- initstan$sample(
    iter_warmup=nWarmup_init,
    iter_sampling=nSamples_init,
    seed=seed,
    data=initdata,
    chains=4,
    parallel_chains=4
  )

  initsum <- posterior::summarise_draws(initrun$draws())
  init_lambda=initsum[grepl("^lambda\\[", initsum$variable),]$mean
  init_tau=initsum[grepl("^tau\\[", initsum$variable),]$mean

  returnNames <- ls(envir=environment())
  return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

}
