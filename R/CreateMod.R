#'\loadmathjax{}
#'@title Create Adjunct CmdStan Model Environment
#'
#'@description Creates an environment to estimate the \mjeqn{p \times i}{} interaction model with CmdStan.
#'@returns an environment object containing fixed \mjeqn{\theta}{} and free \mjeqn{\theta}{} model results
#'@param aux_envir An environment object that contains objects to be loaded into the `CreateMod` environment (such as is returned by the \code{\link{genData}} function)
#'@param coef_hyper Hyperparameter value for the standard deviation of normally distributed parameters
#'@param sd_hyper Hyperparameter value for the shape parameter of gamma distributed parameters
#'@param nWarmup_init Number of burn-in draws for the fixed \mjeqn{\theta}{} model
#'@param nSampels_init Number of posterior draws for the fixed \mjeqn{\theta}{} model (after burn-in has completed)
#'@param nWarmup_run Number of burn-in draws for the free \mjeqn{\theta}{} model
#'@param nSamples_run Number of sampled posterior values for the free \mjeqn{\theta}{} model (after burn-in has completed)
#'
#'@details Creates an environment with methods for fitting the fixed \mjeqn{\theta}{} and free \mjeqn{\theta}{} models. Additionally, `CreateMod` includes a method for recursively checking \mjeqn{\hat{R}}{} convergence.
#'@seealso \code{\link{initialize}}, \code{\link{sample}}, \code{\link{rhatCheck}}
#'@export
#'
CreateMod <- function(
    coef_hyper,
    sd_hyper,
    nWarmup_init,
    nSamples_init,
    nWarmup_run,
    nSamples_run,
    aux_envir
)
{

  args <- as.list(match.call())[-1]

  list2env(as.list(aux_envir), envir=environment())

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
      sum_score=init_theta,
      true_beta_j_theta_est=beta_j_theta_est,
      true_beta_k_lambda_est=beta_k_lambda_est,
      true_beta_k_tau_est=beta_k_tau_est,
      true_beta_jk_eta_est=beta_jk_eta_est
    )

    if(isCorrI){
      initstan <- cmdstsan_model(stan_file="stan/init_pi_corr.stan")
    }

    if(!isCorrI){
      initstan <- cmdstan_model(stan_file="stan/init_pi.stan")
    }

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
      modstan <- cmdstan_model(stan_file="stan/run_pi_corr.stan")
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
        lambda=init_lambda,
        sigma_tau=init_sigma_tau,
        sigma_lambda=init_sigma_lambda
      )
    )

    modsum <- posterior::summarise_draws(modrun$draws())

    returnNames <- ls(envir=environment())
    return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

  }

  returnNames <- ls(envir=environment())
  return(list2env(c(args,mget(returnNames, envir=environment())), envir=parent.frame()))



}
