#'\loadmathjax{}
#'@title Create Adjunct CmdStan Model Environment
#'
#'@description Creates an environment to estimate the \mjeqn{p \times i}{} interaction model with CmdStan.
#'@returns an environment object containing fixed \mjeqn{\theta}{} and free \mjeqn{\theta}{} model results
#'@param aux_envir An environment object that contains objects to be loaded into the `CreateMod` environment (such as is returned by the \code{\link{genData}} function)
#'@param coef_hyper Hyperparameter value for the standard deviation of normally distributed parameters
#'@param sd_hyper Hyperparameter value for the shape parameter of gamma distributed parameters
#'@param nWarmup_init Number of burn-in draws for the fixed \mjeqn{\theta}{} model
#'@param nSamples_init Number of posterior draws for the fixed \mjeqn{\theta}{} model (after burn-in has completed)
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
      init=function(){
        param_names <- initsum[!grepl("^rmsd", initsum$variable) & !grepl("^lp", initsum$variable),]$variable
        param_names_ind <- which(!grepl("^rmsd", initsum$variable) & !grepl("^lp", initsum$variable))
        no_brackets <- gsub("\\[.*\\]", "", param_names)
        init_list <- vector(length=length(unique(no_brackets)), mode="list")
        names(init_list) <- unique(no_brackets)
        for(i in 1:length(param_names)){
          init_list[[no_brackets[i]]] <- c(init_list[[no_brackets[i]]], initsum$mean[param_names_ind[i]])
        }
        if(isCorrI){
          dim(init_list$Omega_itemsL) <- c(2,2)
        }
        dim(init_list$z_items) <- c(2,moddata$I)
        return(init_list)
      }
    )


    modsum <- posterior::summarise_draws(modrun$draws())

    returnNames <- ls(envir=environment())
    return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

  }

  returnNames <- ls(envir=environment())
  return(list2env(c(args,mget(returnNames, envir=environment())), envir=parent.frame()))



}
