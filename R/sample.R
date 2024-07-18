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
      dim(init_list$Omega_itemsL) <- c(2,2)
      dim(init_list$z_items) <- c(2,moddata$I)
      return(init_list)
    }
  )


  modsum <- posterior::summarise_draws(modrun$draws())

  returnNames <- ls(envir=environment())
  return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))

}
