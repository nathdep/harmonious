#' \loadmathjax{}
#' @title Recursive Validation for Rhat Convergence
#'
#'@description Helper function that validates \mjeqn{\hat{R}}{} convergence.
#'@param ... arguments passed from parent frame
#'
#'@details
#' \deqn{
#' \begin{array}{l}
#' \textbf{Input: } \texttt{CmdStan}\hspace{3pt}\text{model,}\hspace{3pt}N_{\mathrm{max}}\text{,}\hspace{3pt}\hat{R}_{1} \\
#' \text{Sample }\texttt{CmdStan}\hspace{3pt}\text{model} \\
#' \quad \textbf{while}\hspace{5pt} \hat{R}_{n}\leq\hat{R}_{thresh}\hspace{5pt} \textbf{or} \hspace{5pt} n \leq N_{\mathrm{max}}\hspace{5pt}\textbf{do} \\
#' \qquad \text{Sample free } \theta \text{ model} \\
#' \qquad n \mathrel{+}= 1 \\
#' \quad \textbf{end do} \\
#' \textbf{end while} \\
#' \end{array}
#' }
#'
#'@export
#'@returns modified free \mjeqn{\theta}{} model that is added to an environment created by \code{\link{CreateMod}}
#'@seealso \code{\link{CreateMod}}
rhatCheck <- function(...){
  cycle=1
  cat("\nMAX RHAT: ", round(max(modsum$rhat, na.rm=TRUE),digits=3), "\n")
  rhat_condition=all(modsum$rhat < rhat_threshold, na.rm=TRUE)
  if(!rhat_condition){
    while(cycle <= maxCycles && rhat_condition == FALSE){
      model$sample(seed=seed+1)
      modsum <- posterior::summarise_draws(modrun$draws())
      cycle=cycle+1
      rhat_condition=all(modsum$rhat <= rhat_threshold, na.rm=TRUE)
      cat("\nMAX RHAT: ", round(max(modsum$rhat, na.rm=TRUE),digits=3), "\n")
    }
  }
  returnNames <- ls(envir=environment())
  return(list2env(mget(returnNames, envir=environment()), envir=parent.frame()))
}
