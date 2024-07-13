#' \loadmathjax{}
#' @title Generate Text Report of Free \mjeqn{\theta}{} Model Results
#'
#'@description Helper function that generates a text document in the user-specified save directory
#'@details Model results include posterior descriptives and simulated data information
#'@param saveDir Path to directory where report is saved
#'@param fileDetails string identifier for name of saved report
#'@param ... additional arguments passed from parent frame
#'@returns a `.txt` file of the free \mjeqn{\theta}{} results
#'@export
genReport <- function(saveDir, fileDetails, ...){

  panderOptions("table.split.table", Inf)

  list2env(mget(ls(envir=.GlobalEnv), envir=.GlobalEnv), envir=environment())

  sink(paste0(saveDir,"/", fileDetails, ".txt"))

  cat("\n\n====================\n\n")
  cat("SEED ", seed)
  cat("\n\n====================\n\n")
  cat("P: ", P, "\n")
  cat("I: ", I, "\n")
  cat("J: ", J, "\n")
  cat("K: ", K, "\n")

  negsum <- sum(ifelse(lambda < 0, 1, 0))

  cat(paste0("PROPORTION \u03A3[\u03bb_i < 0]/I = ", round(100*negsum/I), "%"),"\n")
  cat(paste0("MAX RHAT: ", round(max(modsum$rhat, na.rm=TRUE), digits=3), "/", modsum$variable[which.max(modsum$rhat)]))
  cat("\n")

  if(any(grepl("^Omega", ls(envir=.GlobalEnv)))){
    Omega_items_df <- as.data.frame(round(Omega_items, digits=3))
    colnames(Omega_items_df) = rownames(Omega_items_df) = c("\u03bb", "\u03c4")
    Sigma_items_df <- as.data.frame(round(diag(c(sigma_lambda, sigma_tau)) %*% Omega_items %*% t(diag(c(sigma_lambda, sigma_tau))), digits=3))
    colnames(Sigma_items_df) = rownames(Sigma_items_df) = c("\u03bb", "\u03c4")
    cat("\n\n\nTRUE \u03A9 ITEM RESIDUALS: \n\n")
    print(Omega_items_df)
    cat("\n\nTRUE \u03A3 ITEM REISUDALS: \n\n")
    print(Sigma_items_df)
    cat("\n\n")
    cat("\n\n\nEAP \u03A9 ITEM RESIDUALS: \n\n")
    eap_omega <- modsum[grepl("^Omega_items\\[", modsum$variable),]$mean
    dim(eap_omega) <- c(2,2)
    eap_omega <- as.data.frame(round(eap_omega, digits=3))
    colnames(eap_omega) = rownames(eap_omega) = c("\u03bb", "\u03c4")
    print(eap_omega)
    cat("\n\nTRUE \u03A3 ITEM REISUDALS: \n\n")
    eap_sigma <- modsum[grepl("^Sigma_items\\[", modsum$variable),]$mean
    dim(eap_sigma) <- c(2,2)
    eap_sigma <- as.data.frame(round(eap_sigma, digits=3))
    colnames(eap_sigma) = rownames(eap_sigma) = c("\u03bb", "\u03c4")
    print(eap_sigma)
    cat("\n\n\n")
  }

  if(any(grepl("^mean_bias", modsum$variable))){
    cat("MEAN POSTERIOR BIAS (EST - TRUE)\n\n")
    pandoc.table(modsum[grepl("^mean_bias", modsum$variable),])
    cat("\n\n\n")
  }

  if(any(grepl("^sd_bias", modsum$variable))){
    cat("S.D. POSTERIOR BIAS (EST-TRUE)\n\n")
    pandoc.table(modsum[grepl("^sd_bias", modsum$variable),])
    cat("\n\n\n")
  }

  if(any(grepl("^rmsd", modsum$variable))){
    cat("POSTERIOR RMSD \n\n")
    pandoc.table(modsum[grepl("^rmsd", modsum$variable),])
    cat("\n\n\n")
  }

  if(any(grepl("^mean_bias", modsum$variable))){
    cat("MEAN POSTERIOR BIAS (EST. - TRUE)\n\n")
    pandoc.table(modsum[grepl("^mean_bias", modsum$variable),])
    cat("\n\n\n")
  }

  if(any(grepl("^sd_bias", modsum$variable))){
    cat("S.D. POSTERIOR BIAS (EST. - TRUE)\n\n")
    pandoc.table(modsum[grepl("^sd_bias", modsum$variable),])
    cat("\n\n\n")
  }

  sink()

}
