#' \loadmathjax{}
#' @title Load/Install the cmdstanr package
#'
#' @description
#'Validates installation of the cmdstanr package and, if not found, prompts for its installation from the recommended source
#'
#' @export
#'
#'
#' @details See https://mc-stan.org/cmdstanr/
#'
.onLoad <- function(libname, pkgname) {
  install_cmdstanr <- function() {
    message("cmdstanr package not found. Installation of the package and CmdStan will begin in:")
    
    for (i in 5:0) {
      cat(i, "\r")
      flush.console()
      Sys.sleep(1)
    }
    
    cat("\n")
    
    install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages", getOption("repos")))
  }

  install_cmdstan <- function() {
    message("CmdStan not found or not properly configured. Installation will begin in:")
    
    for (i in 5:0) {
      cat(i, "\r")
      flush.console()
      Sys.sleep(1)
    }
    
    cat("\n")
    
    tryCatch({
      cmdstanr::install_cmdstan()
      message("CmdStan has been successfully installed.")
    }, error = function(e) {
      warning("Failed to install CmdStan: ", e$message, "\nSome features may not work.")
    })
  }
  
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    install_cmdstanr()
  }
  
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr package installation failed.")
  }
  
  cmdstan_installed <- tryCatch({
    cmdstan_path <- cmdstanr::cmdstan_path()
    dir.exists(cmdstan_path)
  }, error = function(e) {
    FALSE
  })
  
  if (!cmdstan_installed) {
    install_cmdstan()
  }
  
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("CmdStan installation failed.")
  }
  
  # Load the cmdstanr package
  library("cmdstanr", character.only = TRUE)
  
  message("CmdStan is properly installed.")
}
