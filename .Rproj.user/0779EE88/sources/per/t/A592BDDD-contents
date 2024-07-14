.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("cmdstanr is not installed. Attempting to install...")
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("stan-dev/cmdstanr")
  }

  tryCatch({
    cmdstan_path <- cmdstanr::cmdstan_path()
    if (is.null(cmdstan_path)) {
      message("CmdStan not found. Attempting to install...")
      cmdstanr::install_cmdstan()
    } else {
      message("CmdStan found at: ", cmdstan_path)
    }
  }, error = function(e) {
    message("Error checking CmdStan path: ", e$message)
    message("Attempting to install CmdStan...")
    cmdstanr::install_cmdstan()
  })
}
