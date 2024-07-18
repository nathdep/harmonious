#' \loadmathjax{}
#' @title Run Free \mjeqn{\theta}{} Model
#' @description Runs the second step of the two-step estiamtion framework
#' @seealso \code{\link{CreateMod}}
#' @param ... arguments passed from parent frame
#' @details  The second step treating latent trait measurements (\mjeqn{\theta_{p}}{}) as freely estimated parameters in the two-step estimation framework for the \mjeqn{p \times i}{} model
#' @returns Model results for the full model added to the environment rendered by \code{\link{CreateMod}}
