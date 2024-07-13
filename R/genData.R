#' \loadmathjax{}
#' @title Data Generating Function for the \mjeqn{p \times i}{} Interaction Model
#'
#' @description Generates data for a \mjeqn{p \times i}{} Interaction Model under the Explanatory Item Response theoretical framework.
#'
#'@param P Number of examinees
#'@param I Number of items
#'@param J Number of potential item feature categories
#'@param K Number of potential person feature categories
#'@param seed Integer seed for replication (if `NULL`, a random seed will be generated)
#'
#' @details Let a model of the log-odds transformed outcome \mjeqn{(\eta_{pi})}{} be defined as:
#' \mjdeqn{\eta_{pi} = \theta_{p}\lambda_{i} + \tau_{i} + \sum_{k=1}^{K}\sum_{j=1}^{J}\beta_{jk_{(\eta)}}x_{ik}z_{pj}}{ representation}{}
#' where the linear predictor of latent trait measurements \mjeqn{\theta_{p}}{}is defined as:
#' \mjdeqn{\theta_{p} = \sum_{j=1}^{J}\beta_{j_{(\theta)}}z_{pj} + u_{p_{(\theta)}}}{}
#' where the linear predictor of item slope/discrimination terms (\mjeqn{\lambda_{i}}{}) is defined as:
#' \mjdeqn{\lambda_{i} = \sum_{k=1}^{K}\beta_{k_{(\lambda)}}x_{ik} + u_{i_{(\lambda)}}}{}
#' and where the linear predictor of item intercept/easiness terms (\mjeqn{\tau_{i}}{}) is defined as:
#' \mjdeqn{\tau_{i} = \sum_{k=1}^{K}\beta_{k_{(\tau)}}x_{ik} + u_{i_{(\tau)}}}{}
#' For the linear predictor of latent trait measurements, the following assumption is made on its residual terms for the purposes of model identification:
#' \mjdeqn{u_{p_{(\theta)}} \sim \mathcal{N}(0,1)}{}
#' If an assumption is made that there is no strcutured dependency between the residual terms for the linear predictors of item intercepts and item slopes (`isCorrI = FALSE`), then:
#' \mjdeqn{\begin{gathered} u_{i_{(\lambda)}} \sim \mathcal{N}(0, \sigma^{2}_{\lambda}) \cr u_{i_{(\tau)}} \sim \mathcal{N}(0, \sigma^{2}_{\tau})\end{gathered}}{}
#' Whereas if it is assumed that the residuals of the linear predictors of item intercepts and item slopes is assumed to have a predictable underlying structure (`isCorrI = TRUE`), then:
#' \mjdeqn{\begin{bmatrix}
#' u_{i_{(\lambda)}} \cr u_{i_{(\tau)}}
#' \end{bmatrix}
#' \sim \mathcal{MVN}
#' \Bigg(\mu =
#'        \begin{bmatrix} 0 \cr 0 \end{bmatrix}
#'      , \hspace{2pt} \mathbf{\Sigma} =
#'        \begin{bmatrix}\sigma^{2}_{\lambda} & \sigma_{\lambda}\sigma_{\tau} \cr \sigma_{\tau}\sigma_{\lambda} & \sigma^{2}_{\tau}
#'      \end{bmatrix}
#'      \Bigg)}{}
#'@export
#'@returns an environment containing simulated data
genData <- function(P,
                    I,
                    J,
                    K,
                    seed=NULL,
                    isCorrI=TRUE)
{

  args=as.list(match.call())[-1]

  if(is.null(seed)){
    seed <- abs(.Random.seed[3])
  }

  set.seed(seed)

  X <- base::sample(x=c(1:K), size=I, replace=TRUE)
  Z <- base::sample(x=c(1:J), size=P, replace=TRUE)

  beta_j_theta_est <- runif(n=J-1, min=-1, max=1)
  beta_j_theta <- c(0, beta_j_theta_est)
  beta_k_tau_est <- runif(n=K-1, min=-1, max=1)
  beta_k_tau <- c(0, beta_k_tau_est)
  beta_k_lambda_est <- runif(n=K-1, min=.75, max=2)
  beta_k_lambda <- c(1, beta_k_lambda_est)
  beta_jk_eta_est <- matrix(data=runif(n=(J-1)*(K-1), min=-1, max=1), nrow=J-1, ncol=K-1)
  beta_jk_eta <- matrix(data=0, nrow=J, ncol=K)

  for(j in 2:J){
    for(k in 2:K){
      beta_jk_eta[j,k] = beta_jk_eta_est[j-1,k-1]
    }
  }

  if(isCorrI){
    Omega_itemsL <- rCorr(nDim=2, eta=2)
    Omega_items <- tcrossprod(Omega_itemsL)
  }

  if(!isCorrI){
    Omega_itemsL <- diag(2)
  }

  sigma_tau <- runif(n=1, min=.75, max=2)
  sigma_lambda <- runif(n=1, min=.75, max=2)

  u_p_theta <- rnorm(n=P, mean=0, sd=1)
  z_items <- matrix(data=rnorm(n=I*2, mean=0, sd=1), nrow=2, ncol=I)
  u_items <- diag(c(sigma_lambda, sigma_tau)) %*% Omega_itemsL %*% z_items

  theta <- beta_j_theta[Z] + u_p_theta
  lambda <- beta_k_lambda[X] + u_items[1,]
  tau <- beta_k_tau[X] + u_items[2,]
  eta <- outer(theta, lambda) + outer(rep(1,P), tau) + beta_jk_eta[Z,X]

  resps <- matrix(data=NA, nrow=P, ncol=I)

  for(i in 1:I){
    for(p in 1:P){
      resps[p,i] <- rbinom(n=1, size=1, prob=plogis(eta[p,i]))
    }
  }

  returnNames <- ls(envir=environment())

  return(c(args, mget(returnNames, envir=environment())))

}
