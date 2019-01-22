
#' Extract posterior samples as a tidy data frame
#'
#' Extract posterior samples as a tidy data frame
#'
#' @param object An object produced by a bayesian fitting function like
#' [`R2jags::jags()`].
#'
#' @export
posterior <- function(object, ...) {
  UseMethod("posterior")
}

#' @export
#'
posterior.rjags <- function(object, ...) {
  as.data.frame(object$BUGSoutput$sims.list)
}

#' #' @export
#'
#' coda.samples.rjags <-
#'   function(model, variable.names = NULL, n.iter,
#'            thin = 1, na.rm = TRUE, ...) {
#'     as.mcmc(model, thin = thin, end = n.iter
#'   }

