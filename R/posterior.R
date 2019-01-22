
#' Extract posterior samples as a tidy data frame
#'
#' `posterior()` is a generic funtion.
#' See usage for a list of object classes for which methods exist.
#'
#' @param object An object produced by a bayesian fitting function like
#' [`R2jags::jags()`].
#'
#' @export
posterior <- function(object, ...) {
  UseMethod("posterior")
}

#' @rdname posterior
#' @export
#'
posterior.rjags <- function(object, ...) {
  as.data.frame(object$BUGSoutput$sims.list)
}

#' @rdname posterior
#' @importFrom brms posterior_samples
#' @export
#'
posterior.brmsfit <- function(object, ...) {
  brmsfit::posterior_samples(object, ...)
}

#' #' @export
#'
#' coda.samples.rjags <-
#'   function(model, variable.names = NULL, n.iter,
#'            thin = 1, na.rm = TRUE, ...) {
#'     as.mcmc(model, thin = thin, end = n.iter
#'   }

