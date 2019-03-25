
#' Convert stanfit Object to mcmc list Object
#'
#' Convert a stanfit object into an mcmc list object for use with coda and bayesplot tools.
#'
#' @param x a stanfit object
#' @param ... additional arguments (ignored)

#' @export
as.mcmc.list.stanfit <- function(x, ...) {
  mcmc.list(lapply(1:ncol(x), function(i) {mcmc(as.array(x)[ , i, ])}))
}
