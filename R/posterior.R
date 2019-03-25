
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

array2posterior <- function(a) {
  res <- a
  dn <- dimnames(res)
  dm <- dim(res)
  n_iter   <- dm[[1]]
  n_chains  <- dm[[2]]
  n_params <- dm[[3]]

  dim(res) <- c(n_iter * n_chains, n_params)
  res <- as.data.frame(res)
  names(res) <- dn[[3]]
  if (is.null(dn[[2]])) {
    dn[[2]] <- paste0("chain:", 1:dm[[2]])
  }
  if ("chain" %in% names(res)) {
    res$.chain <- rep(dn[[2]], each = n_iter)
  } else {
    res$chain  <- rep(dn[[2]], each = n_iter)
  }
  if ("iter" %in% names(res)) {
    res$.iter <- rep(1:n_iter, times = n_chains)
  } else {
    res$iter  <- rep(1:n_iter, times = n_chains)
  }
  names(res) <- gsub("\\[(\\d*)\\]", ".\\1", names(res))
  res
}

#' @rdname posterior
#' @export
#'
posterior.rjags <- function(object, ...) {
  array2posterior(object$BUGSoutput$sims.array)
}

#' @rdname posterior
#' @export
#'
posterior.stanfit <- function(object, ...) {
  res <- rstan::extract(object, permute = FALSE, ...)
  array2posterior(res)
}

#' @rdname posterior
#' @importFrom brms posterior_samples
#' @export
#'
posterior.brmsfit <- function(object, ...) {
  brmsfit::posterior_samples(object, ...)
}

#' @export
posterior.data.frame <- function(object, n = 5000, posterior = "posterior",
                                 remove = c("likelihood", "loglik", "prior",
                                            "loglikelihood")) {

  prob <- object[[posterior]]
  idx <- base::sample(1:nrow(object), size = n, replace = TRUE, prob = prob)
  object[idx, setdiff(names(object), union(posterior, remove)), drop = FALSE]
}

#' #' @export
#'
#' coda.samples.rjags <-
#'   function(model, variable.names = NULL, n.iter,
#'            thin = 1, na.rm = TRUE, ...) {
#'     as.mcmc(model, thin = thin, end = n.iter
#'   }

