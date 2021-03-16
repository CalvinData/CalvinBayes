#'
#' Summarize an rjags object
#'
#' Summarize an rjags object
#'
#' @param object and rjags object
#' @param digits preferred number of digits to display
#' @param intervals quantiles of posterior to diplay
#' @param ... additional arguments
#' @export

summary.rjags <- function (
  object, digits = 3,
  intervals = c(0.025, 0.25, 0.5, 0.75,  0.975), ...)
{
  res <- object$BUGSoutput
  res$digits = digits
  res$intervals = intervals

  sims.matrix <- res$sims.matrix
  mu.vect <- apply(sims.matrix, 2, mean)
  sd.vect <- apply(sims.matrix, 2, sd)
  int.matrix <- apply(sims.matrix, 2, quantile, intervals)
  if (res$n.chains > 1) {
    n.eff <- res$summary[, "n.eff"]
    Rhat <- res$summary[, "Rhat"]
  }
  else {
    n.eff <- Rhat <- NULL
  }
  summaryMatrix <- t(rbind(mu.vect, sd.vect, int.matrix, Rhat, n.eff))
  rownameMatrix <- rownames(summaryMatrix)
  dev.idx <- match("deviance", rownameMatrix)
  if (any(!is.na(dev.idx))) {
    summaryMatrix <- rbind(summaryMatrix[-dev.idx, ], summaryMatrix[dev.idx,
                                                                    ])
    rownames(summaryMatrix) <- c(rownameMatrix[-dev.idx],
                                 rownameMatrix[dev.idx])
  }
  res$summaryMatrix <- summaryMatrix
  structure(res, class = "summary.rjags")
}

#' Print summary.rjags objects
#'
#' Print summary.rjags objects
#'
#' @rdname print.rjags
#' @param x a summary.rjags objejct
#' @export
#'
print.summary.rjags <- function(x, digits = NULL, ...) {
  if (is.null(digits)) digits <- x$digits
  if (!is.null(x$program))
    cat("fit using ", x$program, sep = "")
  cat("\n ", x$n.chains, " chains, each with ", x$n.iter, " iterations (first ",
      x$n.burnin, " discarded)", sep = "")
  if (x$n.thin > 1)
    cat(", n.thin =", x$n.thin)
  cat("\n n.sims =", x$n.sims, "iterations saved\n")
  print(round(x$summaryMatrix, digits), ...)
  if (x$isDIC) {
    msgDICRule <- ifelse(x$DICbyR, "(using the rule, pD = var(deviance)/2)",
                         "(using the rule, pD = Dbar-Dhat)")
    cat(paste("\nDIC info ", msgDICRule, "\n", sep = ""))
    if (length(x$DIC) == 1) {
      cat("pD =", fround(x$pD, 1), "and DIC =", fround(x$DIC,
                                                       1))
    }
    else if (length(x$DIC) > 1) {
      print(round(x$DIC, 1))
    }
    # cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
  }
  invisible(x)
}

# borrowed from R2jags
fround <- function (x, digits)
{
  format(round(x, digits), nsmall = digits)
}

#' Effective Sample Size for MCMC objects
#'
#' Extract effective sample size information from MCMC objects.
#' `neff()` is a generic function. See below for objects that
#' `neff()` knows how to handle.
#'
#' @param object an object containing information about MCMC samples
#' @param regex_pars a vector of regular expressions matching the parameters for which
#'   effective sample size will be returned.
#' @param ... additional arguments passed to specific methods
#'
#' @export

neff <- function(object, ...) {
  UseMethod("neff")
}

#' @export
neff.rjags <- function(object, regex_pars = NULL, ...) {
  res <- object$BUGSoutput$summary[, "n.eff"] %>%
    setNames(rownames(object$BUGSoutput$summary))
  if (!is.null(regex_pars)) {
    res <- res[base::grepl(regex_pars, names(res))]
  }
  res
}


#' @importFrom bayesplot rhat
#' @export
rhat <- function(object, ...) {
  UseMethod("rhat")
  }

#' @export
rhat.default <- function(object, ...) {
  bayesplot::rhat(object, ...)
}

#' @export
rhat.rjags <- function(object, regex_pars = NULL, ...) {
  res <- object$BUGSoutput$summary[, "Rhat"] %>%
    setNames(rownames(object$BUGSoutput$summary))
  if (!is.null(regex_pars)) {
    res <- res[base::grepl(regex_pars, names(res))]
  }
  res
}

#' @export
summary_df <- function(object, ...) {
  UseMethod("summary_df")
}

#' @export
summary_df.rjags <-
  function(object, ...) {
    as_tibble(object$BUGSoutput$summary) %>%
      mutate(param = rownames(object$BUGSoutput$summary)) %>%
      select(param, names(.))

  }
