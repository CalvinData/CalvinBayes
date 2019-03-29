#' Make posterior cacluations
#'
#' A generic and some methods for creating posterior calculations.
#' The intended use is to prepare data for use with bayesplot ppc plots.
#' The default method works for any object for which `posterior()` can
#' create a data frame of posterior samples.
#'
#' @param object An object from which posterior calculations are made.
#' @param formula A formula describing the quantity to be calculated.  The rhs of the
#'   formula is evaluated using one posterior sample of the parameters and `data`.
#'   The lhs, if it exists, is used to name the resulting column when `data.frame = TRUE`.
#' @param data Additional data involved in the computation. This may be the original
#'    data used to fit the model or counter-factual data.
#' @param draws The number of draws to make from the posterior distribution.
#'   Sampling is with replacement if `draws` is larger than the number of
#'   posterior samples in `object`.
#' @param data.frame A logical indicating whtehr the results should be returned
#'   as a data frame (TRUE) or a matrix (FALSE).
#' @param ... Additional arguments, currently ignored.
#' @return A matrix with `draws` rows or data frame with three columns.
#'
#'

#' @export
#'
#' @rdname posterior_calc
posterior_calc <- function(object, ...) {
  UseMethod("posterior_calc")
}

#' @rdname posterior_calc
#' @export
posterior_calc.default <-
  function(object, formula, data = NULL, draws = NULL, data.frame = FALSE, ...) {
    posterior_calc(
      posterior(object),
      formula = formula, data = data, draws = draws, data.frame = data.frame, ...)
  }

#' @rdname posterior_calc
#' @export
posterior_calc.data.frame <-
  function(object, formula, data = NULL, draws = NULL, data.frame = FALSE, ...) {
    Post <- object
    if (is.null(draws) || draws == nrow(Post)) {
      # use all the rows (in order)
      draws <- nrow(Post)
      sampled_rows <- 1:draws
    } else {
      # sample the number of times requested, with replacement if required
      if (draws < 1) { stop("draws must be at least 1") }
      draws <- round(draws)
      sampled_rows <-
        sample(1:nrow(Post), size = draws, replace = draws > nrow(Post))
    }
    # now evalute rhs of formula for each sampled row
    res <-
      do.call(
        rbind,
        lapply(
          sampled_rows,
          function(r) {eval(formula[[length(formula)]],
                            c(as.list(Post[r, , drop = FALSE]),  as.list(data)),
                            parent.frame())}
        )
      )
    if (data.frame) {
      dm <- dim(res)
      res <-
        data.frame(
          yrep  = as.vector(res),
          draw  = rep(1:dm[1], times = dm[2]),
          y_ind = rep(1:dm[2], each = dm[2])
        )
      if (length(formula) == 3) {
        names(res)[1] <- deparse(formula[[2]])
      }
    }
    res
  }
