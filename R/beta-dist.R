#' Compute parameters of a beta distribution
#'
#' Given two parameters which characterize a beta distribution,
#' compute the remaining parameters in the following set
#' `shape1` (`a`), `shape2` (`2`), `mean`, `mode`, `sd`, and `concentration` (`k`).
#'
#' @param shape1,a first shape parameter
#' @param shape2,b second shape paramter
#' @param mean mean of beta distribution
#' @param mode mode of beta distribution
#' @param concentration,k concentration of beta distribution
#' @param sd standard deviation of beta distribution
#' @param plot logical indicating whether a plot of the distribution
#'   should be displayed.
#' @param ... additional arguments passed to [ggformula::gf_dist()] to
#'   modify the plot produced if `plot == TRUE`.
#'
#' @importFrom ggformula gf_segment gf_dist gf_labs
#' @importFrom ggplot2 geom_segment labs
#' @export
#' @examples
#' beta_params(10, 5)
#' beta_params(mean = 0.8, concentration = 10)
#' beta_params(mean = 0.8, k = 10)
#' beta_params(mode = 0.8, concentration = 10)
#' beta_params(mode = 0.8, k = 10)
#' beta_params(mode = 0.8, k = 10, plot = TRUE, color = "red")

beta_params <-
  function(shape1 = NULL,
           shape2 = NULL,
           mean = NULL, sd = NULL,
           concentration = NULL, mode = NULL,
           a = shape1, b = shape2, k = concentration,
           plot = FALSE,
           ...) {

    Res <- NULL
    if (!is.null(a) & !is.null(b)) {
      if (a <= 0 || b <= 0) {
        stop("Both shape parameters must be positive.")
      }
      Res <-
        tibble(
          shape1 = a,
          shape2 = b,
          mean = a / (a + b),
          mode = (a - 1) / (a + b - 2),
          sd   = sqrt( a * b / ((a + b)^2 * (a + b + 1))),
          concentration = a + b
        )
    }

    if (is.null(Res) && !is.null(mode) && ! is.null(k)) {
      if ( mode <= 0 | mode >= 1) stop("must have 0 < mode < 1")
      Res <-
        tibble(
          shape1 =       mode * (k - 2) + 1,
          shape2 = (1 - mode) * (k - 2) + 1,
          mean = shape1 / (shape1 + shape2),
          mode = (shape1 - 1) / (shape1 + shape2 - 2),
          sd   = sqrt( shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))),
          concentration = k
        )
    }

    if (is.null(Res) && !is.null(mean) && ! is.null(k)) {
      if ( mean <= 0 || mean >= 1) stop("must have 0 < mean < 1")
      Res <-  tibble(
          shape1 =      mean  * k,
          shape2 = (1 - mean) * k,
          mean = shape1 / (shape1 + shape2),
          mode = (shape1 - 1) / (shape1 + shape2 - 2),
          sd   = sqrt( shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))),
          concentration = k
        )
    }

    if (is.null(Res) && !is.null(mean) && ! is.null(sd)) {
      if ( mean <= 0 || mean >= 1) stop("must have 0 < mean < 1")
      k <- mean * (1 - mean) / sd^2 - 1
      Res <-
        tibble(
          shape1 =      mean  * k,
          shape2 = (1 - mean) * k,
          mean = shape1 / (shape1 + shape2),
          mode = (shape1 - 1) / (shape1 + shape2 - 2),
          sd   = sqrt( shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))),
          concentration = k
        )
    }

    # if (is.null(Res) && !is.null(mode) && ! is.null(sd)) {
    #   if ( mode <= 0 || mode >= 1) stop("must have 0 < mode < 1")
    #   fn <- function(x) {
    #     a <- x[1]; b <- x[2]
    #     crossprod(
    #       c(mode = (a - 1) / (a + b - 2),
    #         sd   = sqrt(a * b / ((a + b)^2 * (a + b + 1)))) -
    #         c(mode, sd)
    #     )
    #   }
    #   soln <- optim(c(2, 2), fn)$par
    #   Res <-
    #     tibble(
    #       shape1 = soln[1],
    #       shape2 = soln[2],
    #       mean = shape1 / (shape1 + shape2),
    #       mode = mode,
    #       sd   = sd,
    #       concentration = a + b
    #     )
    # }

    if (!is.null(Res)) {
      if (plot) {
        f_mean <-
           0 + dbeta(Res$mean, shape1 = Res$shape1, shape2 = Res$shape2) ~
           Res$mean + Res$mean
        f_mode <-
           0 + dbeta(Res$mode, shape1 = Res$shape1, shape2 = Res$shape2) ~
           Res$mode + Res$mode
        p <- ggformula::gf_dist("beta", shape1 = Res$shape1, shape2 = Res$shape2, ...)
        if (! "kind" %in% names(list(...))) {
            p <-
              p +
              ggplot2::geom_segment(aes(x = Res$mean, xend = Res$mean,
                yend = dbeta(Res$mean, shape1 = Res$shape1, shape2 = Res$shape2),
                y = 0)) +
              ggplot2::geom_segment(aes(x = Res$mode, xend = Res$mode,
                yend = dbeta(Res$mode, shape1 = Res$shape1, shape2 = Res$shape2),
                y = 0))
            }
        p <- p +
          ggplot2::labs(
            title = paste("Beta(",
                          format(Res$shape1), ", ", format(Res$shape2),
                          ")",
                          sep = "")
          )
        print(p)
      }
      return(Res)
    }

    # fall through if not in one of the cases above.

    stop(paste(
      "Specify one of the following pairs: ",
      "shape1 (a) & shape2 (b); mode & concentration (k); mean & concentration (k); mean & sd",
      sep = "\n\t"))
  }
