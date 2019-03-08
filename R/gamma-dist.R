#' Compute parameters of a gamma distribution
#'
#' Given two parameters which characterize a gamma distribution,
#' compute the remaining parameters in the following set
#' `shape`, `rate`, `scale`, `mean`, `mode`, `sd`.
#'
#' @param shape, shape parameter
#' @param rate, rateparamter
#' @param mean mean of gamma distribution
#' @param mode mode of gamma distribution
#' @param concentration,k concentration of gamma distribution
#' @param sd standard deviation of gamma distribution
#' @param plot logical indicating whether a plot of the distribution
#'   should be displayed.
#' @param ... additional arguments passed to [ggformula::gf_dist()] to
#'   modify the plot produced if `plot == TRUE`.
#'
#' @importFrom ggformula gf_segment gf_dist gf_labs
#' @importFrom ggplot2 geom_segment labs
#' @importFrom dplyr tibble select
#' @export
#' @examples
#' gamma_params(10, 5)
#' gamma_params(10, scale = 5)
#' gamma_params(mean = 10, sd = 3)
#' gamma_params(mode = 10, sd = 3, plot = TRUE)
#' gamma_params(mean = 1, sd = 10, plot = TRUE)

gamma_params <-
  function(shape = NULL,
           rate = NULL,
           scale = NULL,
           mean = NULL, sd = NULL,
           mode = NULL,
           plot = FALSE,
           ...) {

    Res <- NULL

    require_greater <- function(x, target) {
      if (!is.null(x) && x <= target) {
        stop(paste(
          deparse(substitute(x)),
          "must be greater than",
          target))
      }
    }
    require_greater(shape, 0)
    require_greater(rate, 0)
    require_greater(scale, 0)
    require_greater(mean, 0)
    require_greater(mode, 0)
    require_greater(sd, 0)

    if (!is.null(scale) && !is.null(rate)) {
      stop("Only one of scale and rate may be supplied.")
    }

    if (is.null(rate) && ! is.null(scale)) rate <- 1/scale

    if (!is.null(rate) & !is.null(shape)) {
      Res <-
        dplyr::tibble(
          shape = shape,
          rate  = rate,
          scale = 1/rate,
          mean  = shape / rate,
          mode  = (shape - 1) / rate,
          sd    = sqrt(shape) / rate
        )
    } else if (!is.null(mean) & !is.null(sd)) {
      Res <-
        dplyr::tibble(
          shape = mean^2 / sd^2,
          rate  = mean / sd^2,
          scale = 1/rate,
          mean  = mean,
          mode  = (shape - 1) / rate,
          sd    = sd
        )
    } else if (!is.null(mode) & !is.null(sd)) {
      Res <-
        dplyr::tibble(
          mode  = mode,
          sd    = sd,
          rate  = (mode + sqrt(mode^2 + 4 * sd^2)) / (2*sd^2),
          shape = 1 + mode * rate,
          scale = 1 / rate,
          mean  = shape / rate
        )
    } else if (!is.null(shape) & !is.null(sd)) {
      Res <-
        dplyr::tibble(
          shape = shape,
          sd    = sd,
          rate  = sqrt(shape) / sd,
          scale = 1/rate,
          mean  = shape / rate,
          mode  = (shape - 1) / rate
        )
    } else if (!is.null(shape) & !is.null(mean)) {
      Res <-
        dplyr::tibble(
          shape = shape,
          rate = shape / mean,
          scale = 1 / rate,
          mean  = shape / rate,
          mode  = (shape - 1) / rate,
          sd    = sqrt(shape) / rate
        )
    } else if (!is.null(shape) & !is.null(mode)) {
      Res <-
        dplyr::tibble(
          shape = shape,
          rate = (shape - 1) / mode,
          scale = 1 / rate,
          mean  = shape / rate,
          mode  = (shape - 1) / rate,
          sd    = sqrt(shape) / rate
        )
    } else {
      stop(paste(
        "Specify one of the following pairs: ",
        "shape & rate (or scale); mean & standard deviation; mode & standard deviation",
        sep = "\n\t"))
    }

    if (Res$mode < 0) {Res$mode = NA}
    if (plot) {
      f_mean <-
        0 + dgamma(Res$mean, shape = Res$shape, rate = Res$rate) ~
        Res$mean + Res$mean
      p <- ggformula::gf_dist("gamma", shape = Res$shape, rate = Res$rate, ...)
      if (! "kind" %in% names(list(...))) {
        p <- p +
          ggplot2::geom_segment(
            aes(x = Res$mean, xend = Res$mean,
                yend = dgamma(Res$mean, shape = Res$shape, rate = Res$rate),
                y = 0))
        if (!is.na(Res$mode)) {
          f_mode <-
            0 + dgamma(Res$mode, shape = Res$shape, rate = Res$rate) ~
            Res$mode + Res$mode
          p <- p +
            ggplot2::geom_segment(
              aes(x = Res$mode, xend = Res$mode,
                  yend = dgamma(Res$mode, shape = Res$shape, rate = Res$rate),
                  y = 0))
        }
      }
      p <- p +
        ggplot2::labs(
          title = paste("Gamma(",
                        format(Res$shape), ", ", format(Res$rate),
                        ")",
                        sep = "")
        )
      print(p)
    }
    return(Res %>% dplyr::select(shape, rate, scale, mode, mean, sd))
  }

