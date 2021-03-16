#' ACF Plots
#'
#' A wrapper around `stats::acf()` to make easy ACF plots in the ggplot2 system.
#'
#' @inheritParams stats::acf
#' @importFrom stats acf qnorm
#' @importFrom broom tidy
#' @param fill fill color for ACF plot
#' @param alpha opacity of fill for ACF plot
#' @param color color of reference lines (use "transparent" to hide)
#' @param plot A logical indicating whether the original base plot graphic should be displayed.
#'   Generally this should be left to its default value of `FALSE`.
#'
#' @seealso `stats::acf()`
#'
#' @export
#' @examples
#'
#' acf_plot(rnorm(100))
#' acf_plot(rnorm(100), lag.max = 100, fill = "red", alpha = 0.3, color = "forestgreen")
#'

acf_plot <-
  function(x, lag.max = NULL,
           type = c("correlation", "covariance", "partial"),
           plot = FALSE,
           na.action = na.fail,
           demean = TRUE,
           ...,
           color = "steelblue",
           fill = "black",
           alpha = 0.7,
           data = NULL) {
    if (!is.null(data)) {
      x <- eval(substitute(x), data, parent.frame())
    }
    acf_out <- stats::acf(x, lag.max = lag.max, type = type, plot = plot, na.action = na.action, demean = demean, ...)
    ACF <- acf_out %>% broom::tidy()
    n <- acf_out$n.used
    fence <- stats::qnorm((1 - 0.95)/2) / sqrt(n)
    fence <- c(-1, 1) * fence
    ggplot() +
      geom_col(aes(x = lag, y = acf), data = ACF, fill = fill, alpha = alpha) +
      geom_hline(yintercept = fence, linetype = "dashed", color = color)

}
