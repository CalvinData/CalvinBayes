#' Shifted and scaled t distributions
#'
#' The functions are designed for compatibility with Stan's student t distribution
#' which has three parameters: degrees of freedom, mean, and standard deviation.
#'
#' @rdname student_t
#' @param n Number of random draws to perform.
#' @param df Degrees of freedom
#' @param mean Location parameter (equals mean when `df > 1`).
#' @param sigma Scale parameter.  Note: this is smaller than the standard deviation
#'   (when the standard deviation exists).
#' @param sd Standard deviation of distribution, but note that this only exists when
#'   `df > 2`.  If provided, `sd` supersedes `sigma`. A warning message is provided when
#'   `df <= 2` and `sd` is used.
#' @return A vector of random draws from the appropriate t distribution(s).
#' @export
#' @examples
#' rstudent_t(5, mean = 10, sigma = 2, df = 3)
#' rstudent_t(5, mean = 10, sd = 2, df = 3)
#' sd(rstudent_t(5000, mean = 10, sd = 2, df = 4))
#' # sigma = 2, sd = 2 * sqrt(4/2) approx 2.8
#' sd(rstudent_t(50000, mean = 10, sigma = 2, df = 4))

rstudent_t <- function(n, df, mean = 0, sigma = 1, sd = NULL) {
  if (!is.null(sd)) {
    suppressWarnings(
      sigma <- sqrt((df-2)/df) * sd
    )
    if (! all(df > 2)) {
      warning("sd is only defined when df > 2. NaNs returned when df <= 2.")
    }
  }
  raw <- rt(n, df = df)
  mean + raw * sigma
}
