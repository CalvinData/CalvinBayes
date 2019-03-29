#' Shifted and scaled t distributions
#'
#' The functions are designed for compatibility with Stan's student t distribution
#' which has three parameters: degrees of freedom, mean, and standard deviation.
#'
#' @rdname student_t
#' @param n Number of random draws to perform.
#' @param df,mean,sd Degrees of freedom, mean, and standard deviation of distribution
#' @return A vector of random draws from the appropriate t distribution(s).
#' @export
rstudent_t <- function(n, df, mean = 0, sd = 1) {
  raw <- rt(n, df = df)
  mean + raw * sqrt((df-2)/df) * sd
}
