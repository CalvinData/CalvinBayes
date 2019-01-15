
#' Convert vector to 0's and 1's
#'
#' JAGS and Stan generally require the dichotymous data be coded as 0's and 1's. This
#' function converts other data fromats to 0's and 1's.
#'
#' @param x a vector
#' @param one A vector of values to be coded as 1.  All other values will be coded as 0.
#'   If `NULL`, a defult choice is made.
#' @export
#' @examples
#' zero_one(iris$Species)

zero_one <- function(x, one = NULL) {
  if (is.null(one)) {
    if (is.logical(x)) {
      one <- TRUE
    } else {
      one <- head(sort(unique(x)), 1)
    }
  }
  w <- which(x %in% one)
  res <- rep(0, length(x))
  res[w] <- 1
  res
}
