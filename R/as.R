
#' Convert objects to another type of object
#'
#' Convert objects to another type of object
#'
#' @rdname as
#' @param x an object to convert
#' @param ... additional arguments
#' @export
as.array.rjags <- function(x, ...) {
  x$BUGSoutput$sims.array
}

#' @rdname as
#' @export
as.matrix.rjags <- function(x, ...) {
  x$BUGSoutput$sims.matrix
}
