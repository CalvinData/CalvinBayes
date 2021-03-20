
#' Extract Stan fit from wrapper object
#'
#' The default method with search a list and return an element that inherits from class "stanfit",
#' if one exists.
#'
#' @param object An object from which to extract a stanfit object.
#' @param ... Additional arguments (currently ignored).
#'
#' @export
stanfit <- function(object, ...) {
  UseMethod("stanfit")
}

#' @export
stanfit.ulam <- function(object, ...) {
  object@stanfit
}

#' @export
stanfit.stanfit <- function(object, ...) {
  object
}

#' @export
stanfit.default <- function(object, ...) {
  if (is.list(object)) {
    # search for and return first stanfit object in the list
    is_stanfit <- sapply(object, function(x) inherits(x, "stanfit"))
    if (any(is_stanfit)) {
      return(object[[which.max(is_stanfit)]])
    }
  }
  stop("I don't know how to extract a stanfit from that object")
}

#' @export
stanfit.brmsfit <- function(object, ...) {
  object$fit
}
