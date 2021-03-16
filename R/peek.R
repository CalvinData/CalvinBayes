#' Perform side effect
#'
#' Perform side effect
#' @param object An R object
#' @param fun a function that can be applied to `object`
#' @param ... additional arguments to `fun`
#' @examples
#'
#' if (require(dplyr)) {
#'   iris %>%
#'   peek(slice_sample, n = 3) %>%
#'   group_by(Species) %>%
#'   summarise(n = n())
#' }
#'
#' @export

peek <- function(object, fun = function(x, ...) x, ...) {
  print(fun(object, ...))
  object
}
