
#' Compute highest density intervals
#'
#' Compute highest density intervals
#' @rdname hdi
#' @export
hdi <- function(object, ...) {
  UseMethod("hdi")
}

#' @param object an object describing a posterior distribution
#' @param prob the desired mass of the HDI region.
#' @param params a vector of parameter names
#' @return a data frame with 1 row per paramter and variables
#'   * `lo` lower end of hdi
#'   * `hi` higher end of hdi
#'   * `prob` is the total probability
#' @examples
#' # Determining HDI of a beta(30,12) distribution
#' # Create a grid
#' Grid <-
#'   expand.grid(x = seq(0, 1, length.out = 201)) %>%
#'     mutate(posterior = dbeta(x, 30, 12))
#' hdi(Grid, "x", prob = 0.9)
#' x <- rnorm(25, 3, 2)  # some random data
#' Grid <-
#'   expand.grid(
#'     mean = seq(0, 10, length.out = 101),
#'     sd   = seq(0, 10, length.out = 101)
#'     ) %>%
#'     mutate(
#'       prior = 1,
#'       loglik =
#'         purrr::map2_dbl(mean, sd, function(a, b) sum(dnorm(x, a, b, log = TRUE)), x = x),
#'       likelihood = exp(loglik),
#'       posterior = prior * likelihood
#'     )
#'  hdi(Grid, params = c("mean", "sd"))
#'

#' Assume data frame is posterior samples
#' @rdname hdi
#' @export
hdi.data.frame <-
  function(object, params = NULL,
           level = 0.95, posterior = "posterior") {
    if (is.null(params)) {
      params <- names(object)[1]
      warning("No parameters specified.  Using ", params)
    }
    if (! posterior %in% names(object)) {
      stop(paste0("No column named ", posterior, " in object"))
    }

    dplyr::bind_rows(
      lapply(
        params,
        function(p) {
          FOO <-
          object %>%
            dplyr::group_by_at(vars(p)) %>%       # collapse to parameter of interest
            dplyr::summarise(posterior = sum(posterior)) %>%  # normalize
            setNames(c("theta", "posterior")) %>% # standardize names
            arrange(posterior) %>%                # sort by posterior
            mutate(
              cum_posterior = cumsum(posterior)   # cumulative posterior
            ) %>%
            filter(
              cum_posterior >= 1 - level,         # keep highest cum_posterior
            )
            FOO %>%
            summarise(                            # summarise what's left
              param = p,
              lo = min(theta),
              hi = max(theta),
              prob = sum(posterior),
              height = min(posterior),
              mode_height = first(posterior),
              mode = first(theta),
            )
        }
      )
    )
  }
