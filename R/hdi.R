
#' Compute highest density intervals
#'
#' Compute highest density intervals
#'
#' @rdname hdi
#' @export
hdi <- function(object, prob = 0.95, pars = NULL, regex_pars, ...) {
  UseMethod("hdi")
}

#' @param object an object describing a posterior distribution
#' @param prob the desired mass of the HDI region.
#' @param pars a vector of parameter names
#' @param regex_pars a regular expression for selecting parameter names
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
#'  hdi_from_grid(Grid, params = c("mean", "sd"))
#'

#' @rdname hdi
#' @importFrom coda as.mcmc HPDinterval
#' @export

hdi.default <-
  function(object, prob = 0.95, pars = NULL, regex_pars = NULL, ...) {
    res <- coda::HPDinterval(coda::as.mcmc(object), prob = 0.95)
    res <-
      data.frame(
        par = row.names(res),
        lo = res[, 1],
        hi = res[, 2],
        prob = prob
      )
    row.names(res) <- NULL

    if (!is.null(pars) && !is.null(regex_pars)) {
      return(res %>% filter(par %in% pars | grepl(regex_pars, par)))
    }

    if (!is.null(pars)) {
      return(res %>% filter(par %in% pars))
    }

    if (!is.null(regex_pars)) {
      return(res %>% filter(grepl(regex_pars, par)))
    }

    res
  }


#' @rdname hdi
#' @export
hdi_from_grid <-
  function(object, pars = NULL,
           prob = 0.95, posterior = "posterior") {
    if (is.null(pars)) {
      pars <- names(object)[1]
      warning("No parameters specified.  Using ", pars)
    }
    if (! posterior %in% names(object)) {
      stop(paste0("No column named ", posterior, " in object"))
    }

    dplyr::bind_rows(
      lapply(
        pars,
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
