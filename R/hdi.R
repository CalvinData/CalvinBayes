
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
#' hdi_from_grid(Grid, "x", prob = 0.9)
#' hdi(mosaic::resample(Grid, prob = Grid$posterior, size = 10000)$x, prob = 0.9)
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
    res <- coda::HPDinterval(coda::as.mcmc(object), prob = prob)

    if (is.list(res)) {
      for (i in 1:length(res)) {
        res[[i]] <-
          convert_to_df(res[[i]], pars = pars, regex_pars = regex_pars) %>%
          mutate(chain = i)
      }
      bind_rows(res) %>%
        arrange(par, chain)
    } else {
      convert_to_df(res, pars = pars, regex_pars = regex_pars, prob = prob)
    }
  }

#' @export
hdi.data.frame <-
  function(object, prob = 0.95, pars = NULL, regex_pars = NULL, ...) {
    if (inherits(pars, "formula")) {
      # for y ~ x use y as name and x as expression to evaluate
      # for   ~ x use x as both name and expression to evaluate
      l <- length(pars)
      object[[deparse(pars[[2]])]] <- eval(pars[[l]], object, parent.frame())
      pars <- tail(names(object), 1)
    }
    object <- object[sapply(object, is.numeric)]
    hdi.default(object, prob = prob, pars = pars, regex_pars = regex_pars, ...)
  }

convert_to_df <- function(object, prob = 0.95, pars = NULL, regex_pars = NULL) {
  res <-
    data.frame(
      par = row.names(object),
      lo = object[, 1],
      hi = object[, 2],
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
    object$posterior <- object[[posterior]]

    dplyr::bind_rows(
      lapply(
        pars,
        function(p) {
          FOO <-
          object %>%
            # collapse to parameter of interest
            dplyr::group_by_at(vars(p)) %>%
            dplyr::summarise(posterior = sum(posterior)) %>%
            # standardize names
            setNames(c("theta", "posterior")) %>%
            # compute resolution (difference in parameter values in grid)
            # for use in converting form probability to density scale at the end
            mutate(resolution = mean(diff(sort(theta)))) %>%
            mutate(posterior = posterior / sum(posterior)) %>%
            arrange(posterior) %>%              # sort by posterior
            mutate(
              cum_posterior = cumsum(posterior)   # cumulative posterior
            ) %>%
            filter(
              cum_posterior >= 1 - prob,          # keep highest cum_posterior
            )
            FOO %>%
            summarise(                            # summarise what's left
              param = p,
              lo = min(theta),
              hi = max(theta),
              prob = sum(posterior),
              height = min(posterior) / first(resolution),       # density scale
              mode_height = last(posterior) / first(resolution), # density scale
              mode = last(theta),
            )
        }
      )
    )
  }
