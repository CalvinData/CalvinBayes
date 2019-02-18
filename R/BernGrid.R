#' Bayesian inference for a proportion via grid method
#'
#' This intended as a black-box example with a small data set to illustrate
#' the results of Bayesian updating.  The model is a Bernoulli model with
#' fixed proportion over all trials.  Prior, data, and grid resolution
#' can be supplied by the user. See examples.
#'
#' @param data a vector of 0's and 1's or a character string with two characters representing
#'   success/failure or head/tails from the bernoulli experiment.
#' @param p a vector of probabilites to update.
#' @param prior a vector of prior values at each probability in `p`,
#'    a constant, a function, or a one-sided formula.
#'    If a function or formula, [purrr::map_dbl()] is used to
#'    apply to each value of `p`.  (See help for [purrr::map_dbl()]
#'    for details regarding how this works.)
#' @param results either `"plot"` or `"data"` to indicate what information is returned.
#' @param steps a logical indicating wether the process should be shown step-by-step or
#'   for all the data at once.
#' @param geom a geom used for plotting.  Good choices include
#'   [ggplot2::geom_line()],
#'   [ggplot2::geom_col()],
#'   [ggplot2::geom_point()],
#'   [ggplot2::geom_area()]
#' @param alpha opacity used for the plot
#' @param resolution if `p` is not specified, `resolution` an be used to create an equally
#'   spaced grid of `resolution + 1` values from 0 to 1.
#' @param filter an expression used to filter the results. See examples.
#' @param ... additional arguments passed to the geom.
#'
#' @importFrom dplyr tibble filter
#' @importFrom ggplot2 labs facet_wrap
#' @importFrom purrr map_dbl
#' @export
#' @examples
#' BernGrid("HHTTH", p = seq(0, 1, by = 0.25), results = "data")
#' BernGrid("HHTTH",
#'             prior = ~ dbeta(.x, shape1 = 2, shape2 = 5))
#' BernGrid("HHTTH", geom = geom_col, resolution = 100,
#'             prior = ~ dbeta(.x, shape1 = 2, shape2 = 5))
#' BernGrid("HHTTH", geom = geom_area, resolution = 100,
#'             prior = ~ dbeta(.x, shape1 = 2, shape2 = 5))
#' BernGrid("HHTTH",
#'   p = seq(0, 1, by = 0.25), prior = c(.1, .2 , .4, .2, .1))
#' BernGrid("SSFFS", p = seq(0, 1, by = 0.25), steps = TRUE, results = "data")
#' BernGrid("SSFFS", p = seq(0, 1, by = 0.25), alpha = 0.9, steps = TRUE)
#' BernGrid("SSFFS", p = seq(0, 1, by = 0.25), alpha = 0.5, steps = TRUE,
#'   filter = n %in% c(4,5))
#' BernGrid("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE)
#' BernGrid("HHTTHTHHTHHH", steps = TRUE, geom = geom_area)
#' BernGrid("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE, filter = n %in% c(1, 5, 10))
#' BernGrid("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE, filter = n == 10)
#' # same posterior as in the previous example -- order doesn't matter
#' BernGrid("TTTTHHHHHHHH", alpha = 0.9, steps = TRUE, filter = n == 10)
#' BernGrid("HHHHHHHHHHHH", alpha = 0.9)
#' BernGrid("HHHHHHHHHHHH", alpha = 0.9, steps = TRUE)
#' BernGrid(rbinom(12, 1, 0.25), alpha = 0.9, steps = TRUE)

BernGrid <-
  function(data = "HHTT",
           p = seq(0, 1, length.out = resolution + 1),
           prior = 1,
           results = c("plot", "data"),
           steps = FALSE,
           geom = if (length(p) <= 20) geom_col else geom_area,
           alpha = 0.5,
           resolution = 200,
           filter = TRUE,
           ...) {
    results <- match.arg(results)
    probs <- p  # force p to be evaluated
    if (is.function(prior) || inherits(prior, "formula")) {
      prior <- purrr::map_dbl(probs, prior)
    }

    filter <- enquo(filter)

    converted <- FALSE

    if (is.factor(data)) {
      data <- as.numeric(data) - 1
      converted <- TRUE
    }

    if (is.character(data)) {
      data <- stringr::str_split(data, pattern = "")[[1]]
      if (all(data %in% c("H", "T"))) {
        # make sure H = 1 and T = 0
        data <- 2 - as.numeric(factor(data))
      } else {
        # else use alphabetical order
        # so S = 1 and F = 0, for example
        data <- as.numeric(factor(data)) - 1
      }
      converted <- TRUE
    }

    if (converted) {
      if (length(data) <= 20) {
        message(paste("Converting data to", paste(data, collapse = ", ")))
      } else {
        message("Converting data to 0s and 1s")
      }
    }

    if (! all(data %in% c(0,1))) {
      stop("data should be all 0's and 1's")
    }

    if (steps) {
      Grid <-
        base::Reduce(c, data, accumulate = TRUE) %>%
        base::lapply(BernGrid, p = probs, results = "data", accumulate = TRUE) %>%
        dplyr::bind_rows() %>%
        dplyr::filter( !! filter) %>%
        group_by(p) %>%
        arrange(p, n) %>%
        dplyr::mutate(
          prior = ifelse(is.na(lag(posterior)), prior, lag(posterior))
        ) %>%
        ungroup() %>%
        arrange(n, p)
    } else {
      Grid <- dplyr::tibble(p = probs) %>%
        dplyr::mutate(
          # prior_paths = prior,
          prior0 = prior,  # used to make sure prior is replicated to correct length when a constant
          prior = prior0 / sum(prior0),
          likelihood0 = dbinom(sum(head(data, -1)), size = length(data)-1, prob = probs),
          likelihood = dbinom(sum(data), size = length(data), prob = probs),
          # paths0 = prior_paths * likelihood0 * 4^(length(data) - 1),
          # paths = prior_paths * likelihood * 4^length(data),
          # posterior0 = paths0 / sum(paths0),
          # posterior = paths / sum(paths),
          posterior0 = prior * likelihood,
          posterior = posterior0 / sum(posterior0),
          d = paste(data, collapse = ""),
          n = length(data)
        ) %>%
        select(p, prior, likelihood, posterior, d, n)
    }

    if (results == "data") return(Grid)

    res <-
      base::suppressWarnings(
        ggplot2::ggplot(Grid) +
          geom(aes(x = p, y = prior, color = "prior", fill = "prior"),
               alpha = alpha, ...) +
          geom(aes(x = p, y = posterior, color = "posterior", fill = "posterior"),
               alpha = alpha, ...) +
          ggplot2::scale_color_manual(values = c("posterior" = "steelblue", "prior" = "gray60")) +
          ggplot2::scale_fill_manual(values = c("posterior" = "steelblue", "prior" = "gray60"))
      )

    if (steps) {
      return(
        res +
          ggplot2::labs(y = "", color = "", fill = "") +
          ggplot2::facet_wrap(~d)
      )
    } else {
      return(
        res +
          ggplot2::labs(y = "", color = "", fill = "",
                        title =
                          if (length(data) <= 16)
                            paste(data, collapse = " ")
                          else
                            paste(
                              length(data) - sum(data), " + " ,
                              sum(data), " = " ,
                              length(data))
                        )

      )
    }
  }

#' @rdname BernGrid
#' @export
bern_grid <- BernGrid

