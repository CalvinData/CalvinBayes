#' Example: Bayesian inference of a proportion
#'
#' This intended as a black-box example with a small data set to illustrate
#' the results Bayesian updating.
#'
#' @param data a vector of 0's and 1's or a character string with two characters representing
#'   success/failure or head/tails from the bernoulli experiment.
#' @param p a vector of probabilites to update.
#' @param prior a vector of prior values at each probability in `p` or a constant.
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
#' @param ... additional arguments passed to the geom.
#'
#' @importFrom dplyr tibble
#' @importFrom ggplot2 labs facet_wrap
#' @export
#' @examples
#' CoinExample("HHTTH", geom = geom_col, p = seq(0, 1, by = 0.25), results = "data")
#' CoinExample("HHTTH", geom = geom_col, p = seq(0, 1, by = 0.25), steps = TRUE, results = "data")
#' CoinExample("HHTTH", p = seq(0, 1, by = 0.25), alpha = 0.9, steps = TRUE)
#' CoinExample("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE)
#' CoinExample("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE)
#' CoinExample("HHHHHHHHHHHH", alpha = 0.9)
#' CoinExample("HHHHHHHHHHHH", alpha = 0.9, steps = TRUE)

CoinExample <-
  function(data = "HHTT",
           p = seq(0, 1, length.out = resolution + 1),
           prior = 1,
           results = c("plot", "data"),
           steps = FALSE,
           geom = geom_line,
           alpha = 0.7,
           resolution = 100,
           ...) {
    results = match.arg(results)
    probs = p  # force p to be evaluated in case it is determined by n

    if (is.factor(data)) {
      data <- 2 - as.numeric(factor(data))
      message(paste("Converting data to", paste(data, collapse = ", ")))
    }

    if (is.character(data)) {
      data <- stringr::str_split(data, pattern = "")[[1]]
      data <- 2 - as.numeric(factor(data))
      message(paste("Converting data to", paste(data, collapse = ", ")))
    }

    if (! all(data %in% c(0,1))) {
      stop("data should be all 0's and 1's")
    }

    if (steps) {
      Grid <-
        base::Reduce(c, data, accumulate = TRUE) %>%
        base::lapply(CoinExample, p = probs, results = "data", accumulate = FALSE) %>%
        dplyr::bind_rows()
    } else {
      Grid <- dplyr::tibble(p = probs) %>%
        dplyr::mutate(
          prior_paths = prior,
          prior = prior_paths / sum(prior_paths),
          likelihood0 = dbinom(sum(head(data, -1)), size = length(data)-1, prob = probs),
          likelihood = dbinom(sum(data), size = length(data), prob = probs),
          paths0 = prior_paths * likelihood0 * 4^(length(data) - 1),
          paths = prior_paths * likelihood * 4^length(data),
          posterior0 = paths0 / sum(paths0),
          posterior = paths / sum(paths),
          d = paste(data, collapse = ""),
          n = length(data)
        ) %>%
        select(p, prior, likelihood, posterior, d, n)
    }

    if (results == "data") return(Grid)

    res <-
      base::suppressWarnings(
        ggplot2::ggplot(Grid) +
          geom(aes(x = p, y = prior, color = "prior", fill = "prior"), size = 1.5, alpha = 0.7) +
          geom(aes(x = p, y = posterior, color = "posterior", fill = "posterior")) +
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
          ggplot2::labs(y = "", color = "", fill = "", title = paste(data, collapse = " "))
      )
    }
  }



