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
#' @importFrom dplyr tibble filter
#' @importFrom ggplot2 labs facet_wrap
#' @export
#' @examples
#' CoinExample("HHTTH", geom = geom_col, p = seq(0, 1, by = 0.25), results = "data")
#' CoinExample("HHTTH",
#'   p = seq(0, 1, by = 0.25), prior = c(.1, .2 , .4, .2, .1),
#'   geom = geom_col, alpha = 0.5)
#' CoinExample("HHTTH", geom = geom_col, p = seq(0, 1, by = 0.25), steps = TRUE, results = "data")
#' CoinExample("HHTTH", p = seq(0, 1, by = 0.25), alpha = 0.9, steps = TRUE)
#' CoinExample("HHTTH", p = seq(0, 1, by = 0.25), alpha = 0.5, steps = TRUE,
#'   geom = geom_col, filter = n %in% c(4,5))
#' CoinExample("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE)
#' CoinExample("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE)
#' CoinExample("HHTTHTHHTHHH", alpha = 0.9, steps = TRUE, filter = n %in% c(1, 5, 10))
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
           filter = TRUE,
           ...) {
    results = match.arg(results)
    probs = p  # force p to be evaluated in case it is determined by n
    filter <- enquo(filter)

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
        base::lapply(CoinExample, p = probs, results = "data", accumulate = TRUE) %>%
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
          ggplot2::labs(y = "", color = "", fill = "", title = paste(data, collapse = " "))
      )
    }
  }



