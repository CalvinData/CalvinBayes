
#' Quick determination of posterior from a beta prior and Bernoulli likelihood
#'
#' Quick determination of posterior from a beta prior and Bernoulli likelihood
#'
#'
#' @param x number of successes in data
#' @param n number of trials in data
#' @param ... prameters describing the beta prior.  Uses [beta_params()].
#' @examples
#' quick_bern_beta(x = 4, n = 5, a = 2, b = 4)
#' quick_bern_beta(x = 4, n = 5, mode = 0.3, concentration = 10)
#' @export
quick_bern_beta <-
  function(
    x, n,     # data, successes and trials
    ...       # see clever trick below
  )
  {
    pars <- beta_params(...)
    a <- pars$shape1
    b <- pars$shape2

    theta_hat <- x / n  # value that makes likelihood largest
    posterior_mode <- (a + x - 1) / (a + b + n - 2)

    # scale likelihood to be as tall as the posterior
    likelihood <- function(theta) {
      dbinom(x, n, theta) / dbinom(x, n, theta_hat) *
        dbeta(posterior_mode, a + x, b + n - x)  # posterior height at mode
    }

    gf_dist("beta", shape1 = a, shape2 = b,
            color = ~ "prior", alpha = 0.5, xlim = c(0,1), size = 1.2) %>%
      gf_function(likelihood,
                  color = ~ "likelihood", alpha = 0.5, size = 1.2) %>%
      gf_dist("beta", shape1 = a + x, shape2 = b + n - x,
              color = ~ "posterior", alpha = 0.5, size = 1.6) %>%
      gf_labs(
        color = "function",
        title = paste0("posterior: Beta(", a + x, ", ", b + n - x, ")")
      ) %>%
      gf_refine(
        scale_color_manual(
          values = c("prior" = "gray50", "likelihood" = "forestgreen",
                     "posterior" = "steelblue")))
  }
