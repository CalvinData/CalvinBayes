
#' Metropolis algorithm
#'
#' Simple Metropolis algorithm implementations for simple models.
#'
#' @rdname metropolis
#' @param x number of success in data
#' @param n number of failures in data
#' @param size sd of (normal) jump distribution(s)
#' @param start starting value(s) for MCMC
#' @param num_steps how long to run MCMC
#' @param prior a function describing the prior
#' @param ... additional arguments for the prior
#'
#' @examples
#' Metro <-
#'   metro_bern(10, 30, size = 0.1, prior = dbeta, shape1 = 4, shape2 = 4)
#' # posterior density
#' Metro %>%
#'   gf_dens(~ theta) %>%
#'   gf_dist("beta", shape1 = 14, shape2 = 24, color = "red")
#' # trace plot
#' Metro %>%
#'   gf_line(theta ~ step)
#'
#' @export

metro_bern <- function(
  x, n,            # x = successes, n = trials
  size = 0.01,     # sd of jump distribution
  start = 0.5,     # value of theta to start at
  num_steps = 1e4, # number of steps to run the algorithm
  prior = dunif,   # function describing prior
  ...              # additional arguments for prior
) {

  theta             <- rep(NA, num_steps)  # trick to pre-alocate memory
  proposed_theta    <- rep(NA, num_steps)  # trick to pre-alocate memory
  move              <- rep(NA, num_steps)  # trick to pre-alocate memory
  theta[1]          <- start

  for (i in 1:(num_steps-1)) {
    # head to new "island"
    proposed_theta[i + 1] <- rnorm(1, theta[i], size)

    if (proposed_theta[i + 1] <= 0 ||
        proposed_theta[i + 1] >= 1) {
      prob_move          <- 0    # because prior is 0
    } else {
      current_prior       <- prior(theta[i], ...)
      current_likelihood  <- dbinom(x, n, theta[i])
      current_posterior   <- current_prior * current_likelihood
      proposed_prior      <- prior(proposed_theta[i+1], ...)
      proposed_likelihood <- dbinom(x, n, proposed_theta[i+1])
      proposed_posterior  <- proposed_prior * proposed_likelihood
      prob_move           <- proposed_posterior / current_posterior
    }


    # sometimes we "sail back"
    if (runif(1) > prob_move) { # sail back
      move[i + 1] <- FALSE
      theta[i + 1] <- theta[i]
    } else {                    # stay
      move[i + 1] <- TRUE
      theta[i + 1] <- proposed_theta[i + 1]
    }
  }

  tibble(
    step = 1:num_steps,
    theta = theta,
    proposed_theta = proposed_theta,
    move = move, size = size
  )
}

#' @rdname metropolis
#' @examples
#' metro_norm(rnorm(25, 10, 1), start = list(mu = 5, log_sigma = log(5))) %>%
#'   gf_density( ~ mu)
#'
#' @export
#'
metro_norm <- function(
  y,
  num_steps = 1e5,
  size = 1,         # sd's of jump distributions
  start = list(mu = 0, log_sigma = 0)
) {

  size <- rep(size, 2)[1:2]        # make sure exactly two values
  mu        <- rep(NA, num_steps)  # trick to pre-alocate memory
  log_sigma <- rep(NA, num_steps)  # trick to pre-alocate memory
  move      <- rep(NA, num_steps)  # trick to pre-alocate memory
  mu[1] <- start$mu
  log_sigma[1] <- start$log_sigma
  move[1] <- TRUE

  for (i in 1:(num_steps - 1)) {
    # head to new "island"
    mu[i + 1]        <- rnorm(1, mu[i], size[1])
    log_sigma[i + 1] <- rnorm(1, log_sigma[i], size[2])
    move[i + 1] <- TRUE

    log_post_current <-
      dnorm(mu[i], 0, 1, log = TRUE) +
      dnorm(log_sigma[i], 0, 1, log = TRUE) +
      sum(dnorm(y, mu[i], exp(log_sigma[i]), log = TRUE))
    log_post_proposal <-
      dnorm(mu[i + 1], 0, 1, log = TRUE) +
      dnorm(log_sigma[i + 1], 0, 1, log = TRUE) +
      sum(dnorm(y, mu[i + 1], exp(log_sigma[i+1]), log = TRUE))
    prob_move <- exp(log_post_proposal - log_post_current)

    # sometimes we "sail back"
    if (runif(1) > prob_move) {
      move[i + 1] <- FALSE
      mu[i + 1] <- mu[i]
      log_sigma[i + 1] <- log_sigma[i]
    }

  }
  tibble(
    step = 1:num_steps,
    mu = mu,
    log_sigma = log_sigma,
    move = move,
    size = paste(size, collapse = ", ")
  )
}
