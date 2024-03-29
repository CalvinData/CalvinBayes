% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metropolis.R
\name{metro_bern}
\alias{metro_bern}
\alias{metro_norm}
\title{Metropolis algorithm}
\usage{
metro_bern(
  x,
  n,
  step_size = 0.01,
  start = 0.5,
  num_steps = 10000,
  prior = dunif,
  ...
)

metro_norm(
  y,
  num_steps = 1e+05,
  step_size = 1,
  start = list(mu = 0, log_sigma = 0)
)
}
\arguments{
\item{x}{number of success in data}

\item{n}{number of failures in data}

\item{step_size}{sd of (normal) jump distribution(s)}

\item{start}{starting value(s) for MCMC}

\item{num_steps}{how long to run MCMC}

\item{prior}{a function describing the prior}

\item{...}{additional arguments for the prior}

\item{y}{vecctor of numeric response values}
}
\description{
Simple Metropolis algorithm implementations for simple models.
}
\examples{
Metro <-
  metro_bern(10, 30, step_size = 0.1, prior = dbeta, shape1 = 4, shape2 = 4)
# posterior density
Metro \%>\%
  gf_dens(~ theta) \%>\%
  gf_dist("beta", shape1 = 14, shape2 = 24, color = "red")
# trace plot
Metro \%>\%
  gf_line(theta ~ step)

metro_norm(rnorm(25, 10, 1), start = list(mu = 5, log_sigma = log(5))) \%>\%
  gf_density( ~ mu)

}
