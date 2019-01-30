testthat::context("Checking zero_one conversions")

test_that("beta_params() calculates correctly.", {
  A <- dplyr::bind_rows(
    beta_params(4, 6),
    beta_params(shape1 = 4, shape2 = 6),
    beta_params(40, 60),
    beta_params(mean = 0.2, sd = 3),
    beta_params(mode = 0.2, concentration = 8)
  )
  expect_equivalent(A$mean, A$shape1 / (A$shape1 + A$shape2))
  expect_equivalent(A$mode, (A$shape1 - 1)  / (A$shape1 + A$shape2 - 2))
  expect_equivalent(
    A$sd,
    sqrt((A$shape1 * A$shape2) / ((A$shape1 + A$shape2)^2 * (A$shape1 + A$shape2 + 1)))
  )
  expect_equivalent(A$concentration, A$shape1 + A$shape2)
})




