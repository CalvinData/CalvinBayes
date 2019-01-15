testthat::context("Checking zero_one conversions")

test_that("one used correctly", {
  expect_equivalent(
    zero_one(iris$Species, one = "versicolor"),
    rep(c(0, 1, 0), each = 50)
  )
  expect_equivalent(
    zero_one(iris$Species, one = c("setosa", "versicolor")),
    rep(c(1, 1, 0), each = 50)
  )
})


test_that("defaults working as desired", {
  expect_equivalent(
    zero_one(iris$Species),
    rep(c(1, 0, 0), each = 50)
  )
  expect_equivalent(
    zero_one(c(TRUE, FALSE, FALSE, TRUE)),
    c(1, 0, 0, 1)
  )
})



