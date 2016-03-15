context("completionRate")

testthat::test_that("completionRate returns the expected objects with race parameter", {
  data(scorecard13)
  result <- completionRate(, scorecard13,c("University of Chicago","Northwestern University"),"race")
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})

testthat::test_that("completionRate returns the expected objects without race parameter", {
  data(scorecard13)
  result <- completionRate(, scorecard13,c("University of Chicago","Northwestern University"),"")
  expect_is(result, "gg")
  expect_is(result, "ggplot")
})