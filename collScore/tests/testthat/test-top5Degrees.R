context("top5Degrees")

testthat::test_that("top5Degrees returns the expected objects", {
  result <- top5Degrees(,scorecard13,c("Stanford University","University of Southern California"),)
  expect_is(result, "data.frame")
})

