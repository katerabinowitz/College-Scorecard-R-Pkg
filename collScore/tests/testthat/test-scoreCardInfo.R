context("scoreCardInfo")

testthat::test_that("scoreCardInfo returns the expected objects", {
  result <- scorecardInfo(c("CITY","INSTNM","UNITID"))
  expect_is(result, "data.frame")
})